library(tidyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(lubridate)
library(forecast)
library(zoo)
library("depmixS4")

get_data <- function() {
  # Get the data from the data set
  data <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE)
  
  # Convert the column Date to Date objects (so that we can extract)
  data$Date = as.POSIXlt(data$Date, format = "%d/%m/%Y")
  
  # Extract data from all complete weeks
  df <- data[data$Date >= as.POSIXlt("2007-01-01") & data$Date <= as.POSIXlt("2007-12-30"),]
  
  # Parse column 2 into separate columns
  dfsplit <- strsplit(df[,2], ",") # split the data
  df2 <- data.frame(dfsplit) # Put the data into a df
  df2 <- as.data.frame(t(df2)) #transpose to turn rows into cols
  rm(dfsplit)
  
  # Reindexing with help from James Comment: 
  # https://stackoverflow.com/questions/7567790/change-the-index-number-of-a-dataframe
  rownames(df2) <- 1:nrow(df2)
  df2 <- subset(df2, select = -V1) # Drop all columns we dont need
  df2$V2 <- substr(df2$V2, 2, 9) # Drop the quotations from "time" column
  
  # Rename the column names
  colnames(df2) <- c("Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  # Merge Date and Time into one column
  df$Date <- as.POSIXlt(paste(df$Date, df2$Time), format = "%Y-%m-%d %H:%M:%S")
  
  # Drop the columns that we don't want anymore
  df <- subset (df, select = Date)
  df$Time <- df2$Time
  df2 <- subset (df2, select = -Time)

  # Merge df and df2 and deselect all we dont need
  df <- cbind(df, df2)
  
  # Convert from char to numeric values
  df[,3:9] <- sapply(df[,3:9], as.numeric)
  
  return (df)
}
average_week <- function(data, colname = "Global_intensity") {
  df <- subset(data, select = c(Date, Time))
  df2 <- subset(data, select = c(colname))
  df <- cbind(df, df2)
  
  df$Day = weekdays(as.Date(df$Date))
  
  # n weeks, ms samples and msn number of samples returned for the week
  n = 52
  nw = 1
  msn = 10080
  ms = 10
  df_ma <- data.frame(Date = data$Date, 
                      Num_week=strftime(data$Date, format = "%V"), 
                      Day = strftime(data$Date, format = "%A"),
                      Time = strftime(data$Date, format = "%T"),
                      Moving_average=data[colname])
  
  df_ma$Num_week <- sub("^0", "", df_ma$Num_week)
  
  # breakdown the average calculations per week 
  df_ma$Moving_average <- ma(data[colname], ms, centre = TRUE)
  
  # create average week
  average_week <- df_ma %>% group_by(Day, Time)
  average_week <- average_week %>% summarise(
    Moving_average = mean(Moving_average, na.rm = TRUE)
  )
  
  # manually reorder average_week
  order <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")
  average_week = average_week %>%
    mutate(Day =  factor(Day, levels = order)) %>%
    arrange(Day)
  
  average_week$Date <- df[1:10080, "Date"]
  
  return (average_week)
}
train_hmm <- function(data, nstates, ntimes, feature) {
  set.seed(8)
  model <- depmix(response = get(feature) ~ 1, data = data, nstates = nstates, ntimes = ntimes)
  hmm <- fit(model)
  return (hmm)
}

# main
setwd("/Users/MichaelKuby/Documents/GitHub/CMPT318_CyberSecurity/CMPT318_Assignment_2/Group Assignment 2")
df <- get_data()

#scale the data
df_scaled <- as.data.frame(scale(df[3:9]))
df_scaled$Date <- df$Date
df_scaled$Time <- df$Time

# compute the average week for a given feature (column)
feature_of_interest <- "Global_intensity"
average_week <- average_week(df_scaled, feature_of_interest)
average_week$Date <- as.POSIXct(average_week$Date)

# Select a day (Choose Tuesday)
day <- "Tuesday"
average_tuesday <- average_week[average_week$Day == day,]

# Plot the average chosen day
ggplot(data = average_tuesday) +
  geom_point(mapping = aes(x = Date, y = Moving_average, color = "Feature of Interest")) +
  labs( title = "Smoothened Global Intensity vs. Time") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("Time") +
  ylab("Global Intensity (Amperes)") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")

# Choose a time window
start <- 5
end <- 9
tuesday <- 2

average_tuesday_window <- subset(average_tuesday, hour(average_tuesday$Date) >= start & hour(average_tuesday$Date) < end & wday(average_tuesday$Date, week_start=1) == tuesday)

# Plot the average Tuesday from start to end
ggplot(data = average_tuesday_window) +
  geom_point(mapping = aes(x = Date, y = Moving_average, color = "Feature of Interest")) +
  labs( title = "Tuesday Average Global Intensity vs. Time") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("Time") +
  ylab("Global Intensity (Amperes)") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")

# Extract the same time window for each week of the dataset and concatenate the extracted
# time windows to build a dataset for the training of HMMs.
n_times <- subset(df, hour(df$Date) >= start & hour(df$Date) < end & wday(df$Date, week_start=1) == tuesday)
n_times <- subset(n_times, select = c("Date", "Time", feature_of_interest))

# Train HMM's
weeks = 52
ntimes = rep(nrow(n_times)/weeks, weeks)

hmm4 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 4, ntimes = ntimes, feature = feature_of_interest)
hmm6 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 6, ntimes = ntimes, feature = feature_of_interest)
hmm8 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 8, ntimes = ntimes, feature = feature_of_interest)
hmm10 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 10, ntimes = ntimes, feature = feature_of_interest)
hmm12 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 12, ntimes = ntimes, feature = feature_of_interest)
hmm14 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 14, ntimes = ntimes, feature = feature_of_interest)
hmm16 <- train_hmm(data = subset(n_times, select = -c(Time)), nstates = 16, ntimes = ntimes, feature = feature_of_interest)

num_states <- c(4, 6, 8, 10, 14)
bic <- c(BIC(hmm4), BIC(hmm6), BIC(hmm8),BIC(hmm10),BIC(hmm14))
logl <- c(logLik(hmm4), logLik(hmm6), logLik(hmm8),logLik(hmm10),logLik(hmm14))

bic_logl_df <- data.frame(num_states = num_states, BIC = bic, logl = logl)

ggplot(data = bic_logl_df) +
  geom_line(mapping = aes(x = num_states, y = BIC, color = "BIC Value")) +
  geom_line(mapping = aes(x = num_states, y = logl, colour = "Log Likelihood")) +
  labs(title = "BIC Value and Log Likelihood of various HMM models vs. Number of States") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("States") +
  ylab("Value")

summary(hmm14)
print(hmm14)
