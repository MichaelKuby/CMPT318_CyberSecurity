library(tidyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(lubridate)
library(forecast)
library(zoo)
library("depmixS4")

load_data <- function() {
  # Get the data from the data set
  data <- read.table("TermProjectData.txt", header = TRUE)
  
  # Convert the column Date to Date objects (so that we can extract)
  data$Date = as.POSIXlt(data$Date, format = "%d/%m/%Y")
  
  # Extract data from all complete weeks
  df <- data[data$Date >= as.POSIXlt("2006-12-11") & data$Date <= as.POSIXlt("2009-11-30"),]
  
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
perform_PCA <- function(df) {
  df = na.omit(df)
  df.pca <- prcomp(df[, 3:9], center = TRUE, scale. = TRUE, retx = TRUE)
  summary(df.pca)
  head(df.pca$x)
  # How many components should we use?
  return (df.pca)
}
split_data <- function(data) {
  set.seed(123)
  sample <- sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data))))
  return (sample)
}
average_week <- function(data, colname = "Global_intensity") {
  df <- subset(data, select = c(Date, Time))
  df2 <- subset(data, select = c(colname))
  df <- cbind(df, df2)
  rm(df2)
  
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
    mutate(Day = factor(Day, levels = order)) %>%
    arrange(Day)
  
  average_week <- na.omit(average_week)
  average_week$Date <- df[1:10080, "Date"]
  
  return (average_week)
}
train_multivariate_hmm <- function(data, nstates, ntimes, features, family) {
  set.seed(50)
  model <- depmix(features, data = data, nstates = nstates, ntimes = ntimes, family=family)
  hmm <- fit(model)
  return (hmm)
}
test_multivariate_hmm <- function(data, ntimes, features, family, chosen_hmm) {
  nstates = chosen_hmm@nstates
  test_hmm <- depmix(response = features, data = subset(data, select = -c(Time)), nstates = nstates, ntimes = ntimes, family=family)
  test_hmm <- setpars(test_hmm, getpars(chosen_hmm))
  return(test_hmm)
}

# main
setwd("/Users/MichaelKuby/Documents/GitHub/CMPT318_CyberSecurity/Term Project")
df <- load_data()

# Use PCA to train HMM?
use_pca <- FALSE

# feature engineering
df.pca <- perform_PCA(df)

# Select the features we wish to use
if (use_pca) {
  # Use PC1, PC2, PC3 to explain 68% of the variation in the data
  data = df.pca$x
  data = subset(data, select = c(PC1, PC2, PC3))
  data <- as.data.frame(data)  
  
  # add in the day and time
  df2 <- na.omit(df)
  data$Date <- df2$Date
  data$Time <- df2$Time
  rm(df2)
} else {
  # Choose 2 or 3 features
  data <- as.data.frame(scale(df[3:9]))
  data$Date <- df$Date
  data$Time <- df$Time
  data <- subset(data, select = c(Date, Time, Global_active_power, Voltage, Sub_metering_2))
  data <- na.omit(data)
}

# compute the average week from 2007 for a given feature (column)
if (use_pca) {
  feature_1 <- "PC1"
} else {
  feature_1 <- "Sub_metering_2"
}

data_07 <- data[data$Date >= as.POSIXlt("2007-01-01") & data$Date <= as.POSIXlt("2007-12-30"),]
average_week_PC1 <- average_week(data_07, feature_1)
average_week_PC1$Date <- as.POSIXct(average_week_PC1$Date)  

# Select a day (Choose Tuesday)
day <- "Tuesday"
average_tuesday <- average_week_PC1[average_week_PC1$Day == day,]

if (use_pca) {
  # Plot the average chosen day
  ggplot(data = average_tuesday) +
    geom_point(mapping = aes(x = Date, y = Moving_average, color = "Smoothened PC1")) +
    labs( title = "Smoothened PC1 vs. Time") +
    guides(color = guide_legend(title = "Colour Guide")) +
    xlab("Time") +
    ylab("PC1 (Linear Combination") +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")
} else {
  # Plot the average chosen day
  ggplot(data = average_tuesday) +
    geom_point(mapping = aes(x = Date, y = Moving_average, color = "Smoothened Sub_metering_2")) +
    labs( title = "Smoothened Sub_metering_2 vs. Time") +
    guides(color = guide_legend(title = "Colour Guide")) +
    xlab("Time") +
    ylab("Sub_metering_2") +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")
}

# Choose a time window
start <- 3
end <- 6
tuesday <- 2

average_tuesday_window <- subset(average_tuesday, hour(average_tuesday$Date) >= start & hour(average_tuesday$Date) < end & wday(average_tuesday$Date, week_start=1) == tuesday)

if (use_pca) {
  # Plot the average Tuesday from start to end
  ggplot(data = average_tuesday_window) +
    geom_point(mapping = aes(x = Date, y = Moving_average, color = "Smoothened PC1")) +
    labs( title = "Smoothened PC1 vs. Time") +
    guides(color = guide_legend(title = "Colour Guide")) +
    xlab("Time") +
    ylab("PC1 (Linear Combination") +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")
} else {
  # Plot the average Tuesday from start to end
  ggplot(data = average_tuesday_window) +
    geom_point(mapping = aes(x = Date, y = Moving_average, color = "Smoothened Sub_metering_2")) +
    labs( title = "Smoothened Sub_metering_2 vs. Time") +
    guides(color = guide_legend(title = "Colour Guide")) +
    xlab("Time") +
    ylab("Sub_metering_2") +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")
}

# remove what we don't need anymore
rm(data_07)
rm(average_tuesday)

# Split the data into train and test
# sample <- sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data)))) # used for random sampling and won't work for our purposes.
train <- subset(data, hour(data$Date) >= start & hour(data$Date) < end & wday(data$Date, week_start=1) == tuesday & year(data$Date) < 2009)
test <- subset(data, hour(data$Date) >= start & hour(data$Date) < end & wday(data$Date, week_start=1) == tuesday & year(data$Date) >= 2009)

# Train HMM's
weeks = 107
ntimes = rep(nrow(train)/weeks, weeks)
family = list(gaussian(), gaussian(), gaussian())

if (use_pca){
  features = list(PC1~1, PC2~1, PC3~1)
} else {
  # features should be set
  features = list(Sub_metering_2~1, Global_active_power~1, Voltage~1) #
}

hmm4 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 4, ntimes = ntimes, feature = features, family = family)
hmm6 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 6, ntimes = ntimes, feature = features, family = family)
hmm8 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 8, ntimes = ntimes, feature = features, family = family)
hmm10 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 10, ntimes = ntimes, feature = features, family = family)
hmm12 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 12, ntimes = ntimes, feature = features, family = family)
hmm14 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 14, ntimes = ntimes, feature = features, family = family)
hmm16 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 16, ntimes = ntimes, feature = features, family = family)
hmm18 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 18, ntimes = ntimes, feature = features, family = family)
hmm20 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 20, ntimes = ntimes, feature = features, family = family)
hmm22 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 22, ntimes = ntimes, feature = features, family = family)
hmm24 <- train_multivariate_hmm(data = subset(train, select = -c(Time)), nstates = 24, ntimes = ntimes, feature = features, family = family)

num_states <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
bic <- c(BIC(hmm4), BIC(hmm6), BIC(hmm8),BIC(hmm10),BIC(hmm12),BIC(hmm14),BIC(hmm16),BIC(hmm18),BIC(hmm20),BIC(hmm22), BIC(hmm24))
logl <- c(logLik(hmm4), logLik(hmm6), logLik(hmm8),logLik(hmm10),logLik(hmm12),logLik(hmm14),logLik(hmm16),logLik(hmm18),logLik(hmm20),logLik(hmm22), logLik(hmm24))

bic_logl_df <- data.frame(num_states = num_states, BIC = bic, logl = logl)

ggplot(data = bic_logl_df) +
  geom_line(mapping = aes(x = num_states, y = BIC, color = "BIC Value")) +
  geom_line(mapping = aes(x = num_states, y = logl, colour = "Log Likelihood")) +
  labs(title = "BIC Value and Log Likelihood of various HMM models vs. Number of States") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("States") +
  ylab("Value")

# It looks like 22 states is ideal since BIC for hmm24 increases
# summary(hmm22)
# print(hmm22)

# Feed the scaled test data to this new model in order to calculate log-likelihood of the test data on the trained model.
# need n_times and family arguments

weeks2 = 47
ntimes2 = rep(nrow(test)/weeks2, weeks2)
family = list(gaussian(), gaussian(), gaussian())

test_hmm4 <- test_multivariate_hmm(test, ntimes2, features, family, hmm4)
test_hmm6 <- test_multivariate_hmm(test, ntimes2, features, family, hmm6)
test_hmm8 <- test_multivariate_hmm(test, ntimes2, features, family, hmm8)
test_hmm10 <- test_multivariate_hmm(test, ntimes2, features, family, hmm10)
test_hmm12 <- test_multivariate_hmm(test, ntimes2, features, family, hmm12)
test_hmm14 <- test_multivariate_hmm(test, ntimes2, features, family, hmm14)
test_hmm16 <- test_multivariate_hmm(test, ntimes2, features, family, hmm16)
test_hmm18 <- test_multivariate_hmm(test, ntimes2, features, family, hmm18)
test_hmm20 <- test_multivariate_hmm(test, ntimes2, features, family, hmm20)
test_hmm22 <- test_multivariate_hmm(test, ntimes2, features, family, hmm22)
test_hmm24 <- test_multivariate_hmm(test, ntimes2, features, family, hmm24)

# Compare log likelihoods and BIC's of the models on the TEST data

num_states <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
bic_test <- c(BIC(test_hmm4), BIC(test_hmm6), BIC(test_hmm8),BIC(test_hmm10),BIC(test_hmm12),BIC(test_hmm14),BIC(test_hmm16),BIC(test_hmm18),BIC(test_hmm20),BIC(test_hmm22), BIC(test_hmm24))
logl_test <- c(logLik(test_hmm4), logLik(test_hmm6), logLik(test_hmm8),logLik(test_hmm10),logLik(test_hmm12),logLik(test_hmm14),logLik(test_hmm16),logLik(test_hmm18),logLik(test_hmm20),logLik(test_hmm22), logLik(test_hmm24))

bic_logl_df_test <- data.frame(num_states = num_states, BIC = bic_test, logl = logl_test)

ggplot(data = bic_logl_df_test) +
  geom_line(mapping = aes(x = num_states, y = BIC, color = "BIC Value")) +
  geom_line(mapping = aes(x = num_states, y = logl, colour = "Log Likelihood")) +
  labs(title = "BIC Value and Log Likelihood of various HMM models vs. Number of States on TEST Data") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("States") +
  ylab("Value")
