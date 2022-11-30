library(tidyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(lubridate)
library(forecast)
library(zoo)
library("depmixS4")

load_data <- function(filename) {
  # Get the data from the data set
  data <- read.table(filename, header = TRUE)
  
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
load_anomaly_data <- function(filename){
  # Get the data from the data set
  data <- read.table(filename, header = TRUE)
  
  # Convert the column Date to Date objects (so that we can extract)
  data$Date = as.POSIXlt(data$Date, format = "%d/%m/%Y")
  
  
  # Parse column 2 into separate columns
  dfsplit <- strsplit(data[,2], ",") # split the data
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
  data$Date <- as.POSIXlt(paste(data$Date, df2$Time), format = "%Y-%m-%d %H:%M:%S")
  
  # Drop the columns that we don't want anymore
  data <- subset (data, select = Date)
  data$Time <- df2$Time
  df2 <- subset (df2, select = -Time)
  
  # Merge df and df2 and deselect all we dont need
  data <- cbind(data, df2)
  
  # Convert from char to numeric values
  data[,3:9] <- sapply(data[,3:9], as.numeric)
  return (data)
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
train_multivariate_hmm <- function(data, nstates, end, start, use_pca) {
  set.seed(1)
  
  # Train HMM's
  weeks = nrow(data) / ((end - start) * 60)
  ntimes = rep(nrow(data)/weeks, weeks)
  
  if (use_pca){
    features = list(PC1~1, PC2~1, PC3~1)
    family = list(gaussian(), gaussian(), gaussian())
  } else {
    # features should be set
    features = list(Global_active_power~1, Sub_metering_2~1)
    family = list(multinomial("identity"), multinomial("identity")) #CONVERGED WITH multinomial(), gaussian()
  }
  
  model <- depmix(features, data = data, nstates = nstates, ntimes = ntimes, family=family)
  hmm <- fit(model)
  return (hmm)
}
test_multivariate_hmm <- function(data, chosen_hmm, end, start, use_pca) {
  set.seed(1)
  weeks = nrow(data) / ((end - start) * 60)
  ntimes = rep(nrow(data)/weeks, weeks)
  
  if (use_pca){
    features = list(PC1~1, PC2~1, PC3~1)
    family = list(gaussian(), gaussian(), gaussian())
  } else {
    # features should be set
    features = list(Global_active_power~1, Sub_metering_2~1)
    family = list(multinomial("identity"), multinomial("identity")) #CONVERGED WITH multinomial(), gaussian()
  }
  
  new_mod <- depmix(response = features, data = data, nstates = chosen_hmm@nstates, ntimes = ntimes, family=family)
  new_mod <- setpars(new_mod, getpars(chosen_hmm))
  return(new_mod)
}
compute_log_Lik <- function (filename, start, end, day, PCA, chosen_hmm, use_pca) {
  # import data to test for anomalies
  anomaly_data <- load_anomaly_data(filename)
  
  # Select the hours we are interested in
  anomaly_data <- subset(anomaly_data, hour(anomaly_data$Date) >= start & hour(anomaly_data$Date) < end & wday(anomaly_data$Date, week_start=1) == tuesday)
  
  if (use_pca) {
    # Convert and scale the data based on the PCA
    anomaly_data_PC_scaled <- scale(x = subset(anomaly_data, select = -c(Date, Time)), PCA$center, PCA$scale) %*% PCA$rotation 
    
    # Drop features 4-7
    anomaly_data_PC_scaled <- subset(x = anomaly_data_PC_scaled, select = c(PC1, PC2, PC3))
    
    # Put the data in a dataframe not a matrix
    anomaly_data_PC_scaled <- as.data.frame(anomaly_data_PC_scaled)
    
    weeks = nrow(anomaly_data_PC_scaled) / ((end - start) * 60)
    ntimes = rep(nrow(anomaly_data_PC_scaled)/weeks, weeks)
    
    # Test the data with our chosen hmm for the log Likelihood
    tested_hmm <- test_multivariate_hmm(anomaly_data_PC_scaled, chosen_hmm, end, start, use_pca)
    new_fm <- forwardbackward(tested_hmm, return.all=TRUE, useC=TRUE)
    print(new_fm$logLik)
  } else {
    # non PCA 
  }
  return (tested_hmm)
}


# main
setwd("/Users/MichaelKuby/Documents/GitHub/CMPT318_CyberSecurity/Term Project")
df <- load_data("TermProjectData.txt")

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
  data <- as.data.frame(scale(df[3:9], center = TRUE, scale = TRUE))
  data$Date <- df$Date
  data$Time <- df$Time
  data <- subset(data, select = c(Date, Time, Global_active_power, Sub_metering_2))
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
train <- subset(train, select = -c(Date, Time))
test <- subset(test, select = -c(Date, Time))

############ CAREFUL ############
if (use_pca) {
  hmm4_PC <- train_multivariate_hmm(data = train, nstates = 4, end = end, start = start, use_pca = use_pca)
  hmm6_PC <- train_multivariate_hmm(data = train, nstates = 6, end = end, start = start, use_pca = use_pca)
  hmm8_PC <- train_multivariate_hmm(data = train, nstates = 8, end = end, start = start, use_pca = use_pca)
  hmm10_PC <- train_multivariate_hmm(data = train, nstates = 10, end = end, start = start, use_pca = use_pca)
  hmm12_PC <- train_multivariate_hmm(data = train, nstates = 12, end = end, start = start, use_pca = use_pca)
  hmm14_PC <- train_multivariate_hmm(data = train, nstates = 14, end = end, start = start, use_pca = use_pca)
  hmm16_PC <- train_multivariate_hmm(data = train, nstates = 16, end = end, start = start, use_pca = use_pca)
  hmm18_PC <- train_multivariate_hmm(data = train, nstates = 18, end = end, start = start, use_pca = use_pca)
  hmm20_PC <- train_multivariate_hmm(data = train, nstates = 20, end = end, start = start, use_pca = use_pca)
  hmm22_PC <- train_multivariate_hmm(data = train, nstates = 22, end = end, start = start, use_pca = use_pca)
  hmm24_PC <- train_multivariate_hmm(data = train, nstates = 24, end = end, start = start, use_pca = use_pca)
} else {
  hmm4_of <- train_multivariate_hmm(data = train, nstates = 4, end = end, start = start, use_pca = use_pca)
  hmm6_of <- train_multivariate_hmm(data = train, nstates = 6, end = end, start = start, use_pca = use_pca)
  hmm8_of <- train_multivariate_hmm(data = train, nstates = 8, end = end, start = start, use_pca = use_pca)
  hmm10_of <- train_multivariate_hmm(data = train, nstates = 10, end = end, start = start, use_pca = use_pca)
  hmm12_of <- train_multivariate_hmm(data = train, nstates = 12, end = end, start = start, use_pca = use_pca)
  hmm14_of <- train_multivariate_hmm(data = train, nstates = 14, end = end, start = start, use_pca = use_pca)
  hmm16_of <- train_multivariate_hmm(data = train, nstates = 16, end = end, start = start, use_pca = use_pca)
  hmm18_of <- train_multivariate_hmm(data = train, nstates = 18, end = end, start = start, use_pca = use_pca)
  hmm20_of <- train_multivariate_hmm(data = train, nstates = 20, end = end, start = start, use_pca = use_pca)
  hmm22_of <- train_multivariate_hmm(data = train, nstates = 22, end = end, start = start, use_pca = use_pca)
  hmm24_of <- train_multivariate_hmm(data = train, nstates = 24, end = end, start = start, use_pca = use_pca)
}

#Graph the results from the train data
num_states <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
bic <- c(BIC(hmm4_PC), BIC(hmm6_PC), BIC(hmm8_PC),BIC(hmm10_PC),BIC(hmm12_PC),BIC(hmm14_PC),BIC(hmm16_PC),BIC(hmm18_PC),BIC(hmm20_PC),BIC(hmm22_PC), BIC(hmm24_PC))
logl <- c(logLik(hmm4_PC), logLik(hmm6_PC), logLik(hmm8_PC),logLik(hmm10_PC),logLik(hmm12_PC),logLik(hmm14_PC),logLik(hmm16_PC),logLik(hmm18_PC),logLik(hmm20_PC),logLik(hmm22_PC), logLik(hmm24_PC))

bic_logl_df <- data.frame(num_states = num_states, BIC = bic, logl = logl)

ggplot(data = bic_logl_df) +
  geom_line(mapping = aes(x = num_states, y = BIC, color = "BIC Value")) +
  geom_line(mapping = aes(x = num_states, y = logl, colour = "Log Likelihood")) +
  labs(title = "BIC Value and Log Likelihood of various HMM models vs. Number of States") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("States") +
  ylab("Value")

# Feed the scaled test data to this new model in order to calculate log-likelihood of the test data on the trained model.
if (use_pca) {
  test_hmm4_PC <- test_multivariate_hmm(test, hmm4_PC, end, start, use_pca)
  test_hmm6_PC <- test_multivariate_hmm(test, hmm6_PC, end, start, use_pca)
  test_hmm8_PC <- test_multivariate_hmm(test, hmm8_PC, end, start, use_pca)
  test_hmm10_PC <- test_multivariate_hmm(test, hmm10_PC, end, start, use_pca)
  test_hmm12_PC <- test_multivariate_hmm(test, hmm12_PC, end, start, use_pca)
  test_hmm14_PC <- test_multivariate_hmm(test, hmm14_PC, end, start, use_pca)
  test_hmm16_PC <- test_multivariate_hmm(test, hmm16_PC, end, start, use_pca)
  test_hmm18_PC <- test_multivariate_hmm(test, hmm18_PC, end, start, use_pca)
  test_hmm20_PC <- test_multivariate_hmm(test, hmm20_PC, end, start, use_pca)
  test_hmm22_PC <- test_multivariate_hmm(test, hmm22_PC, end, start, use_pca)
  test_hmm24_PC <- test_multivariate_hmm(test, hmm24_PC, end, start, use_pca)
} else {
  test_hmm4_of <- test_multivariate_hmm(test, hmm4_of, end, start, use_pca)
  test_hmm6_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm8_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm10_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm12_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm14_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm16_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm18_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm20_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm22_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
  test_hmm24_of <- test_multivariate_hmm(test, chosen_hmm, end, start, use_pca)
}

#Graph the results from the test data
num_states <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)

if (use_pca) {
  bic <- c(BIC(test_hmm4_PC), BIC(test_hmm6_PC), BIC(test_hmm8_PC), BIC(test_hmm10_PC), BIC(test_hmm12_PC), BIC(test_hmm14_PC), BIC(test_hmm16_PC), BIC(test_hmm18_PC), BIC(test_hmm20_PC), BIC(test_hmm22_PC), BIC(test_hmm24_PC))
  logl <- c(logLik(test_hmm4_PC), logLik(test_hmm6_PC), logLik(test_hmm8_PC),logLik(test_hmm10_PC),logLik(test_hmm12_PC),logLik(test_hmm14_PC),logLik(test_hmm16_PC),logLik(test_hmm18_PC),logLik(test_hmm20_PC),logLik(test_hmm22_PC), logLik(test_hmm24_PC))
} else {
  bic <- c(BIC(test_hmm4_of), BIC(test_hmm6_of), BIC(test_hmm8_of), BIC(test_hmm10_of), BIC(test_hmm12_of), BIC(test_hmm14_of), BIC(test_hmm16_of), BIC(test_hmm18_of), BIC(test_hmm20_of), BIC(test_hmm22_of), BIC(test_hmm24_of))
  logl <- c(logLik(test_hmm4_of), logLik(test_hmm6_of), logLik(test_hmm8_of),logLik(test_hmm10_of),logLik(test_hmm12_of),logLik(test_hmm14_of),logLik(test_hmm16_of),logLik(test_hmm18_of),logLik(test_hmm20_of),logLik(test_hmm22_of), logLik(test_hmm24_of))
}

bic_logl_df <- data.frame(num_states = num_states, BIC = bic, logl = logl)

ggplot(data = bic_logl_df) +
  geom_line(mapping = aes(x = num_states, y = BIC, color = "BIC Value")) +
  geom_line(mapping = aes(x = num_states, y = logl, colour = "Log Likelihood")) +
  labs(title = "BIC Value and Log Likelihood of various HMM models vs. Number of States") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("States") +
  ylab("Value")

# set the chosen HMM
if (use_pca) {
  chosen_hmm = hmm24_PC
} else {
  # set the chosen hmm
}

# Test data sets for anomalies
anomaly_data_1 <- compute_log_Lik("DataWithAnomalies1.txt", start, end, tuesday, df.pca, chosen_hmm, use_pca)
anomaly_data_2 <- compute_log_Lik("DataWithAnomalies2.txt", start, end, tuesday, df.pca, chosen_hmm, use_pca)
anomaly_data_3 <- compute_log_Lik("DataWithAnomalies3.txt", start, end, tuesday, df.pca, chosen_hmm, use_pca)

