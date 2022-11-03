library(tidyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(lubridate)
library(forecast)

# Set the proper working directory
setwd("/Users/MichaelKuby/Documents/GitHub/CMPT318_CyberSecurity/CMPT318_Assignment_2/Group Assignment 2")

# Get the data from the data set
data <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE)

# Convert the column Date to Date objects (so that we can extract)
data$Date = as.POSIXlt(data$Date, format = "%d/%m/%Y")

# Extract data from all complete weeks (we will take away the last day as it is a monday)
df <- data[data$Date >= as.POSIXlt("2007-01-01") & data$Date <= as.POSIXlt("2007-12-30"),]

# Parse column 2 into separate columns
dfsplit <- strsplit(df[,2], ",") # split the data
df2 <- data.frame(dfsplit) # Put the data into a df
df2 <- as.data.frame(t(df2)) #transpose to turn rows into cols

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
df <- subset (df, select = -c( Global_reactive_power, Global_active_power, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3) )
df$Day = weekdays(as.Date(df$Date))

#Convert char to double
df$Global_intensity <- as.double(df$Global_intensity) # Convert char to double

# n weeks, ms samples and msn number of samples returned for the week
n = 52
nw = 1
msn = 10080
ms = 10
dfw <- data.frame(Num_week=df$Global_intensity, Time=df$Time, Moving_average=df$Global_intensity)

# breakdown the average calculations per week 
for (i in 1:n) {
  nStart = i*10080 - 10079
  nEnd = nStart + 10079
  dfw[nStart:nEnd, 1] = i
  dfw[nStart:nEnd, 2] = ma(df$Global_intensity[nStart:nEnd], ms, centre = TRUE)
}
dfw$Day <- df$Day
dfw <- na.omit(dfw)           # remove all incomplete cases of a data object

# create average week
df3 <- na.omit(df)           # remove all incomplete cases of a data object
average_week <- aggregate(x = df3$Global_intensity, by = list(df3$Time, df3$Day), FUN = mean, simplify = TRUE, drop = TRUE)
colnames(average_week) <- c("Time", "Day", "Averaged Global_intensity")

