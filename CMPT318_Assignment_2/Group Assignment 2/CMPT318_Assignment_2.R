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
df <- subset (df, select = -c( Global_reactive_power, Global_active_power, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3) )
df$Day = weekdays(as.Date(df$Date))

#Convert char to double
df$Global_intensity <- as.double(df$Global_intensity) # Convert char to double

# n weeks, ms samples and msn number of samples returned for the week
n = 52
nw = 1
msn = 10080
ms = 10
df_ma <- data.frame(Num_week=df$Global_intensity, Moving_average=df$Global_intensity)

# breakdown the average calculations per week 
for (i in 1:n) {
  nStart = i*10080 - 10079
  nEnd = nStart + 10079
  df_ma[nStart:nEnd, 1] = i
  df_ma[nStart:nEnd, 2] = ma(df$Global_intensity[nStart:nEnd], ms, centre = TRUE)
}
df_ma$Day <- df$Day
df_ma$Time <- df$Time

# create average week
df3 <- na.omit(df)           # remove all incomplete cases of a data object
average_week <- aggregate(x = df3$Global_intensity, by = list(df3$Time, df3$Day), FUN = mean, simplify = TRUE, drop = TRUE)
colnames(average_week) <- c("Time", "Day", "Averaged Global_intensity")

# manually reorder average_week
order <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")
average_week = average_week %>%
  mutate(Day =  factor(Day, levels = order)) %>%
  arrange(Day)

for (i in 1:n)
{
  df_ma[df_ma$Num_week == i, "Squared_error"] <- (average_week$`Averaged Global_intensity` - df_ma[df_ma$Num_week == i, "Moving_average"])^2
}

df_ma2 <- na.omit(df_ma)

# aggregate by the week, taking the sum of squared errors
weekly_error <- aggregate(x = df_ma2$Squared_error, by = list(df_ma2$Num_week), FUN = sum, simplify = TRUE)
colnames(weekly_error) <- c("Week", "Sum of Squared Errors")


for (i in 1:n)
{
  weekly_error[weekly_error$Week == i, "Mean Squared Error"] <- weekly_error[weekly_error$Week == i, "Sum of Squared Errors"] / length(which(df_ma2$Num_week == i))
}

weekly_error$rank <- rank(weekly_error$`Mean Squared Error`)

# Most anomalous week is week 52 given by the greatest MSE
# Least anomalous week is week 17 given by the lowest MSE

# Plot the smoothened versions of the most and least anomalous weeks against the average smoothened week

df_plot <- data.frame(Time=average_week$Time, Week_17 = df_ma[df_ma$Num_week == 17, "Moving_average"], Week_52 = df_ma[df_ma$Num_week == 52, "Moving_average"])
