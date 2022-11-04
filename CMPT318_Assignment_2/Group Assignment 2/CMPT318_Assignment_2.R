library(tidyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(lubridate)
library(forecast)
library(zoo)

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
rm(df2)
rm(data)

# n weeks, ms samples and msn number of samples returned for the week
n = 52
nw = 1
msn = 10080
ms = 10
df_ma <- data.frame(Date = df$Date, 
                    Num_week=strftime(df$Date, format = "%V"), 
                    Day = strftime(df$Date, format = "%A"),
                    Time = strftime(df$Date, format = "%T"),
                    Moving_average=df$Global_intensity)

df_ma$Num_week <- sub("^0", "", df_ma$Num_week)

# breakdown the average calculations per week 
df_ma$Moving_average <- ma(df$Global_intensity, ms, centre = TRUE)

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

# Compute Squared Error

for (i in 01:n)
{
  df_ma[df_ma$Num_week == i, "Squared_error"] <- (average_week$'Moving_average' - df_ma[df_ma$Num_week == i, "Moving_average"])^2
}
df_ma2 <- na.omit(df_ma)

# aggregate by the week, taking the sum of squared errors
weekly_error <- df_ma2 %>% group_by(Num_week)
weekly_error <- weekly_error %>% summarise(
  Sum_of_Squared_Errors = sum(Squared_error)
)

for (j in 1:n)
{
  weekly_error[weekly_error$Week == j, "Mean Squared Error"] <- weekly_error[weekly_error$Week == j, "Sum of Squared Errors"] / length(which(df_ma2$Num_week == j))
}

weekly_error$rank <- rank(weekly_error$`Mean Squared Error`)

# Most anomalous week is week 52 given by the greatest MSE
# Least anomalous week is week 17 given by the lowest MSE

# Plot the smoothened versions of the most and least anomalous weeks against the average smoothened week

dates <- data.frame(Date=df$Date[df$Date >= as.POSIXlt("2007-04-23") & df$Date < as.POSIXlt("2007-04-30")])
dates <- na.omit(dates)

df_plot <- data.frame(Date = dates,
                      Week_17 = df_ma[df_ma$Num_week == 17, "Moving_average"], 
                      Week_52 = df_ma[df_ma$Num_week == 52, "Moving_average"], 
                      Average_Week = average_week$`Moving_average`)

# I noticed that week 17 has a lot of NA data points. So the following is to check the original data to see if 
# this was an error produced somewhere, or present in the original data. Week 17 is april 23 to may 29 
# sum(is.na(df$Global_intensity[df$Date >= as.POSIXlt("2007-04-23") & df$Date < as.POSIXlt("2007-04-30")]))
# sum(is.na(df_plot$Week_17))
# I can see in the original data that there are 2919 NA's for week 17 global intensity. Strangely in the smoothened data we are missing less at 2869.
# I assume the moving average technique allows for certain points that would have been NA to actually obtain a value, which is kind of interesting.

ggplot(data = df_plot) +
  geom_point(mapping = aes(x = Date, y = Week_17, color = "Least Anomalous (Week 17)")) +
  geom_point(mapping = aes(x = Date, y = Week_52, color = "Most Anomalous (Week 52)")) +
  geom_point(mapping = aes(x = Date, y = Average_Week, color = "Average Week"))+
  labs( title = "Smoothened Global Intensity vs. Time") +
  guides(color = guide_legend(title = "Colour Guide")) +
  xlab("Time") +
  ylab("Global Intensity (Amperes)")

weekly_error <- subset (weekly_error, select = c( "Week", "Mean Squared Error", "rank" ))