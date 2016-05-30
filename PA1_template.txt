library(knitr)

library(dplyr)
library(ggplot2)

# set the file url 
# fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

setwd( "/home/odelibalta/Documents/Study/Coursera/DataScientist/5_Reproducible_Research/Week2/Assignment" )
if ( ! file.exists( "./data" ) ) 
      dir.create( "./data" )
 

# download.file( fileUrl, destfile = "./data/zipData.zip", method = "curl" )

###Unzip DataSet to /data directory
# unzip(zipfile="./data/zipData.zip",exdir="./data")

# load data
# read.csv('household_power_consumption.txt',header=T, sep=';')
df <- read.csv( './data/activity.csv', header=T, sep=',' ) 

# remove NA 
data <- df[ with ( df, { !( is.na( steps ) ) } ), ]

# 1 Calculate the total number of steps taken per day
daily <- group_by( data, date )
steps_per_day <- summarise( daily, total = sum( steps ) )

# 2 Make a histogram of the total number of steps taken each day
hist( steps_per_day$total, main="total number of steps per day", xlab="Total number of steps a day")

# 3 Calculate and report the mean and median of the total number of steps taken per day
summary( steps_per_day )

## Mean is 10766, median is 10765.

## What is the average daily activity pattern?

# 1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_by_interval <- aggregate( steps ~ interval, data, mean )

plot( steps_by_interval$interval, steps_by_interval$steps, type='l', main="Avg number of steps across all days", 
      xlab="Interval", ylab="Avg number of steps")

# 2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps  <- which.max( steps_by_interval$steps )
steps_by_interval[ max_steps, ]

# The interval 835 has the maximum average value of steps (206.1698).

# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum( is.na( df ) )
# answer is 2304

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# below is just the strategy, actual code is within the next step
mean( df$steps, na.rm = T )

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
df_no_na <- df
# subset the na s and assign them the mean of the day
df_no_na$steps[is.na( df_no_na$steps ) ] <- mean( df_no_na$steps, na.rm = T )
 
 
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
df_no_na_steps_daily <- aggregate( steps ~ date, df_no_na, sum )
hist(df_no_na_steps_daily$steps, main="Total number of steps per day (NA removed)", 
     xlab="Daily total number of steps")

# get mean and median of imputed data
mean( df_no_na_steps_daily$steps )
median( df_no_na_steps_daily$steps )

# get mean and median of data without NA's
mean( steps_per_day$total ) 
median( steps_per_day$total )

# While the median increases, mean value stays the same

## Are there differences in activity patterns between weekdays and weekends?

# 1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

df_no_na['type_of_day'] <- weekdays( as.Date( df_no_na$date ) )
# subset the weekend days to the weekend
df_no_na$type_of_day[df_no_na$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
# rest is the week
df_no_na$type_of_day[df_no_na$type_of_day != "weekend"] <- "weekday"
# this needs to be a vector 
df_no_na$type_of_day <- as.factor( df_no_na$type_of_day )

# getting the avg steps 5 min interval 
df_no_na_avg_steps_interval <- aggregate( steps ~ interval + type_of_day, df_no_na, mean )



# creat a plot
qplot(interval, 
      steps, 
      data = df_no_na_avg_steps_interval,  
      geom = c("line"), 
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
      facet_wrap(~ type_of_day, ncol = 1)























