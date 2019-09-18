---
title: "Reproducible Research - Course Project 1"
author: "Prabeeti Bulani"
date: "9/18/2019"
output: html_document
---


# Introduction
Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute 
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.



# 1. Loading and preprocessing the data

```r
activity_data <- read.csv('activity.csv', header = TRUE, sep = ",",colClasses=c("numeric", "character", "numeric"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
activity_data$date <- as.Date(activity_data$date, format = "%m/%d/%Y")
```

```
## Error in as.Date(activity_data$date, format = "%m/%d/%Y"): object 'activity_data' not found
```

```r
activity_data$interval <- as.factor(activity_data$interval)
```

```
## Error in is.factor(x): object 'activity_data' not found
```

```r
## What is mean total number of steps taken per day?
total_steps_1day <- aggregate(steps ~ date, activity_data, sum)
```

```
## Error in eval(m$data, parent.frame()): object 'activity_data' not found
```

```r
colnames(total_steps_1day) <- c("date","steps")
```

```
## Error in colnames(total_steps_1day) <- c("date", "steps"): object 'total_steps_1day' not found
```

# 2.Make a histogram of the total number of steps taken each day

```r
ggplot(total_steps_1day, aes(x=steps)) +  geom_histogram(fill="yellow",binwidth = 1000) +  xlab("Steps per Day") +  ylab("Count in a day") +
  ggtitle("Histogram-Steps Taken per Day")+ theme_bw() 
```

```
## Error in ggplot(total_steps_1day, aes(x = steps)): object 'total_steps_1day' not found
```

# 3.Mean and median number of steps taken each day

```r
mean_steps_per_day   <- mean(total_steps_1day$steps, na.rm=TRUE)
```

```
## Error in mean(total_steps_1day$steps, na.rm = TRUE): object 'total_steps_1day' not found
```

```r
median_steps_per_day <- median(total_steps_1day$steps, na.rm=TRUE)
```

```
## Error in median(total_steps_1day$steps, na.rm = TRUE): object 'total_steps_1day' not found
```

```r
mean_steps_per_day
```

```
## Error in eval(expr, envir, enclos): object 'mean_steps_per_day' not found
```

```r
median_steps_per_day
```

```
## Error in eval(expr, envir, enclos): object 'median_steps_per_day' not found
```
# 4.Time series plot of the average number of steps taken
#######What is the average daily activity pattern?

```r
daily_activity_pattern <- aggregate(activity_data$steps, 
                                by = list(interval = activity_data$interval),
                                FUN=mean, na.rm=TRUE)
```

```
## Error in aggregate(activity_data$steps, by = list(interval = activity_data$interval), : object 'activity_data' not found
```

```r
#convert to integers this helps in plotting
daily_activity_pattern$interval <- as.integer(levels(daily_activity_pattern$interval)[daily_activity_pattern$interval])
```

```
## Error in levels(daily_activity_pattern$interval): object 'daily_activity_pattern' not found
```

```r
colnames(daily_activity_pattern) <- c("interval", "steps")
```

```
## Error in colnames(daily_activity_pattern) <- c("interval", "steps"): object 'daily_activity_pattern' not found
```

```r
ggplot(daily_activity_pattern, aes(x=interval, y=steps)) +   
        geom_line(color="blue", size=1) +  xlab("Interval") +  ylab("steps_count") +
  ggtitle("Average Daily Steps")+   theme_bw()
```

```
## Error in ggplot(daily_activity_pattern, aes(x = interval, y = steps)): object 'daily_activity_pattern' not found
```

# 5.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_5minute_interval <- daily_activity_pattern[which.max(daily_activity_pattern$steps),]
```

```
## Error in eval(expr, envir, enclos): object 'daily_activity_pattern' not found
```

```r
max_5minute_interval
```

```
## Error in eval(expr, envir, enclos): object 'max_5minute_interval' not found
```
# 6.Code to describe and show a strategy for imputing missing data

```r
#######Imputing missing values
#######Calculate and report the total number of missing values in the dataset 
sum(is.na(activity_data))
```

```
## Error in eval(expr, envir, enclos): object 'activity_data' not found
```

```r
#######Create a new dataset that is equal to the original dataset but with the missing data filled in.Means for the 5-minute intervals are used as fillers for missing values.
activity_data <- merge(activity_data, daily_activity_pattern, by = "interval", suffixes = c("", ".y"))
```

```
## Error in merge(activity_data, daily_activity_pattern, by = "interval", : object 'activity_data' not found
```

```r
step_count <- is.na(activity_data$steps)
```

```
## Error in eval(expr, envir, enclos): object 'activity_data' not found
```

```r
activity_data$steps[step_count] <- activity_data$steps.y[step_count]
```

```
## Error in eval(expr, envir, enclos): object 'activity_data' not found
```

```r
activity_data <- activity_data[, c(1:3)]
```

```
## Error in eval(expr, envir, enclos): object 'activity_data' not found
```

```r
str(activity_data)
```

```
## Error in str(activity_data): object 'activity_data' not found
```
# 7.Histogram of the total number of steps taken each day after missing values are imputed

```r
#######Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps_taken_in_day <- aggregate(steps ~ date, data = activity_data, FUN = sum)
```

```
## Error in eval(m$data, parent.frame()): object 'activity_data' not found
```

```r
ggplot(steps_taken_in_day, aes(x=steps)) +  geom_histogram(fill="magenta",binwidth = 1000) +  xlab("Steps per Day") +  ylab("Count in a day") +
  ggtitle("Histogram-Total no. of Steps Taken per Day")+ theme_bw() 
```

```
## Error in ggplot(steps_taken_in_day, aes(x = steps)): object 'steps_taken_in_day' not found
```

```r
barplot(steps_taken_in_day$steps, names.arg = steps_taken_in_day$date, xlab = "date", ylab = "steps")
```

```
## Error in barplot(steps_taken_in_day$steps, names.arg = steps_taken_in_day$date, : object 'steps_taken_in_day' not found
```

```r
mean2 <- mean(steps_taken_in_day$steps)
```

```
## Error in mean(steps_taken_in_day$steps): object 'steps_taken_in_day' not found
```

```r
median2 <- median(steps_taken_in_day$steps)
```

```
## Error in median(steps_taken_in_day$steps): object 'steps_taken_in_day' not found
```

```r
mean2
```

```
## Error in eval(expr, envir, enclos): object 'mean2' not found
```

```r
median2
```

```
## Error in eval(expr, envir, enclos): object 'median2' not found
```

```r
# Mean and median are same now after imputing data earlier it was different
# Mean and Median After Imputing Data - 10766.19 and 10766.19
# Mean and Median Before Imputing Data - 10766.19 and 10765 
```
# 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
# Are there differences in activity patterns between weekdays and weekends?
#######Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#######Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


activity_data$weekdays <- factor(format(activity_data$date, "%A"))
```

```
## Error in format(activity_data$date, "%A"): object 'activity_data' not found
```

```r
levels(activity_data$weekdays)
```

```
## Error in levels(activity_data$weekdays): object 'activity_data' not found
```

```r
levels(activity_data$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
```

```
## Error in levels(activity_data$weekdays) <- list(weekday = c("Monday", : object 'activity_data' not found
```

```r
levels(activity_data$weekdays)
```

```
## Error in levels(activity_data$weekdays): object 'activity_data' not found
```

```r
table(activity_data$weekdays)
```

```
## Error in table(activity_data$weekdays): object 'activity_data' not found
```

```r
days_average <- aggregate(activity_data$steps, 
                      list(interval = as.numeric(as.character(activity_data$interval)), 
                           weekdays = activity_data$weekdays),
                      FUN = "mean")
```

```
## Error in aggregate(activity_data$steps, list(interval = as.numeric(as.character(activity_data$interval)), : object 'activity_data' not found
```

```r
names(days_average)[3] <- "mean_Steps"
```

```
## Error in names(days_average)[3] <- "mean_Steps": object 'days_average' not found
```

```r
xyplot(days_average$mean_Steps ~ days_average$interval | days_average$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

```
## Error in eval(modelRHS.vars[[i]], data, env): object 'days_average' not found
```


