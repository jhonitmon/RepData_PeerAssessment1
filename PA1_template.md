---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
data <- read.csv("activity.csv")
data$date <- ymd(data$date)
data$interval <- as.factor(data$interval)
```
## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
sum_data <- group_by(data,date) %>% summarise(steps = sum(steps))
sum_data <- sum_data[complete.cases(sum_data$steps),]
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(sum_data$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(sum_data$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps <- median(sum_data$steps)
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval_mean <- aggregate(steps ~ interval, data = data,mean,na.rm = T)
plot(steps ~ interval,data = interval_mean,type = "l")
lines(interval_mean$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval_steps = interval_mean[which.max(interval_mean$steps),]
max_interval_steps
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)

```r
na_sum = sum(is.na(data$steps)) 
na_sum
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_filled = data
for (i in 1:nrow(data_filled)) {
  if(is.na(data_filled$steps[i])) {
    data_filled$steps[i] = subset(interval_mean,interval == data_filled$interval[i])$steps
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sum_data_filled <- group_by(data_filled,date) %>% summarise(steps = sum(steps))
par(mfrow = c(1,2))
hist(sum_data$steps)
hist(sum_data_filled$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_steps_filled <- mean(sum_data_filled$steps)
mean_steps_filled
```

```
## [1] 10766.19
```

```r
mean_change <- (1 - mean_steps/mean_steps_filled) * 100
```

```r
median_steps_filled <- median(sum_data_filled$steps)
median_steps_filled
```

```
## [1] 10766.19
```

```r
median_change <- (1 - median_steps/median_steps_filled) * 100 
```

The mean did not change and the median changed bz about 0.011 %.
Filling the data made the total daily number of steps bigger.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
for (i in 1:nrow(data_filled)) {
  if(weekdays(data_filled$date[i]) == "Saturday" || weekdays(data_filled$date[i]) == "Sunday") {
    data_filled$dayType[i] <- "weekend"
  } else {
    data_filled$dayType[i] <- "weekday"
  }
}
data_filled$dayType <- as.factor(data_filled$dayType)
```

2. Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data_filled_mean <- aggregate(steps ~ interval + dayType, data = data_filled,mean,na.rm = T)
data_filled_mean$interval <- as.integer(data_filled_mean$interval)
library(lattice)
xyplot(steps ~ interval | dayType, data_filled_mean, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps",)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
