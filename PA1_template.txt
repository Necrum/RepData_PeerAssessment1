---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Load data from current directory

```r
data <- read.csv("activity.csv")
```




## What is mean total number of steps taken per day?
Aggregate data by date and show the histogram

```r
steps <- aggregate(data[1], by=data[2], FUN=sum, na.rm=TRUE)
hist(steps$steps, 
     breaks=30,
     col="green",
     main="Histogram of the total number of steps taken per day",
     xlab="Steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Calculate the mean and median

```r
steps_mean = mean(steps$steps)
steps_median = median(steps$steps)
```
**Mean** of the total number of steps taken per day is 9354.2295082

**Median** of the total number of steps taken per day is 10395




## What is the average daily activity pattern?

```r
interval = aggregate(data[1], by=data[3], FUN=mean, na.rm=TRUE)
plot(x=interval$interval,
     y=interval$steps,
     type="l",
     col="green",
     main="Average steps per 5-minute interval",
     xlab="Interval",
     ylab="Number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Get the interval with maximum number of steps

```r
interval_max <- interval[interval$steps==max(interval$steps),]
```
**Maximum interval** is number 835 with 206.1698113 steps.




## Imputing missing values
The total number of missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

The strategy for filling in all of the missing values in the dataset is to replace them by the mean number of steps for corresponding 5-minute intervals.


```r
library(dplyr)
data_new <- data %>% 
    group_by(interval) %>% 
    mutate(steps=ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
```


Histogram for imputed data

```r
steps_imputed <- aggregate(data_new[1], by=data_new[2], FUN=sum, na.rm=TRUE)
hist(steps_imputed$steps,
     breaks=30,
     col="blue",
     main="Histogram of total number of steps taken per day (imputed)",
     xlab="Steps per Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Calculate the mean and median for imputed data

```r
steps_imputed_mean = mean(steps_imputed$steps)
steps_imputed_median = median(steps_imputed$steps)
```
**Mean (imputed data)** of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>

**Median (imputed data)** of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>




## Are there differences in activity patterns between weekdays and weekends?
Create new factor variable (dayofweek)

```r
data_new <- data_new %>% 
     mutate(date=as.POSIXct(date)) %>%
     mutate(dayofweek=ifelse(weekdays(date) %in% c("Saturday","Sunday"),
                             "Weekend", "Weekday"))
```


Make a panel plot

```r
library(ggplot2)
data_comparison <- aggregate(data_new[1], by=data_new[c(3,4)], FUN=mean, na.rm=TRUE)
plot <- ggplot(data=data_comparison, aes(x=interval,y=steps))
plot + geom_line() + facet_wrap(~dayofweek,nrow=2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
