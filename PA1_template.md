# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


Sum the data by days and draw a histogram 


```r
totalsteps <- with(activity, tapply(steps, date, sum))
hist(totalsteps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Count the mean and the median


```r
mean(totalsteps, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(totalsteps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averagesteps <- with(activity, tapply(steps, as.factor(interval), mean, na.rm = TRUE))
plot(names(averagesteps), averagesteps, type="l", xlab = "Time of day", ylab = "Steps", main = "Daily activity pattern", col=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
names(averagesteps[averagesteps==max(averagesteps)])
```

```
## [1] "835"
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
anyNA(activity$date)
```

```
## [1] FALSE
```

```r
anyNA(activity$interval)
```

```
## [1] FALSE
```

```r
anyNA(activity$steps)
```

```
## [1] TRUE
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* We may use the data for the same time periods for all days

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity2 <- activity
activity2$means <- ave(activity2$steps, as.factor(activity2$interval), FUN = function(x) mean(x, na.rm = TRUE))
activity2$steps[is.na(activity2$steps)] <- activity2$means[is.na(activity$steps)]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalsteps2 <- with(activity2, tapply(steps, date, sum))
hist(totalsteps2)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Print mean and median

```r
mean(totalsteps2)
```

```
## [1] 10766
```

```r
median(totalsteps2)
```

```
## [1] 10766
```

* Almost no impact.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity2$weekday <- ifelse(weekdays(as.Date(activity2$date)) %in% c("Sunday", "Saturday"), "weekend", "weekday") 
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(ggplot2)
qplot(interval, steps, data=activity2, stat = "summary", fun.y = "mean", geom = "line", facets = weekday ~ .)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
