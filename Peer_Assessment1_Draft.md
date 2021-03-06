Reproducible Research - Peer Assessment 1
========================================================

## Introduction

"It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day." 
*http://class.coursera.org/repdata-003/human_grading/view/courses/972142/assessments/3/submissions, 11 June 2014.*

## Loading and preprocessing the data

We load the data using read.csv(), and format the date as a POSIXct variable.


```r
unzip('./activity.zip')
options(stringsAsFactors = FALSE)
activity <- read.csv('activity.csv',header=T)
activity$date <- as.POSIXct(activity$date)
```

## What is mean total number of steps taken per day?

We use the aggregate() function to find the total number of steps taken per day, then compute the mean and median values, ignoring missing values.


```r
stepsPerDay <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
mean(stepsPerDay$x,na.rm = T)
```

```
## [1] 10766
```

```r
median(stepsPerDay$x,na.rm = T)
```

```
## [1] 10765
```

Here is a histogram of the number of steps taken per day.


```r
hist(stepsPerDay$x,breaks=seq(0,25000,by=500),main = "Number of steps taken",xlab = "Steps")
abline(v = mean(stepsPerDay$x,na.rm = T), col = "blue", lwd = 2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## What is the average daily activity pattern?

We use the aggregate() function to calculate the average number of steps taken, averaged across all days, for each five minute interval, and store this information in a dataframe stepsPerInt.  We then generate a line plot showing the average daily activity pattern for the day.



```r
stepsPerInt <- aggregate(activity$steps, by=list(activity$interval), FUN=mean,na.rm=T)
maxStepInt <- subset(stepsPerInt,(stepsPerInt$x==max(stepsPerInt$x)))
plot(stepsPerInt,type="l", xlab="Interval", ylab="Steps", main="Number of steps taken")
points(x=maxStepInt$Group.1, y=maxStepInt$x, col="red", pch=4, cex = 2)
axis(1,c(maxStepInt$Group.1),col="red",labels=F)
axis(2,c(maxStepInt$x),col="red",labels=F)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

We see that on average across all the days in the dataset, the maximum number of steps were taken during the 835 interval.

## Imputing missing values

We see that this data set contains 2304 missing values.


```r
nrow(subset(activity,is.na(activity$steps)))
```

```
## [1] 2304
```

We create a function findAverage() that pulls the average number of steps taken for a given interval from stepsPerInt.  We use this function to fill in missing values with the average number of steps taken during the interval associated with that missing value, creating an imputed dataset called imputed.


```r
findAverage <- function(i) {
        as.integer(subset(stepsPerInt,(stepsPerInt$Group.1==i))$x)
                            }
imputed <- activity
for (r in 1:nrow(imputed)) {
        if (is.na(imputed$steps[r])) {
                imputed$steps[r] <- findAverage(imputed$interval[r])
        }
}
```

We then compute the number of steps taken per day in the imputed data set.  We find that the mean is slightly lower and the median is significantly lower.



```r
stepsPerDayImp <- aggregate(imputed$steps, by=list(imputed$date), FUN=sum)
mean(stepsPerDayImp$x,na.rm = T)
```

```
## [1] 10750
```

```r
median(stepsPerDayImp$x,na.rm = T)
```

```
## [1] 10641
```

We also see that the imputed data set is more clustered around the mean that the non-imputed data set.


```r
par(mfrow = c(2,1))
hist(stepsPerDay$x,breaks=seq(0,25000,by=500),main = "Non-imputed data",xlab = "Steps")
abline(v = mean(stepsPerDay$x,na.rm = T), col = "blue", lwd = 2)
hist(stepsPerDayImp$x,breaks=seq(0,25000,by=500),main = "Imputed data",xlab = "Steps")
abline(v = mean(stepsPerDayImp$x,na.rm = T), col = "blue", lwd = 2)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

## Are there differences in activity patterns between weekdays and weekends?

We create a factor variable with two levels, weekday and weekend.  We use this factor variable to create a plot showing the average number of steps taken per interval across all weekend days, and another showing the average number of steps taken per interval across all weekdays.  We see that more steps are taken in the afternoon on weekends.


```r
wknd <- c('Saturday','Sunday')
imputed$wknd <- factor(ifelse(weekdays(imputed$date) %in% wknd,'weekend','weekday'))
weekendData <- subset(imputed,imputed$wknd=='weekend')
weekdayData <- subset(imputed,imputed$wknd=='weekday')

stepsPerIntWknd <- aggregate(weekendData$steps, by=list(weekendData$interval), FUN=mean,na.rm=T)
stepsPerIntWkdy <- aggregate(weekdayData$steps, by=list(weekdayData$interval), FUN=mean,na.rm=T)

par(mfrow = c(2,1))
plot(stepsPerIntWknd,type="l", xlab="", ylab="Steps", main="Weekends")
plot(stepsPerIntWkdy,type="l", xlab="Interval", ylab="Steps", main="Weekdays")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
