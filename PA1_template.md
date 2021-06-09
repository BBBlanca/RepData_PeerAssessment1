---
title: "Course project 1"
author: "Yue Wang"
date: "6/8/2021"
output: html_document
---
Loading and preprocessing the data

```r
library(knitr)
setwd("~/Desktop/coursera/Reproducible Research/Course Project 1")
aa<- read.csv("activity.csv")
aa$date<- as.Date(aa$date)
```
To calculate the total number of steps taken per day

```r
bb<- sapply(split(aa$steps, aa$date), sum, na.rm = TRUE)
print(bb)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 
##          0        126      11352      12116      13294      15420      11015          0      12811       9900      10304      17382      12426 
## 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##      15098      10139      15084      13452      10056      11829      10395       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 2012-11-06 2012-11-07 2012-11-08 
##      10119      11458       5018       9819      15414          0      10600      10571          0      10439       8334      12883       3219 
## 2012-11-09 2012-11-10 2012-11-11 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 2012-11-19 2012-11-20 2012-11-21 
##          0          0      12608      10765       7336          0         41       5441      14339      15110       8841       4472      12787 
## 2012-11-22 2012-11-23 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      20427      21194      14478      11834      11162      13646      10183       7047          0
```
To make a histogram of the total number of steps taken per day

```r
hist(bb, xlab = "number of steps", main = "Total number of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
To calculate and report the mean and median of the total number of steps taken per day

```r
mean(bb)
```

```
## [1] 9354.23
```

```r
median(bb)
```

```
## [1] 10395
```
To make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
cc<- as.data.frame(sapply(split(aa$steps, aa$interval), mean, na.rm = TRUE))
colnames(cc) <- "steps"
cc$intervals<- aa[1:288, 3]
library(ggplot2)
g<- ggplot(cc, aes(intervals, steps))
g + geom_point() + geom_line()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
To find out which 5-minute interval contains the maximum number of steps

```r
subset(cc, cc$steps == max(cc$steps))
```

```
##        steps intervals
## 835 206.1698       835
```
To calculate and report the total number of missing values in the dataset

```r
sum(is.na(aa$steps))
```

```
## [1] 2304
```
To devise a strategy for fillling in all of the missing values in the dataset

```r
for(i in 1:nrow(aa)){
  if(is.na(aa[i, 1])){
    aa[i, 1]<- cc[cc$intervals == aa[i, 3], ]$steps
  }
}
newdataset<- aa
```
To make new histogram with new dataset

```r
dd<- sapply(split(newdataset$steps, newdataset$date), sum, na.rm = TRUE)
hist(bb, xlab = "number of steps", main = "Total number of steps taken per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
The new mean and median

```r
mean(dd)
```

```
## [1] 10766.19
```

```r
median(dd)
```

```
## [1] 10766.19
```
To create a new factor variable in the dataset with two levels - "weekday" and "weekend"

```r
newdataset$date<- as.Date(newdataset$date)
newdataset$days<- weekdays(newdataset$date)
for(i in 1:nrow(newdataset)){
 if (newdataset[i, 4] %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
    newdataset[i, 4]<- "weekday"
  } else {newdataset[i, 4]<- "weekend"}
}
newdataset$days<- as.factor(newdataset$days)
head(newdataset)
```

```
##       steps       date interval    days
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```
To differentiate activity levels between weekdays and weekends

```r
ee<- split(newdataset, newdataset$days)
ff<- lapply(ee, function(x) with(x, tapply(steps, interval, mean)))
ff<- as.data.frame(ff)
colnames(ff)<- c("weekday", "weekend")
library(tidyr)
ff<- ff %>% gather(days, steps, "weekday", "weekend")
ff$intervals<- cc$intervals
par(mfrow = c(2, 1))
with(subset(ff, ff$days == "weekday"), plot(intervals, steps, type = "l", main = "weekday"))
with(subset(ff, ff$days == "weekend"), plot(intervals, steps, type = "l", main = "weekend"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
