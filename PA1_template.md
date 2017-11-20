# Rep Assignment 1
`r Sys.Date()`  



###1. Code for reading in the dataset and/or processing the data

```r
setwd("D:/Folder/Coursera/R/Reproducible Research/week2")
data_raw <- read.csv("activity.csv", sep = ",", stringsAsFactors = F, header = T)
activity <- data_raw[,c("date","interval","steps")]
```

<br>

###2. Histogram of the total number of steps taken each day

```r
activity_sub1 <- activity[!is.na(activity$steps),-2]
sum_step <- aggregate(activity_sub1[,-1], by=list(activity_sub1$date), FUN = 'sum')
names(sum_step) <- c("Date", "Total Steps")

histogram(~`Total Steps`, data=sum_step, col = "grey", breaks = 20)
```

![](https://github.com/bitaolc/RepData_PeerAssessment1/blob/master/PA1_template_files/unnamed-chunk-1-1.png)<!-- -->
<br>

###3. Mean and median number of steps taken each day

#####Mean of the total steps taken per day: 10766.19

#####Median of the total steps taken per day: 10765

<br>

###4. Time series plot of the average number of steps taken

```r
activity_sub2 <- activity[!is.na(activity$steps),-1]
mean_step <- aggregate(activity_sub2[,-1], by=list(activity_sub2$interval), FUN = 'mean')
names(mean_step) <- c("Interval", "Average Steps")
xyplot(`Average Steps` ~ Interval, data = mean_step, type = "l")
```

![](https://github.com/bitaolc/RepData_PeerAssessment1/blob/master/PA1_template_files/unnamed-chunk-2-1.png)<!-- -->

<br>

###5. The 5-minute interval that, on average, contains the maximum number of steps

#####The interval 835 contains the maximum number of 206.17 (average) steps

<br>

###6. Code to describe and show a strategy for imputing missing data

#####2304 missing values

#####Use mean of the 5-minite interval to replace missing values

<br>

###7. Histogram of the total number of steps taken each day after missing values are imputed

```r
activity_sub3 <- activity[is.na(activity$steps),-3]#missing values
names(mean_step) <- c("interval","steps")
activity_sub4 <- rbind(merge(activity_sub3, mean_step, all = T), activity[!is.na(activity$steps),])#replace missing value with interval mean and combine with non-missing values
sum_step_new <- aggregate(activity_sub4[3], by = list(activity_sub4$date), FUN = "sum")
names(sum_step_new) <- c("Date", "New Total Steps")

histogram(~`New Total Steps`, data=sum_step_new, col = "grey", breaks = 20)
```

![](https://github.com/bitaolc/RepData_PeerAssessment1/blob/master/PA1_template_files/unnamed-chunk-3-1.png)<!-- -->
<br>

#####Mean of the total steps taken per day: 10766.19

#####Median of the total steps taken per day: 10766.19

#####Mean is the same, while median is different. The histogram looks more normalized

<br>

###8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activity_sub5 <- activity_sub4
activity_sub5$day <- weekdays(as.Date.character(activity_sub5$date))
activity_sub5$day <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", activity_sub5$day)
activity_sub5$day <- gsub("Saturday|Sunday", "weekend", activity_sub5$day)
activity_sub5$day <- as.factor(activity_sub5$day)

mean_step_new <- aggregate(activity_sub5[3], by = list(activity_sub5$interval, activity_sub5$day), FUN = "mean")
names(mean_step_new) <- c("Interval","Day","Average steps")
xyplot(`Average steps` ~ Interval |as.factor(Day), data = mean_step_new, type = "l", layout=c(1,NA))
```

![](https://github.com/bitaolc/RepData_PeerAssessment1/blob/master/PA1_template_files/unnamed-chunk-4-1.png)<!-- -->

#####The object is more active in general throughout the weekends and more active at specific period during weekdays.
