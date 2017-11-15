---
title: "Rep Assignment 1"
author: "Cong Liu"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(lattice)
```

```{r read_data}
data_raw <- read.csv("D:/Folder/Coursera/R/Reproducible Research/week2/activity.csv", sep = ",", stringsAsFactors = F, header = T)
activity <- data_raw[,c("date","interval","steps")]
```

```{r message=FALSE, warning=FALSE}
activity_sub1 <- activity[!is.na(activity$steps),-2]
sum_step <- aggregate(activity_sub1[,-1], by=list(activity_sub1$date), FUN = 'sum')
names(sum_step) <- c("Date", "Total Steps")

histogram(~`Total Steps`, data=sum_step, col = "grey")
```

#####Mean of the total steps taken per day: `r format(round(mean(sum_step[,2]),3), scientific = F)`
#####Median of the total steps taken per day: `r format(median(sum_step[,2]), scientific = F)`

<br>

```{r}
activity_sub2 <- activity[!is.na(activity$steps),-1]
mean_step <- aggregate(activity_sub2[,-1], by=list(activity_sub2$interval), FUN = 'mean')
names(mean_step) <- c("Interval", "Average Steps")
xyplot(`Average Steps` ~ Interval, data = mean_step, type = "l")
```

####The interval `r mean_step[which(mean_step[,2] == max(mean_step[,2])),1]` contains the maximum number of average steps

<br>

####`r sum(is.na(activity))` missing values

####use mean of the 5-minite interval to replace missing values


```{r}
activity_sub3 <- activity[is.na(activity$steps),-3]#missing values
names(mean_step) <- c("interval","steps")
activity_sub4 <- rbind(merge(activity_sub3, mean_step, all = T), activity[!is.na(activity$steps),])#replace missing value with mean and combine with non-missing values
sum_step_new <- aggregate(activity_sub4[3], by = list(activity_sub4$date), FUN = "sum")
names(sum_step_new) <- c("Date", "New Total Steps")

histogram(~`New Total Steps`, data=sum_step_new, col = "grey")

```

#####Mean of the total steps taken per day: `r format(round(mean(sum_step_new[,2]),3), scientific = F)`
#####Median of the total steps taken per day: `r format(median(sum_step_new[,2]), scientific = F)`

####Means are the same, while medians are different. The histogram looks more normalized

```{r}
activity_sub5 <- activity_sub4
activity_sub5$day <- weekdays(as.Date.character(activity_sub5$date))
activity_sub5$day <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", activity_sub5$day)
activity_sub5$day <- gsub("Saturday|Sunday", "weekend", activity_sub5$day)
activity_sub5$day <- as.factor(activity_sub5$day)

mean_step_new <- aggregate(activity_sub5[3], by = list(activity_sub5$interval, activity_sub5$day), FUN = "mean")
names(mean_step_new) <- c("Interval","Day","Average steps")
xyplot(`Average steps` ~ Interval |as.factor(Day), data = mean_step_new, type = "l")
```
###https://github.com/bitaolc/RepData_PeerAssessment1

###589a43e0172d808c7c7637037e93d9353ede47fc
