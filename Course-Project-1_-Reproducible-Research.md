---
title: 'Course Project 1: Reprodicible Research'
author: "Manuel Diaz"
output: 
  html_document:
    keep_md: true
---

## Introduction.

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br>
date: The date on which the measurement was taken in YYYY-MM-DD format </br>
interval: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 


## Loading and preprocessing the data.


```r
library(ggplot2)
library(data.table)
library(ggthemes)
```



```r
"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" -> url1

setwd("C:/Users/aleja/Documents/Cursos/Coursera R pratices/RepData_PeerAssessment1")

ifelse(!dir.exists(file.path(getwd(), "Data")), 
       dir.create(file.path(getwd(), "Data")), FALSE)
```

```
## [1] FALSE
```

```r
download.file(url = url1, file.path("./Data","Assesment1_data.zip" ),
              method = "curl")


unzip("Assesment1_data.zip", exdir = "./Data")
list.files(path = "./Data")
```

```
## [1] "activity.csv"        "Assesment1_data.zip"
```

```r
fread("./Data/activity.csv")-> df
```


```r
head(df)
```

```
##    steps       date interval
## 1:    NA 2012-10-01        0
## 2:    NA 2012-10-01        5
## 3:    NA 2012-10-01       10
## 4:    NA 2012-10-01       15
## 5:    NA 2012-10-01       20
## 6:    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

* Calculate the total number of steps taken per day.

```r
with(df, aggregate(steps, by=list(date), FUN = sum, na.rm=T))->total_steps
total_steps$Group.1<-as.Date(total_steps$Group.1,"%Y-%m-%d");head(total_steps)
```

```
##      Group.1     x
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
names(total_steps)<-c("Date","N_steps")
```

* If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
ggplot(total_steps, aes(x = N_steps)) +
     geom_histogram(fill = "red", binwidth = 1000) +
     labs(title = "Steps by Date", x = "Steps", y = "Frequency")+
     theme_economist_white()
```

![](Course-Project-1_-Reproducible-Research_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

* Calculate and report the mean and median of the total number of steps taken per day.


```r
cat("Mean of Steps", mean(total_steps$N_steps, na.rm = T), sep = "\n")
```

```
## Mean of Steps
## 9354.23
```

```r
cat("Median of Steps", median(total_steps$N_steps, na.rm = T), sep = "\n")
```

```
## Median of Steps
## 10395
```


## What is the average daily activity pattern?

* Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
df[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), 
   by = .(interval)]-> activity_pattern

ggplot(activity_pattern, aes(x = interval , y = steps)) ->g1

g1 + 
     geom_line(color="red", size=1) + 
     labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Average Steps per day")+
     theme_igray()
```

![](Course-Project-1_-Reproducible-Research_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activity_pattern[steps == max(steps), .(maximun_interval = interval)]
```

```
##    maximun_interval
## 1:              835
```



## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
sapply(df, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```


* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
cat("replace missiong values with the median", sep = "\n")
```

```
## replace missiong values with the median
```

```r
df[,lapply(.SD,function(x){ifelse(is.na(x),median(df$steps, na.rm = T),x)})]
```

```
##        steps       date interval
##     1:     0 2012-10-01        0
##     2:     0 2012-10-01        5
##     3:     0 2012-10-01       10
##     4:     0 2012-10-01       15
##     5:     0 2012-10-01       20
##    ---                          
## 17564:     0 2012-11-30     2335
## 17565:     0 2012-11-30     2340
## 17566:     0 2012-11-30     2345
## 17567:     0 2012-11-30     2350
## 17568:     0 2012-11-30     2355
```

*Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
df[,lapply(.SD,function(x){ifelse(is.na(x),median(df$steps, na.rm = T),x)})]->df2
sapply(df2, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##        0        0        0
```


* Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
with(df2, aggregate(steps, by=list(date), FUN = sum, na.rm=T))->total_steps2
total_steps2$Group.1<-as.Date(total_steps2$Group.1,"%Y-%m-%d");head(total_steps2)
```

```
##      Group.1     x
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
names(total_steps2)<-c("Date","N_steps")

ggplot(total_steps2, aes(x = N_steps)) +
     geom_histogram(fill = "red", binwidth = 1000) +
     labs(title = "Steps by Date", x = "Steps", y = "Frequency")+
     theme_gdocs()
```

![](Course-Project-1_-Reproducible-Research_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
cat("Mean of Steps without NA", mean(total_steps2$N_steps, na.rm = T), sep = "\n")
```

```
## Mean of Steps without NA
## 9354.23
```

```r
cat("Median of Steps without NA", median(total_steps2$N_steps, na.rm = T), sep = "\n")
```

```
## Median of Steps without NA
## 10395
```

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
df[, date := as.POSIXct(date, format = "%Y-%m-%d")]
df[, `Day of Week`:= weekdays(x = date)]
df[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", 
         x = `Day of Week`), "weekday/weekend"] <- "weekday"
df[grepl(pattern = "Saturday|Sunday", 
         x = `Day of Week`), "weekday/weekend"] <- "weekend"
df[, `weekday or weekend` := as.factor(`weekday/weekend`)]
head(df, 10)
```

```
##     steps       date interval Day of Week weekday/weekend weekday or weekend
##  1:    NA 2012-10-01        0      Monday         weekday            weekday
##  2:    NA 2012-10-01        5      Monday         weekday            weekday
##  3:    NA 2012-10-01       10      Monday         weekday            weekday
##  4:    NA 2012-10-01       15      Monday         weekday            weekday
##  5:    NA 2012-10-01       20      Monday         weekday            weekday
##  6:    NA 2012-10-01       25      Monday         weekday            weekday
##  7:    NA 2012-10-01       30      Monday         weekday            weekday
##  8:    NA 2012-10-01       35      Monday         weekday            weekday
##  9:    NA 2012-10-01       40      Monday         weekday            weekday
## 10:    NA 2012-10-01       45      Monday         weekday            weekday
```

```r
df[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]->df[is.na(steps), "steps"]

df[, c(lapply(.SD, mean, na.rm = TRUE)), 
           .SDcols = c("steps"), 
           by = .(interval, `weekday/weekend`)] ->activity_pattern_diff 
head(df)
```

```
##    steps       date interval Day of Week weekday/weekend weekday or weekend
## 1:     0 2012-10-01        0      Monday         weekday            weekday
## 2:     0 2012-10-01        5      Monday         weekday            weekday
## 3:     0 2012-10-01       10      Monday         weekday            weekday
## 4:     0 2012-10-01       15      Monday         weekday            weekday
## 5:     0 2012-10-01       20      Monday         weekday            weekday
## 6:     0 2012-10-01       25      Monday         weekday            weekday
```

* Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ggplot(activity_pattern_diff , aes(x = interval , y = steps, color=`weekday/weekend`))->g2
g2 + 
     geom_line() + 
     labs(title = "Average for Daily Steps, by Week Type", x = "Intervals", y = "Number of Steps") + 
     facet_wrap(~`weekday/weekend` , ncol = 1, nrow=2)+
     theme_excel_new()
```

![](Course-Project-1_-Reproducible-Research_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

