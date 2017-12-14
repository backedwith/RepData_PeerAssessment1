Load Libraries

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.2

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.4.2

``` r
library(rmarkdown)
```

**Loading and preprocessing the data**

Load data

``` r
setwd("/Users/Anna/Desktop/Coursera Data Science")
```

Download and unzip file of dataset

``` r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile='activity.zip')
unzip(zipfile = "activity.zip")  
```

Read data

``` r
activitydata <- read.csv("activity.csv")
summary(activitydata)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

**What is mean total number of steps taken per day?**

Summerize data by steps per day

``` r
stepsbyday <- tapply(activitydata$steps, activitydata$date, sum, na.rm=TRUE)
```

Create Historgram

``` r
qplot(stepsbyday, xlab='Total steps per day', ylab='Frequency', binwidth=500)
```

![](README_figs/README-histogram1-1.png)

Calculate and report the MEAN and MEDIAN total number of steps taken per day

``` r
mean(stepsbyday) 
```

    ## [1] 9354.23

``` r
median(stepsbyday)
```

    ## [1] 10395

Mean: 9354.23 Median: 10395

\*\*What is the average daily activity pattern?\*

Make a TIME SERIES PLOT (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
averagestepspertimeblock <- aggregate(x=list(meansteps=activitydata$steps), by=list(interval=activitydata$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averagestepspertimeblock, aes(x=interval, y=meansteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![](README_figs/README-timeseriesplot-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
moststeps <- which.max(averagestepspertimeblock$meansteps)
timemoststeps <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averagestepspertimeblock[moststeps,'interval'])
print(timemoststeps)
```

    ## [1] "8:35"

**Imputing missing values**

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
missingvalues <- length(which(is.na(activitydata$steps)))
print(missingvalues)
```

    ## [1] 2304

Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
data_full <- activitydata
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
```

Make a HISTOGRAM of the total number of steps taken each day

``` r
total.steps <- tapply(data_full$steps, data_full$date, sum, na.rm=TRUE, simplify=T)
qplot(total.steps, xlab = "total number of steps taken each day", ylab = "count", binwidth=500)
```

![](README_figs/README-histogram2-1.png)

Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean

``` r
mean(total.steps)
```

    ## [1] 10766.19

``` r
median(total.steps)
```

    ## [1] 10766.19

mean(total.steps) \[1\] 10766

median(total.steps) \[1\] 10766

Mean and median values are higher after adding missing data.

**Are there differences in activity patterns between weekdays and weekends?**

Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
data_full$WeekendOrWeekday <- ifelse(weekdays(as.Date(data_full$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
```

Make a PANEL PLOT containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
new_dataset <- (data_full %>% group_by(interval, WeekendOrWeekday) %>% summarise(mean = mean(steps)))
ggplot(new_dataset, mapping = aes(x = interval, y = mean)) + geom_line() +
    facet_grid(WeekendOrWeekday ~.) + xlab("Interval") + ylab("Mean of Steps")
```

![](README_figs/README-panelplot-1.png)
