# Reproducible Research: Peer Assessment 1
Victor Salit  


## Loading and preprocessing the data

```r
data <- read.csv(unz("activity.zip", "activity.csv"),stringsAsFactors = FALSE)
```


```r
data$date <- as.Date(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?

```r
completedata <- data[complete.cases(data$step),]
summary(completedata)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-29   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0
```

```r
head(completedata)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
tail(completedata)
```

```
##       steps       date interval
## 17275     0 2012-11-29     2330
## 17276     0 2012-11-29     2335
## 17277     0 2012-11-29     2340
## 17278     0 2012-11-29     2345
## 17279     0 2012-11-29     2350
## 17280     0 2012-11-29     2355
```

```r
by1 <- factor(completedata$date)
total_steps <- aggregate(completedata$steps, by=list(by1), sum, simplify = TRUE)
names(total_steps) <- c("date", "steps")
hist(total_steps$steps, col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
meansteps <- mean(total_steps$steps)
mediansteps <- median(total_steps$steps)
```
The mean total number of steps taken per day is 1.0766189\times 10^{4}.  
The median total number of steps taken per day is 10765.

## What is the average daily activity pattern?

```r
by2 <- factor(completedata$interval)
averageactivity <- aggregate(completedata$steps, by=list(by2),mean,simplify=TRUE)
names(averageactivity) <- c("interval", "mean_steps")
plot(as.character(averageactivity$interval),averageactivity$mean_steps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
maxinterval <- averageactivity$interval[which.max(averageactivity$mean_steps)]
```
On average across all the days in the dataset the interval 835 contains the maximum number of steps.

## Imputing missing values

```r
NAs <- sum(is.na(data$steps))
```

The total number of missing values in the dataset is 2304.  
Strategy for filling the missing data: interval mean across all days:


```r
filled <- data

index <- match(data$interval[is.na(data$steps)], averageactivity$interval)
filled$steps[is.na(data$steps)] <- averageactivity[index,2]
```



## Are there differences in activity patterns between weekdays and weekends?
