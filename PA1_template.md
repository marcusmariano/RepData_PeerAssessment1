# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Loading data


```r
data  <- read.csv("data/activity.csv", sep = ",", 
                  header = TRUE, stringsAsFactors = F)
```

Process and transform the data

```r
dat <- data
dat$date <- as.Date(dat$date, "%Y-%m-%d")
dat$steps[dat$steps < 1] <- NA
dat$interval[dat$interval < 1] <- NA
```

## What is mean total number of steps taken per day?

Total number of steps taken per day


```r
total_step_day <- tapply(dat$steps, dat$date, sum, na.rm = TRUE) 
    ##  total number of steps taken per day
```

Histogram of the total number of steps taken each day


```r
hist(total_step_day, breaks = 25, xlab="Total number of steps par day",
     main="Histogram of the total number of steps per day",
      xlim=c(0, 22000))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Mean of the total number of steps taken per day


```r
total_mean_day <- tapply(dat$steps, dat$date, mean, na.rm = TRUE)
total_mean_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        NaN   63.00000  140.14815  121.16000  154.58140  145.47170 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##  101.99074        NaN  134.85263   95.19231  137.38667  156.59459 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##  119.48077  160.61702  131.67532  157.12500  152.86364  152.36364 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##  127.19355  125.24096   96.93407  154.71264  101.34091  104.43750 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##   56.63636   77.02273  134.92000  110.17308   80.93548  110.32584 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##  179.23256        NaN  143.24324  117.45556        NaN  141.06757 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##  100.40964  135.61053   61.90385        NaN        NaN  132.71579 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##  156.01449   90.56790        NaN   20.50000   89.19672  183.83333 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##  162.47312  117.88000   95.14894  188.04412  177.62609  252.30952 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##  176.56098  140.88095  128.29885  158.67442  212.14583  110.10938 
## 2012-11-30 
##        NaN
```

Median of the total number of steps taken per day

```r
total_media_day <- tapply(dat$steps, dat$date, median, na.rm = TRUE)
total_media_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA       63.0       61.0       56.5       66.0       67.0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##       52.5         NA       48.0       56.5       35.0       46.0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##       45.5       60.5       54.0       64.0       61.5       52.5 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##       74.0       49.0       48.0       52.0       56.0       51.5 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       35.0       36.5       72.0       61.0       54.5       40.0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##       83.5         NA       55.5       59.0         NA       66.0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       52.0       58.0       42.5         NA         NA       55.0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##       42.0       57.0         NA       20.5       43.0       65.5 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##       80.0       34.0       58.0       55.0       65.0      113.0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##       65.5       84.0       53.0       57.0       70.0       44.5 
## 2012-11-30 
##         NA
```

## What is the average daily activity pattern?

Series plot of the 5-minute interval and the average number of steps

```r
interval_day <- tapply(dat$interval, dat$date, median, na.rm = TRUE)
datalist <- unique(dat$date)
in_st <- data.frame(date = datalist, steps = total_step_day, interval = interval_day)

plot(in_st$date, in_st$steps, type = "l", xlab = "5-minute interval per day", 
     xaxt = "n", ylab = "average number of steps", 
     main = "Series Plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

maximum number of steps on 5-minute interval


```r
max_step_interval <- tapply(dat$steps, dat$interval, mean, na.rm = TRUE)
max(max_step_interval, na.rm = TRUE)
```

```
## [1] 352.4839
```

```r
max_interval <- which(max_step_interval %in% max(max_step_interval, na.rm = TRUE))
max_step_interval[max_interval] ## maximum number of steps on 5-minute interval
```

```
##      835 
## 352.4839
```


## Imputing missing values

Total number of missing values in the dataset

```r
sum(is.na(dat))
```

```
## [1] 13379
```





