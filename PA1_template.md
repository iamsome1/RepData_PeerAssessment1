# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
clean_data<-data[complete.cases(data),] #Removing NA values
```


## What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day

```r
library(ggplot2) 
ggplot(clean_data, aes(date, steps)) + 
  geom_bar(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

* Calculate and report the mean and median total number of steps taken per day

```r
total_steps <- tapply(clean_data$steps, clean_data$date, FUN=sum)
mean(total_steps) 
```

```
## [1] 10766.19
```

```r
median(total_steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averages <- aggregate(clean_data$steps, list(interval = as.numeric(as.character(clean_data$interval))), FUN = "mean")

ggplot(averages, aes(interval, x)) + 
  geom_line(size = 0.8)+
  labs(title = "Time Series Plot", x = "5-minute interval", y = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averages[which.max(averages$x),]
```

```
##     interval        x
## 104      835 206.1698
```


## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* Create a new dataset that is equal to the original dataset but with the missing data filled in.

My strategy is to fill NA with mean of 5 minute interval

```r
fill_data <- data 
for (i in 1:nrow(fill_data)) {
    if (is.na(fill_data$steps[i])) {
        fill_data$steps[i] <- averages[which(fill_data$interval[i] == averages$interval), ]$x
    }
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 

```r
ggplot(fill_data, aes(date, steps)) + 
  geom_bar(stat = "identity") +  
  labs(x = "Date", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

* What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps2 <- aggregate(fill_data$steps, 
                           list(Date = fill_data$date), 
                           FUN = "sum")$x

mean_steps2<-mean(total_steps2)
median_step2<-median(total_steps2)
```

* Compare them before imputing missing data, there is no difference in mean after imputing missing data and new median value is greater after imputing missing data.

```r
mean_step1<-mean(total_steps)
median_step1<-median(total_steps)
mean_steps2-mean_step1
```

```
## [1] 0
```

```r
median_step2-median_step1
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
fill_data$weekdays <- factor(format(fill_data$date, "%A"))
levels(fill_data$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(fill_data$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
averages <- aggregate(fill_data$steps, 
                      list(interval = as.numeric(as.character(fill_data$interval)), 
                           weekdays = fill_data$weekdays),
                      FUN = "mean")
ggplot(averages, aes(interval, x)) + geom_line() + facet_grid(weekdays ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
