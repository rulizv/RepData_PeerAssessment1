# PA1_template
Raul Zambrano  
April 18, 2015  

##What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
data <- read.csv(unz("activity.zip", "activity.csv"),stringsAsFactors=FALSE)
data$date=as.POSIXct(data$date)
perDayTotals=aggregate(list(totalStepsPerDay = data[,1]),by=list(day=cut(data$date,"1 day")),sum)
res.mean = mean(perDayTotals$totalStepsPerDay,na.rm = TRUE)
res.median = median(perDayTotals$totalStepsPerDay,na.rm = TRUE)
perDayTotals
```

```
##           day totalStepsPerDay
## 1  2012-10-01               NA
## 2  2012-10-02              126
## 3  2012-10-03            11352
## 4  2012-10-04            12116
## 5  2012-10-05            13294
## 6  2012-10-06            15420
## 7  2012-10-07            11015
## 8  2012-10-08               NA
## 9  2012-10-09            12811
## 10 2012-10-10             9900
## 11 2012-10-11            10304
## 12 2012-10-12            17382
## 13 2012-10-13            12426
## 14 2012-10-14            15098
## 15 2012-10-15            10139
## 16 2012-10-16            15084
## 17 2012-10-17            13452
## 18 2012-10-18            10056
## 19 2012-10-19            11829
## 20 2012-10-20            10395
## 21 2012-10-21             8821
## 22 2012-10-22            13460
## 23 2012-10-23             8918
## 24 2012-10-24             8355
## 25 2012-10-25             2492
## 26 2012-10-26             6778
## 27 2012-10-27            10119
## 28 2012-10-28            11458
## 29 2012-10-29             5018
## 30 2012-10-30             9819
## 31 2012-10-31            15414
## 32 2012-11-01               NA
## 33 2012-11-02            10600
## 34 2012-11-03            10571
## 35 2012-11-04               NA
## 36 2012-11-05            10439
## 37 2012-11-06             8334
## 38 2012-11-07            12883
## 39 2012-11-08             3219
## 40 2012-11-09               NA
## 41 2012-11-10               NA
## 42 2012-11-11            12608
## 43 2012-11-12            10765
## 44 2012-11-13             7336
## 45 2012-11-14               NA
## 46 2012-11-15               41
## 47 2012-11-16             5441
## 48 2012-11-17            14339
## 49 2012-11-18            15110
## 50 2012-11-19             8841
## 51 2012-11-20             4472
## 52 2012-11-21            12787
## 53 2012-11-22            20427
## 54 2012-11-23            21194
## 55 2012-11-24            14478
## 56 2012-11-25            11834
## 57 2012-11-26            11162
## 58 2012-11-27            13646
## 59 2012-11-28            10183
## 60 2012-11-29             7047
## 61 2012-11-30               NA
```

2. Make a histogram of the total number of steps taken each day

![](ActivityMonitoing_files/figure-html/unnamed-chunk-2-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

Mean: 10766.19   

Median: 10765  

##What is the average daily activity pattern?


```r
avgIntervals=aggregate(list(averageStepsPerInterval = data[,1]),by=list(interval=data$interval),mean,na.rm=TRUE)
avgIntervals$interval=paste(substr(sprintf('%04d',avgIntervals$interval),1,2),':',substr(sprintf('%04d',avgIntervals$interval),3,4))
avgIntervals$interval=strptime(avgIntervals$interval,'%H : %M')
avgMax=which.max(x = avgIntervals$averageStepsPerInterval)
```

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

![](ActivityMonitoing_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5-minute interval with the maximum number of steps is 08:35 with 206.1698113 steps.

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
emptyRows=is.na(data$steps)
```

The number of missing values is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy is to reeplace the NA values with the average value for that specific interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
dataNoNas=data
allIntervals=unique(data$interval)
#Replace the NA values with the rounded average value of the time period
#dataNoNas[emptyRows,1]=round(avgIntervals[which(allIntervals==dataNoNas[emptyRows,3]),2],0)
emptyInts=dataNoNas[emptyRows,3]
for (i in 1:length(emptyInts)) {
    j = which(allIntervals==emptyInts[i])
    emptyInts[i]=round(avgIntervals[j,2],0)
}
dataNoNas[emptyRows,1]=emptyInts
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
perDayTotals2=aggregate(list(totalStepsPerDay = dataNoNas[,1]),by=list(day=cut(dataNoNas$date,"1 day")),sum)
res2.mean = mean(perDayTotals2$totalStepsPerDay,na.rm = TRUE)
res2.median = median(perDayTotals2$totalStepsPerDay,na.rm = TRUE)
```

![](ActivityMonitoing_files/figure-html/unnamed-chunk-8-1.png) 

Mean: 10765.64   

Median: 10762.00  

The total number of steps increases, however the average and means values are altered relatively less.

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dataNoNas[,4]=weekdays(dataNoNas$date) %in% c('Saturday','Sunday')
names(dataNoNas)[4]='Weekend'
dataNoNas=transform(dataNoNas,Weekend = ifelse(Weekend,'Weekend','Weekday'))
dataNoNas$Weekend=factor(dataNoNas$Weekend)
avgIntervals2=aggregate(list(averageStepsPerInterval = dataNoNas[,1]),by=list(interval=dataNoNas$interval,typeOfDay=dataNoNas$Weekend),mean,na.rm=TRUE)
avgIntervals2$interval=paste(substr(sprintf('%04d',avgIntervals2$interval),1,2),':',substr(sprintf('%04d',avgIntervals2$interval),3,4))
avgIntervals2$interval=strptime(avgIntervals2$interval,'%H : %M')
avgIntervalsWeekdays=avgIntervals2[avgIntervals2$typeOfDay=='Weekday',]
avgIntervalsWeekends=avgIntervals2[avgIntervals2$typeOfDay=='Weekend',]
```

![](ActivityMonitoing_files/figure-html/unnamed-chunk-10-1.png) 

