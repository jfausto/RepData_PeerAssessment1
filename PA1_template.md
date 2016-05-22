Reproducible Research -W2- Assignment
================

### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
-   **date**: The date on which the measurement was taken in YYYY-MM-DD format
-   **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### 1. Code for reading in the dataset and/or processing the data

Once unzip and stored in the working directory, it is loaded into a dataframe

``` r
activity <- read.csv("activity.csv")
```

The dataframe looks like as follows

``` r
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

We want to transform *date* column in a "Date"

``` r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### 2. Histogram of the total number of steps taken each day

First, we aggregate the steps by day

``` r
actbyDate <-  aggregate(steps ~ date, activity, sum)
```

The Histogram is plotted

``` r
with(actbyDate,hist(steps,col="light blue",breaks=10,main="# of Steps per Day",xlab="# of steps"))
```

![](PA1_template_files/figure-markdown_github/plot%20stepsbyday-1.png)<!-- -->

Note: with more breaks the frequency values decrease. With a value of 10, the plot resembles a normal distribution centered around 11000 steps as we will see in the following section

### 3. Mean and median number of steps taken each day

For this calculation, NAs are not considered

``` r
mediansteps <- median(actbyDate$steps,na.rm=TRUE)
avgsteps <- mean(actbyDate$steps,na.rm=TRUE)
```

The median is **10765** and the mean (or average) is **10766.19**

### 4. Time series plot of the average number of steps taken

We considered the average of the steps by interval for all days

``` r
avgActbyInterval <- aggregate(steps ~ interval, activity, mean,na.rm=TRUE)
```

The value of average steps is plotted during the time period

``` r
library(ggplot2)
ggplot(avgActbyInterval,aes(x=interval,y=steps,group=0)) +
    geom_line(col="light blue", size=1) + 
    labs(title="Avg # of steps by interval",x="Daily interval",y="Avg # of steps") + 
    theme_bw()
```

![](PA1_template_files/figure-markdown_github/plot%20time%20average-1.png)<!-- -->

### 5. The 5-minute interval that, on average, contains the maximun number of steps

The maximun number of steps

``` r
maxnumberofsteps <- round(max(avgActbyInterval$steps),digits=0)
intervalmax <- avgActbyInterval[which.max(avgActbyInterval$steps),]
```

The interval **835** has the maximum average of number of steps: **206**

### 6. Code to describe and show a strategy for imputing missing data

The number of NAs:

``` r
sum(is.na(activity))
```

    ## [1] 2304

It can be seen that all NAs are in *steps* column

``` r
summary(activity)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

NAs are going to be filled with the mean for this interval of the rest of the days. Dataframe: avgActbyInterval

``` r
library(data.table)
activityFilled <- activity

# Merge with the lookup df (avgActbyInternal)
activityFilled <- merge(avgActbyInterval,activityFilled, by="interval")

#filling NAs with the reference values
for(i in 1:nrow(activityFilled)) {if (is.na(activityFilled[i,]$steps.y)) {activityFilled[i,]$steps.y = activityFilled[i,]$steps.x }}

#Removing the extra column
activityFilled <- subset(activityFilled,select=-steps.x)
#renaming the steps.y column back to steps
colnames(activityFilled)<-c("interval","steps","date")


summary(activityFilled)
```

    ##     interval          steps             date           
    ##  Min.   :   0.0   Min.   :  0.00   Min.   :2012-10-01  
    ##  1st Qu.: 588.8   1st Qu.:  0.00   1st Qu.:2012-10-16  
    ##  Median :1177.5   Median :  0.00   Median :2012-10-31  
    ##  Mean   :1177.5   Mean   : 37.38   Mean   :2012-10-31  
    ##  3rd Qu.:1766.2   3rd Qu.: 27.00   3rd Qu.:2012-11-15  
    ##  Max.   :2355.0   Max.   :806.00   Max.   :2012-11-30

As it can be seen from the summary, there is no NAs left in the new dataframe **activityFilled**

### 7. Histogram of the total number of steps taken each day after missing values are imputed

In order to plot the new dataframe, we aggregate by date

``` r
actFilledbyDate <-  aggregate(steps ~ date, activityFilled, sum)
```

The Histogram is plotted

``` r
with(actFilledbyDate,hist(steps,col="blue",breaks=10,main="# of Steps per Day with NAs imputed",xlab="# of steps"))
```

![](PA1_template_files/figure-markdown_github/plot%20stepsbyday%20noNA-1.png)<!-- -->

The median and the mean (average) of this new dataframe without NAs is calculated similarly

``` r
medianstepsF <- median(actFilledbyDate$steps,na.rm=TRUE)
avgstepsF <- mean(actFilledbyDate$steps,na.rm=TRUE)
```

Now, The median is **10766.19** and the mean (or average) is **10766.19**

Obviously, since we have imputed the NAs based on the mean (average) for each interval, the new mean is exactly the same that for the dataframe with NAs. And now the median matches the mean.

1.  With NAs

-   Median: **10765**
-   Mean: **10766.19**

1.  Without NAs

-   Median: **10766.19**
-   Mean: **10766.19**

#### *What is the impact of imputing missing data on the estimates of the total daily number of steps?*

In terms of total number of steps:
- Original: **570608**
- NA's filled: **656737.5**
- Diff: **13.11**%

There is an increment of the 13% on the total number of steps by filling the NAs with the average values in each interval.
In any case, creating values out of the blue for a given set of data collected must be very carefully done and from my point of view, the first choice to remove NAs would be transform them into 0. If we know how the data was collected and *it is known the source of NAs* we can make some assumptions, but if not there could be a bias in our analysis to the results *we want to achieve* instead of extracting information from the data itself.
One thing is to make predictions or extrapolations about the future based on data on the past and a very different thing is to change the past to accomadate the set of data to ease statistical calculation purposes.

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

A new factor variable, **dayofweek**, is created to store if a given day is part of the weekday or it is part of the weekend instead.

``` r
# My locale is in Spanish and R Studio doesn't allow me to change Sys.setlocale("LC_TIME", "en_US") or other similar locale value
# weekend <- c("Saturday","Sunday")

weekend <- c("sábado","domingo")

dayofweek <- weekdays(activityFilled$date)

for (i in 1:length(dayofweek)){
    if (dayofweek[i] %in% weekend){
        dayofweek[i] = "weekend"
        } else {
        dayofweek[i] = "weekday"
        }
}

# Append the vector to the dataframe
activityFilledWeek <- cbind(activityFilled,dayofweek)

str(activityFilledWeek)
```

    ## 'data.frame':    17568 obs. of  4 variables:
    ##  $ interval : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ steps    : num  1.72 0 0 0 0 ...
    ##  $ date     : Date, format: "2012-10-01" "2012-11-23" ...
    ##  $ dayofweek: Factor w/ 2 levels "weekday","weekend": 1 1 2 1 2 1 2 1 1 2 ...

To compare weekends and weekdays

``` r
#Averages are going to be calculated separately. First subsetting
actFilledweekday <- subset(activityFilledWeek, dayofweek=="weekday")
actFilledweekend <- subset(activityFilledWeek, dayofweek=="weekend")

#Average calculation
avgActFilledweekdaybyInterval <- aggregate(steps ~ interval, actFilledweekday, mean,na.rm=TRUE)
#Dayofweek=weekday is added
dayofweek<-"weekday"
avgActFilledweekdaybyInterval <- cbind(avgActFilledweekdaybyInterval,dayofweek)

avgActFilledweekendbyInterval <- aggregate(steps ~ interval, actFilledweekend, mean,na.rm=TRUE)
#Dayofweek=weekend is added
dayofweek <- "weekend"
avgActFilledweekendbyInterval <- cbind(avgActFilledweekendbyInterval,dayofweek)

#The 2 dataframes are merged together
avgActFilledbyInterval <-rbind(avgActFilledweekdaybyInterval,avgActFilledweekendbyInterval)
```

Now, the panel with the comparisons between weekday and weekend is displayed using ggplot2.

``` r
ggplot(avgActFilledbyInterval, aes(x=interval, y=steps,color=dayofweek)) + 
        geom_line() +
        facet_wrap(~dayofweek, nrow=2, ncol=1) +
        ggtitle("Avg # of Steps comparison - Weekday vs. Weekend")+
        labs(x="Interval", y="Avg # of Steps") +
        theme_bw()+theme(legend.position="none") #no background and legend
```

![](PA1_template_files/figure-markdown_github/draw%20panel-1.png)<!-- -->

By the previous graphs, it can be understood that the activity within the weekends appears more distrubited whereas in weekdays has a significant peak before the interval 1000th, around 850th interval. Since it may happen in the morning, perhaps it represents the activity associated to commute to work. Evenings in weekdays appears also more distributed in the intervals 1500th to 2000th.
