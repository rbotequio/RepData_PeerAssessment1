Loading and preprocessing the data
----------------------------------

    setwd("~/Desktop/data/RDPA")
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
    unzip(zipfile="./activity.zip")
    Acty <- read.csv("activity.csv")

Transform date in Date format:

    Acty$date <- as.Date(Acty$date) 
    head(Acty)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Read Libraries

    library(data.table)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lattice)
    library(tidyr)

What is mean total number of steps taken per day?
-------------------------------------------------

Calculate Total Steps per Day

    TStepsDay <- aggregate(steps ~ date, Acty, FUN = sum)

![](PA1_template_files/figure-markdown_strict/Hist%20Total%20per%20day-1.png)

Mean and Median per day:

    MeanSteps <- mean(TStepsDay$steps)
    MeanSteps

    ## [1] 10766.19

    MedianSteps <- median(TStepsDay$steps)
    MedianSteps

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Calculate the mean of steps per interval of 5 minutes:

    aDayInterPat <- Acty %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE))
    head(aDayInterPat)

    ## # A tibble: 6 x 2
    ##   interval  steps
    ##      <int>  <dbl>
    ## 1        0 1.72  
    ## 2        5 0.340 
    ## 3       10 0.132 
    ## 4       15 0.151 
    ## 5       20 0.0755
    ## 6       25 2.09

    plot(aDayInterPat$interval, aDayInterPat$steps, type = "l", xlab = "5 minutes interval Day", ylab = "Average of Steps in All Days per interval")

![](PA1_template_files/figure-markdown_strict/Mean%20per%20interval-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    fMAxintMean <- aDayInterPat %>% filter(steps == max(steps,  na.rm = TRUE))
    fMAxintMean$interval

    ## [1] 835

Imputing missing values
-----------------------

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

    TotalNAStpes <- sum(is.na(Acty$steps))
    TotalNAStpes

    ## [1] 2304

Add New Column with complete Stpes by Median Interval per DaY

    Acty$CompleteSteps <- ifelse(is.na(Acty$steps), round(aDayInterPat$steps[match(Acty$interval, aDayInterPat$interval)],0), Acty$steps)
    head(Acty)

    ##   steps       date interval CompleteSteps
    ## 1    NA 2012-10-01        0             2
    ## 2    NA 2012-10-01        5             0
    ## 3    NA 2012-10-01       10             0
    ## 4    NA 2012-10-01       15             0
    ## 5    NA 2012-10-01       20             0
    ## 6    NA 2012-10-01       25             2

\#Create a new dataset that is equal to the original dataset but with
the missing data filled in.

    ActyFull <- data.frame(date  = Acty$date, interval = Acty$interval, steps = Acty$CompleteSteps)
    head(ActyFull)

    ##         date interval steps
    ## 1 2012-10-01        0     2
    ## 2 2012-10-01        5     0
    ## 3 2012-10-01       10     0
    ## 4 2012-10-01       15     0
    ## 5 2012-10-01       20     0
    ## 6 2012-10-01       25     2

\#Make a histogram of the total number of steps taken each day

    TStepsDayF <- aggregate(steps ~ date, ActyFull, FUN = sum)
    hist(TStepsDayF$steps, col = "blue", xlab = "Total Steps per Day", main = "Histogram Total Steps per Day")

![](PA1_template_files/figure-markdown_strict/Total%20perday%20Full-1.png)

\#Calculate and report the mean and median total number of steps taken
per day.

    MeanStepsF <- mean(TStepsDayF$steps)
    MeanStepsF

    ## [1] 10765.64

    MedianStepsF <- median(TStepsDayF$steps)
    MedianStepsF

    ## [1] 10762

\#Do these values differ from the estimates from the first part of the
assignment?

    DifMedianSteps <- round((MedianSteps-MedianStepsF)/MedianStepsF, 5)
    DifMedianSteps

    ## [1] 0.00028

    DifMedianSteps <- round((MeanSteps-MeanStepsF)/MeanStepsF, 10)
    DifMedianSteps

    ## [1] 5.10267e-05

\#What is the impact of imputing missing data on the estimates of the
total daily number of steps?

    par(mfrow = c(1, 2))
    hist(TStepsDayF$steps, col = "blue", xlab = "Total Steps per Day", main = "Total Steps with NA")
    hist(TStepsDay$steps, col = "red", xlab = "Total Steps per Day", main = "Total Steps Filled")

![](PA1_template_files/figure-markdown_strict/Two%20hist-1.png)

    summary(TStepsDay)

    ##       date                steps      
    ##  Min.   :2012-10-02   Min.   :   41  
    ##  1st Qu.:2012-10-16   1st Qu.: 8841  
    ##  Median :2012-10-29   Median :10765  
    ##  Mean   :2012-10-30   Mean   :10766  
    ##  3rd Qu.:2012-11-16   3rd Qu.:13294  
    ##  Max.   :2012-11-29   Max.   :21194

    summary(TStepsDayF)

    ##       date                steps      
    ##  Min.   :2012-10-01   Min.   :   41  
    ##  1st Qu.:2012-10-16   1st Qu.: 9819  
    ##  Median :2012-10-31   Median :10762  
    ##  Mean   :2012-10-31   Mean   :10766  
    ##  3rd Qu.:2012-11-15   3rd Qu.:12811  
    ##  Max.   :2012-11-30   Max.   :21194

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    ActyFull$tDay <- ifelse(weekdays(ActyFull$date) == "Sábado" | weekdays(ActyFull$date, abbreviate = TRUE) == "Domingo", "weekend", "weekday")
    head(ActyFull)

    ##         date interval steps    tDay
    ## 1 2012-10-01        0     2 weekday
    ## 2 2012-10-01        5     0 weekday
    ## 3 2012-10-01       10     0 weekday
    ## 4 2012-10-01       15     0 weekday
    ## 5 2012-10-01       20     0 weekday
    ## 6 2012-10-01       25     2 weekday

1.  Make a panel plot containing a time series plot (i.e. type = “l”
    type = “l”) of the 5-minute interval (x-axis) and the average number
    of steps taken, averaged across all weekday days or weekend days
    (y-axis).

<!-- -->

    Meaninterweek <- ActyFull %>% group_by(interval, tDay) %>% summarise(steps = mean(steps, na.rm = TRUE))

    xyplot(steps ~ interval | tDay, Meaninterweek, type = "l", layout = c(1, 2), 
           xlab = "Interval", ylab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/PLotw-1.png)
