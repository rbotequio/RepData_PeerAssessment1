#####set workdirectory

setwd("~/Desktop/data/RDPA")

####### Loading and preprocessing the data #####

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
unzip(zipfile="./activity.zip")
Acty <- read.csv("activity.csv")
#convert Date to Date format 
Acty$date <- as.Date(Acty$date) 


#Read Labraries
library(data.table)
library(dplyr)
library(lattice)
library(tidyr)

#Calculate Total Steps per Day

TStepsDay <- aggregate(steps ~ date, Acty, FUN = sum)

#Histogram of Total Steps per Day

hist(TStepsDay$steps, col = "red", xlab = "Total Steps per Day")

MeanSteps <- mean(TStepsDay$steps)
MedianSteps <- median(TStepsDay$steps)

#What is the average daily activity pattern?

aDayInterPat <- Acty %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE))

plot(aDayInterPat$interval, aDayInterPat$steps, type = "l", xlab = "5 minutes interval Day", ylab = "Average of Steps in All Days per interval")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

#Interval in Median per day with maximum Stpes
fMAxintMean <- aDayInterPat %>% filter(steps == max(steps,  na.rm = TRUE))


#### Imputing missing values

TotalNAStpes <- sum(is.na(Acty$steps))

#### New Column with complete Stpes by Median Interval per DaY

Acty$CompleteSteps <- ifelse(is.na(Acty$steps), round(aDayInterPat$steps[match(Acty$interval, aDayInterPat$interval)],0), Acty$steps)

####Create a new dataset that is equal to the original dataset but with the missing data filled in.

ActyFull <- data.frame(date  = Acty$date, interval = Acty$interval, steps = Acty$CompleteSteps)

#Make a histogram of the total number of steps taken each day 

TStepsDayF <- aggregate(steps ~ date, ActyFull, FUN = sum)

hist(TStepsDayF$steps, col = "blue", xlab = "Total Steps per Day")

#Calculate and report the mean and median total number of steps taken per day. 

MeanStepsF <- mean(TStepsDayF$steps)

MedianStepsF <- median(TStepsDayF$steps)

#Do these values differ from the estimates from the first part of the assignment? 

DifMedianSteps <- round((MedianSteps-MedianStepsF)/MedianStepsF, 5)

DifMeanSteps <- round((MeanSteps-MeanStepsF)/MeanStepsF, 10)


#What is the impact of imputing missing data on the estimates of the total daily number of steps?
par(mfrow = c(1, 2))
hist(TStepsDayF$steps, col = "blue", xlab = "Total Steps per Day", main = "Total Steps with NA")
hist(TStepsDay$steps, col = "red", xlab = "Total Steps per Day", main = "Total Steps Filled")

summary(TStepsDay)
summary(TStepsDayF)


# Are there differences in activity patterns between weekdays and weekends?

ActyFull$tDay <- ifelse(weekdays(ActyFull$date) == "Sábado" | weekdays(ActyFull$date, abbreviate = TRUE) == "Domingo", "weekend", "weekday")
Meaninterweek <- ActyFull %>% group_by(interval, tDay) %>% summarise(steps = mean(steps, na.rm = TRUE))
    
xyplot(steps ~ interval | tDay, Meaninterweek, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")

