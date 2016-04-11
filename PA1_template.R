## load the data

activity <- read.csv("activity.csv")

## Calculate the total number of steps taken per day
totalsteps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)

## Make a histogram of the total number of steps taken each day
hist(totalsteps, col="blue")

## Calculate and report the mean and median of the total number of steps taken per day
meantotsteps<- mean(totalsteps, na.rm=TRUE)
medtotalsteps<- median(totalsteps, na.rm=TRUE)

##-----------------------------------------------------------------------------------------

## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)
## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?

library(ggplot2)
avg <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)

ggplot(data=avg,aes(x=interval,y=steps))+geom_line()+xlab("5 Minute Inverval")+ylab("Avg # of Steps Taken")

## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
avg[which.max(avg$steps),]

## returns answer of 
##     interval    steps
#104      835 206.1698

## --------------------------------------
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(activity))
## returns
## [1] 2304

fillvalues <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps) else filled <- (avg[avg$interval == interval, "steps"])
return(filled)}

filleddata <- activity
filleddata$steps <- mapply(fillvalues, filleddata$steps, filleddata$interval)

totalsteps2 <- tapply(filleddata$steps, filleddata$date, FUN = sum)

hist(totalsteps2, col = "grey")

## Make a histogram of the total number of steps taken each day 
## and Calculate and report the mean and median total number of steps taken 
## per day. Do these values differ from the estimates from the first part 
## of the assignment? 
## What is the impact of imputing missing data on the estimates 
## of the total daily number of steps?

mean(totalsteps2)
## [1] 10766.19
median (totalsteps2)
##[1] 10766.19

## values are different from original set as we have replaced the default zero
## values with real values

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
## indicating whether a given date is a weekday or weekend day.

weekdayorend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
    return("weekday") else if (day %in% c("Saturday", "Sunday")) 
      return("weekend") else stop("invalid date")
}
filleddata$date <- as.Date(filleddata$date)
filleddata$day <- sapply(filleddata$date, FUN = weekdayorend)

##--------------------------------------------------------------------------------------------
avg2 <- aggregate(steps ~ interval + day, data = filleddata, mean)
ggplot(avg2, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
  xlab("5-minute interval") + ylab("Number of steps")



