
### set work directory 
setwd("/Users/jwang/Documents/Coursera/reproresearch")


### Loading and preprocessing the data
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
str(data) 

### What is mean total number of steps taken per day?
total_steps <- tapply(data$steps, data$date,sum,na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken per day", ylab="frequency")
mean(total_steps)
median(total_steps)

### What is the average daily activity pattern?
install.packages("ggplot2")
library(ggplot2)

average_steps <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval),mean, na.rm=TRUE)
ggplot(data=average_steps, aes(x=interval, y=steps)) + geom_line() + xlab("interval") + ylab("number of steps")
maximum_steps <- averages[which.max(average_steps$steps),]
maximum_steps 

###  Imputing missing values
# total missing numbers 
table(is.na(data$steps))

# Replace each missing value with the mean value of its 5-minute interval
fill_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[average_steps$interval==interval, "steps"])
  return(filled)
}
filled_data <- data
filled_data$steps <- mapply(fill_value, filled_data$steps, filled_data$interval)
table(is.na(filled_data$steps))
 
## update the mean and median 
total_steps <- tapply(filled_data$steps, filled_data$date, sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken per day")
mean(total_steps)
median(total_steps)

### Are there differences in activity patterns between weekdays and weekends?
weekday_or_weekend <- function(which_date) {
  day <- weekdays(which_date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled_data$date <- as.Date(filled_data$date)
filled_data$day <- sapply(filled_data$date,weekday_or_weekend)

## ------------------------------------------------------------------------
average_steps <- aggregate(steps ~ interval + day, data=filled_data, mean)
ggplot(average_steps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")

