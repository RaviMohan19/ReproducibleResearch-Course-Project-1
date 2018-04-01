# Course Project 1: Reproducible Research
#########################################################

## Author: Ravi M.B

#########################################################


# Goal 1: Loading and Processing data


setwd("~/R/Reproducible_Research/Week2/CourseProject")
data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
data_activity <- "step_data.zip"
download.file(data_url, data_activity)
unzip(data_activity)
read_activity <- read.csv("activity.csv", sep = ",")
dim(read_activity)
names(read_activity)
head(read_activity)
tail(read_activity)
str(read_activity)

# Evaluating mean total number of steps taken per day, Missing values ignored

read_activity$date <- as.Date(read_activity$date)
clear_na_data <- subset(read_activity, !is.na(read_activity$steps))
total_steps_sum <-
  tapply(
    clear_na_data$steps,
    clear_na_data$date,
    sum,
    na.rm = TRUE,
    simplify = T
  )
total_steps_sum <- total_steps_sum[!is.na(total_steps_sum)]

# histogram of the total number of steps taken each day

hist(x=total_steps_sum,
     col="red",
     breaks=10,
     xlab="Daily steps",
     ylab="Frequency",
     main="Total Number of Steps per Day neglecting missing data")

# mean and median of the total number of steps taken per day

mean(total_steps_sum)
median(total_steps_sum)

# Goal2: Average daily activity pattern

avg_daily <- tapply(clear_na_data$steps, clear_na_data$interval, mean, na.rm=TRUE, simplify=T)
avg_daily_interval <- data.frame(interval=as.integer(names(avg_daily)), avg=avg_daily)
with(avg_daily_interval,
     plot(interval,
          avg,
          type="l",
          xlab="5-minute intervals",
          ylab="average number of steps"))

# check which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps

max_num_steps <- max(avg_daily_interval$avg)
avg_daily_interval[avg_daily_interval$avg == max_num_steps, ]

# Goal 3:Imputing missing values

# Calculate and report the total number of missing values in the dataset

nrow(read_activity[is.na(read_activity$steps),])

# impute the missing values

impute_miss <- read_activity
na_data <- is.na(impute_miss$steps)
clear_na_avg <- tapply(clear_na_data$steps, clear_na_data$interval, mean, na.rm=TRUE, simplify=T)
impute_miss$steps[na_data] <- clear_na_avg[as.character(impute_miss$interval[na_data])]

# making a histogram  total number of steps taken each day
# Calculate and report the mean and median total number of steps taken per day

new_data <- tapply(impute_miss$steps, impute_miss$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_data,
     col="blue",
     breaks=10,
     xlab="Daily steps",
     ylab="Frequency",
     main="Total number of steps(comparison with missing data imputed)")

hist(x=total_steps_sum,
     col="red",
     breaks=10,
     xlab="Daily steps",
     ylab="Frequency",
     main="Total number of steps(comparison with missing data imputed)",
     add=T)

legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("blue", "red") )

mean(new_data)
median(new_data)

# Goal 4: Evaluating if there are any differences in activity patterns 
# between weekdays and weekends?

check_day <- function(days) {
  wd <- weekdays(days)
  ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

test_data <- sapply(impute_miss$date, check_day)
impute_miss$week <- as.factor(test_data)
dim(impute_miss)
head(impute_miss)
tail(impute_miss)
week_data <- aggregate(steps ~ week+interval, data=impute_miss, FUN=mean)

# plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis)
library(lattice)

xyplot(steps ~ interval | factor(week),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=week_data)

