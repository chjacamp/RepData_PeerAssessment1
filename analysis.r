require(dplyr)
require(ggplot2)
require(lubridate)
  
## Loading and preprocessing the data
  
unzip("activity.zip", exdir="data")
activity_data <- read.csv("data/activity.csv")

activity_data$date <- ymd(activity_data$date)

## What is mean total number of steps taken per day?

steps_per_day <- activity_data %>%
  tbl_df %>%
  mutate(steps = as.numeric(as.character(steps))) %>%
  group_by(date) %>%
  mutate(total_steps = sum(steps)) %>%
  summarize(total_steps_mean=mean(total_steps), 
            total_steps_median=median(total_steps))

par(mfrow = c(1,2))
hist(steps_per_day$total_steps_mean, breaks=7,
     main = "Mean Total Steps per Day", 
     xlab = "total mean steps/day")

hist(steps_per_day$total_steps_median, breaks=7,
     main = "Median Total Steps per Day",
     xlab = "total median steps/day")


## What is the average daily activity pattern?

steps_per_interval <- activity_data %>%
  tbl_df %>%
  group_by(interval) %>%
  summarize(steps2 = mean(steps, na.rm=TRUE))

par(mfrow =c(1,1))
with(steps_per_interval, plot(interval, steps2, type="l"))

##I've never used the which.max function, but how handy is this!
##It gives us the index of the interval w/the largest steps. Rad!
largest_interval <- which.max(steps_per_interval$steps2)
steps_per_interval$interval[largest_interval]


## Imputing missing values
dim_activity_data <- dim(activity_data)
how_many_na <- sum(is.na(activity_data$steps))

percentage_na <- how_many_na/dim_activity_data[1]

##just in case I mess up  
activity_data_na <- activity_data

na_replace <- activity_data_na %>%
  group_by(date) %>%
  summarize(steps_mean=mean(steps))

date_for_mean <- ymd("2012-10-01")
grab <- as.integer(0)
j <- as.integer(0)

for (i in 1:length(activity_data$steps)) {

  all_means <- mean(na_replace$steps_mean, na.rm=TRUE)
  date_for_mean <- ymd(activity_data[i,2])
  grab <- which(na_replace$date == ymd(date_for_mean))
  
    if (is.na(activity_data[i,1] && !is.na(na_replace[date_for_mean,2]))) {
      
      activity_data[i,1] <- na_replace[grab,2]
    }
    
    else if (is.na(activity_data[i,1]) && is.na(na_replace[date_for_mean,2])) {
      activity_data[i,1] <- all_means
    }
    
    else if (!is.na(activity_data[i,1])) {
      j <- j+1
    }
  
  i <- i+1
}

index <- which(is.na(activity_data_na$steps))
activity_data_na[index,1] <- na_replace

## Are there differences in activity patterns between weekdays and weekends?

activity_data$steps2 <- activity_data$steps
ind <- is.na(activity_data$steps2) # find where the missing values are
ints <- activity_data$date[ind] # find to what intervals the missing values belong to
activity_data$steps2[ind] <- na_replace$steps_mean[ints] # substitute the missing values with the average for that interval
