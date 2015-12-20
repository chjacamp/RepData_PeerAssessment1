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
  group_by(day(date)) %>%
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

replace_na <- activity_data %>%
  tbl_df %>%
  group_by(day(date)) %>%
  summarize(steps_median=median(steps)) %>%
  
  


## Are there differences in activity patterns between weekdays and weekends?
