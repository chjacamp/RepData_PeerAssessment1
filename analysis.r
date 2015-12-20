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
hist(steps_per_day$total_steps_mean, breaks=7)

hist(steps_per_day$total_steps_median, breaks=7)

## What is the average daily activity pattern?

steps_per_interval <- activity_data %>%
  tbl_df %>%
  

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
