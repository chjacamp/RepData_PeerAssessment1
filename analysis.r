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

##need two new data frames  
activity_data_na <- activity_data

##Na_replace is the mean steps per day
na_replace <- activity_data_na %>%
  group_by(date) %>%
  summarize(steps_mean=mean(steps))

##To begin a for loop, we need empty or semi-empty vectors defined
##for it to operate upon.
date_for_mean <- ymd("2012-10-01")
grab <- as.integer(0)
j <- as.integer(0)

for (i in 1:length(activity_data_na$steps)) { #1:observations

  all_means <- mean(na_replace$steps_mean, na.rm=TRUE) #find all the means
  date_for_mean <- ymd(activity_data_na[i,2])
  grab <- which(na_replace$date == ymd(date_for_mean))
  
    if (is.na(activity_data_na[i,1]) && #condition: activity_data row is NA value
        !is.na(na_replace[date_for_mean,2])) { ##is there a value in na_replace?
      
      activity_data_na[i,1] <- na_replace[grab,2]
    }
    
    else if (is.na(activity_data_na[i,1]) && #activity_data step is NA value
             is.na(na_replace[date_for_mean,2])) { #date for mean is also NA
      activity_data_na[i,1] <- all_means
    }
    
    else if (!is.na(activity_data_na[i,1])) { #was having issues, needed to be sure
                                           #loop was running correctly
       j <- j+1
    }
  
  i <- i+1
}

##Use same code to create steps for days but with new revised data frame


steps_per_day_na <- activity_data_na %>%
  tbl_df %>%
  mutate(steps = as.numeric(as.character(steps))) %>%
  group_by(date) %>%
  mutate(total_steps = sum(steps)) %>%
  summarize(total_steps_mean=mean(total_steps), 
            total_steps_median=median(total_steps))



par(mfrow=c(2,2), mar = c(4,4,2,1), mex = 1.2, cex.main = .8)

##with na's
hist(steps_per_day$total_steps_mean, breaks=7,
     ylim = c(1,35),
     main = "Mean Total Steps per Day 
     With NA Values",
     xlab = "total mean steps/day")

hist(steps_per_day$total_steps_median, breaks=7,
     ylim = c(1,35),
     main = "Median Total Steps per Day 
     With NA Values",
     xlab = "total median steps/day")

##without na's
hist(steps_per_day_na$total_steps_mean, breaks=7,
     main = "Mean Total Steps per Day
     Without NA Values",
     xlab = "total mean steps/day")

hist(steps_per_day_na$total_steps_median, breaks=7,
     main = "Median Total Steps per Day
     Without NA Values",
     xlab = "total median steps/day")


## Are there differences in activity patterns between weekdays and weekends?

##FOR LOOPS FOR DAYS..>FEELING IT
for (i in 1:17568) {
ifelse(any(weekdays(activity_data_na$date[i]) == 
             c("Monday", "Tuesday","Wednesday","Thursday","Friday")),
       
       activity_data_na[i,4] <- "Weekday",
       activity_data_na[i,4] <- "Weekend")
}

activity_data_na[,4] <- as.factor(activity_data_na[,4])
names(activity_data_na) <- c("steps", "date", "interval", "Day")

steps_per_interval_na <- activity_data_na %>%
  tbl_df %>%
  group_by(interval, Day) %>%
  summarize(steps2 = mean(steps, na.rm=TRUE))


p <- ggplot(steps_per_interval_na, aes(interval, steps2)) + 
              geom_line(aes(colour = Day, Group = Day), size=.75) + theme_bw()
p + scale_fill_manual(values=c("#CC6666", "#9999CC"))








