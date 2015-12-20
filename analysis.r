---
  title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
  keep_md: true
---

require(dplyr)
require(ggplot2)
  
## Loading and preprocessing the data
  
unzip("activity.zip", exdir="data")
activity_data <- read.csv("data/activity.csv")

## What is mean total number of steps taken per day?

steps_per_day <- 

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
