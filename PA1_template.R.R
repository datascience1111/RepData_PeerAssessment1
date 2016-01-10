
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
activity$date = as.Date(activity$date)
activity$date <- strftime(activity$date, "%m-%d-%Y")
total_steps <- aggregate(steps ~ date, data = activity, sum, na.rm=TRUE)

#Calculate the total number of steps taken per day

total_steps <- aggregate(steps ~ date, data = activity, sum, na.rm=TRUE)

##Make a histogram of the total number of steps taken each day

hist(total_steps$steps, xlab = "Total Number of Steps Taken", ylab = "Count", main = "Total Number Steps Taken by Date", ylim = c(0,30))

#Calculate and report the mean and median of the total number of steps taken per day

summary(total_steps$steps)

dev.copy(png, file = 'totalsteps.png', width = 1024, height = 768, units = 'px')
dev.off()

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(interval$interval, interval$steps, type = "l", xlab = "Intervals", ylab = "Number of Steps", main = "Total Number of Steps by Intervals")

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

head(arrange(interval, desc(steps)))

dev.copy(png, file = '5mininter.png', width = 1024, height = 768, units = 'px')
dev.off()

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(activity))

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity2 <- activity

for (i in 1:nrow(activity2)){
    if (is.na(activity2$steps[i])) {
        activity2$steps[i] <- interval$steps[which(activity2$interval == interval$interval)] 
    }
}


##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

total_steps2 <- aggregate(steps ~ date, data = activity2, sum, na.rm=TRUE)

hist(total_steps2$steps, xlab = "Total Number of Steps Taken", ylab = "Count", main = "Total Number Steps Taken by Date", ylim = c(0,30))

summary(total_steps$steps)
summary(total_steps2$steps)

dev.copy(png, file = 'totalsteps2.png', width = 1024, height = 768, units = 'px')
dev.off()

##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

activity2$weekdatetype <- weekdays(activity2$date)

activity2$weekdatetype <- as.factor(ifelse(weekdays(activity2$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

weekend <- activity2[activity2$weekdatetype %in% c("Weekend"), ]
interval_weekend <- aggregate(steps ~ interval, data = weekend, FUN = mean)


weekday <- activity2[activity2$weekdatetype %in% c("Weekday"), ]
interval_weekday <- aggregate(steps ~ interval, data = weekday, FUN = mean)

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

par(mfrow=c(2,1))

plot(interval_weekend$interval, interval_weekend$steps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", col = "red", main = "Weekend")
plot(interval_weekday$interval, interval_weekday$steps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", col = "red", main = "Weekday")

dev.copy(png, file = 'weekdays_comp.png', width = 1024, height = 768, units = 'px')
dev.off()







