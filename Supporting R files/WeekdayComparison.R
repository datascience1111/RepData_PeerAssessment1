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



