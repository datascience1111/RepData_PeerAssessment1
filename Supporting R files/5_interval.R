##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(interval$interval, interval$steps, type = "l", xlab = "Intervals", ylab = "Number of Steps", main = "Total Number of Steps by Intervals")

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

head(arrange(interval, desc(steps)))

dev.copy(png, file = '5mininter.png', width = 1024, height = 768, units = 'px')
dev.off()