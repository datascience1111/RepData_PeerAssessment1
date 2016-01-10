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

