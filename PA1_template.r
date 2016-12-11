 ##Loading and preprocessing the data
setwd("C:/Users/Jesus/Documents/R documentos/Reproducible Research/Project 1/repdata_data_activity")
 
rdata <- read.csv("activity.csv") 
data <- aggregate(steps ~ date, data=rdata, sum, na.rm = TRUE)
hist(data$steps, breaks=20, main="Total Steps per Day", xlab="Steps", ylab="Frequency")



##What is mean total number of steps taken per day?
rdataMean <- mean(rdata$steps, na.rm=TRUE)
rdataMedian <- median(rdata$steps, na.rm=TRUE)
print(paste("The mean steps per day is: ", rdataMean))
print(paste("The median steps per day is: ", rdataMedian))


##What is the average daily activity pattern?
stepsdata <- aggregate(steps ~ interval, data=rdata, mean, na.rm=TRUE)
plot(stepsdata$interval, stepsdata$steps, type="l", main="Average Steps per Five Minute Interval",xlab="Interval No.", ylab="steps")

maxsteps <- max(stepsdata$steps)
print(paste("The maximum number of steps in a five minute interval was: ", maxsteps))


###Imputing missing values
missinValues <- sum(is.na(rdata$steps))
print(paste("There are", missinValues, "missings values."))
 
rdata2 <- rdata
rdata2$steps[is.na(rdata2$steps)] <- median(rdata$steps, na.rm=TRUE)
betterdataday <- aggregate(steps ~ date, data=rdata2, sum, na.rm=TRUE)
hist(betterdataday$steps, breaks=20, main="Total Steps per Day \n Adjusted Data",xlab="Steps", ylab="Frequency")

rdataMean2 <- mean(rdata2$steps)
rdataMedian <- median(rdata2$steps)
print(paste("The mean is: ", rdataMean2))
print(paste("The median is: ", rdataMedian))

###Are there differences in activity patterns between weekdays and weekends?
rdata2$date <- as.Date(rdata2$date)
rdata2$dayname <- weekdays(rdata2$date)
rdata2$weekend <- as.factor(ifelse(rdata2$dayname == "Saturday" |rdata2$dayname == "Sunday", "weekend", "weekday"))
library(lattice)
plotdata <- aggregate(steps ~ interval + weekend, rdata2, mean)
xyplot(steps ~ interval | factor(weekend), data=plotdata, aspect=1/3, type="l")


