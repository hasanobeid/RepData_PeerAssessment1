library(ggplot2)

filename = "activity.zip"
if(file.exists(filename)){
  unzip(filename)
}

dt <- read.csv("activity.csv", sep = ",", header = T)
dt$date <- as.Date(as.character(dt$date), "%Y-%m-%d")

sm <- tapply(dt$steps, dt$date, sum, na.rm = T)
sm <- sm[sm!=0]

hist(sm, main = "Number of steps per day", xlab = "Steps per day")

mean(sm)
median(sm)

stepsbyinterval = tapply(dt$steps, dt$interval, mean, na.rm = T)
plot(names(stepsbyinterval),stepsbyinterval, type = "l", main = "Average number of steps", xlab = "Interval", ylab = "Average number of steps")

mx <- stepsbyinterval[which.max(stepsbyinterval)]
names(mx)
mx[[names(mx)]]

sum(is.na(dt$steps))

newdt <- dt
for (idx in 1:nrow(newdt)){
    if (is.na(newdt[idx,]$steps)){
        newdt[idx,]$steps <- stepsbyinterval[[as.character(newdt[idx,]$interval)]]
    }
}
imputedmean <- tapply(newdt$steps, newdt$date, sum, na.rm = T)
hist(imputedmean, main = "Number of steps per day", xlab = "Steps per day")

mean(imputedmean)
median(imputedmean)

newdt$daytype[, weekdays(newdt$date) %in% c("Saturday","Sunday")] <- "Weekend"
newdt$daytype[, newdt$daytype != "Weekend"] <- "Weekday"
newdt$daytype <- as.factor(newdt$daytype)

imputeddata <- aggregate(steps ~ interval + daytype, newdt, mean)
qplot(interval, steps, data = newdt, geom=c("line"), xlab = "Interval", ylab = "Number of steps") + facet_wrap(~ daytype, ncol=1)