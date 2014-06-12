options(stringsAsFactors = FALSE)
activity <- read.csv('activity.csv',header=T)
activity$dateTime <- as.POSIXct(activity$date) + as.difftime(5 * activity$interval,units='mins')

head(activity)

timeString <- function(t) {
        t <- sprintf("%04d",as.numeric(t))
        paste(substr(t,1,nchar(t)-2),substr(t,nchar(t)-1,nchar(t)),sep=":")
}

formatTime <- function(dt,i) {
        tm <- POSIXtime(timeString(i))
        as.POSIXct(paste(dt,tm))
}



activity$dateTime <- formatTime(activity$date,activity$interval)

stepsPerDay <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
m <- mean(stepsPerDay$x,na.rm = T)
median(stepsPerDay$x,na.rm = T)

hist(stepsPerDay$x,breaks=seq(0,25000,by=500),main = "Number of steps taken",xlab = "Steps")
abline(v = m, col = "blue", lwd = 2)

stepsPerInt <- aggregate(activity$steps, by=list(activity$interval), FUN=mean,na.rm=T)

head(stepsPerInt)

maxStepInt <- subset(stepsPerInt,(stepsPerInt$x==max(stepsPerInt$x)))$Group.1

plot(stepsPerInt,type="l",xlab="Interval",ylab="Steps",main="Number of steps taken")
abline(v = maxStepInt, col = "blue", lwd = 1)

length(activity$steps[is.na(activity$steps)])

findAverage <- function(i) as.integer(subset(stepsPerInt,(stepsPerInt$Group.1==i))$x)


subset(activity,is.na(activity$steps))$steps <- findAverage(subset(activity,is.na(activity$steps))$interval)

nrow(activity)

for (r in 1:nrow(activity)) {
        if (is.na(activity$steps[r])) {
                activity$steps[r] <- findAverage(activity$interval[r])
        }
}

isWeekend <- function(d) {
        day = weekdays(d)
        if (day=="Saturday" || day=="Sunday") T
        else F
}

activity$isWeekend <- factor(weekdays(as.POSIXct(activity$date)) %in% c('Saturday','Sunday'))

head(activity)
?aggregate