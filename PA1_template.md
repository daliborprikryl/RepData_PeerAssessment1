# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Step0:Load packages
library(dplyr);library(lattice)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# Step1:Download activity data
setwd("C:/Users/dalib_000/Desktop/Coursera/2.Data Science Specialization/5.Reproducible Research")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destFile <- "C:/Users/dalib_000/Desktop/Coursera/2.Data Science Specialization/5.Reproducible Research/Activity.zip"
download.file (url = fileUrl,destfile = destFile,method = "libcurl")
unzip (destFile ,exdir="Project1")

# Step2:Read activity data
destFile<- "C:/Users/dalib_000/Desktop/Coursera/2.Data Science Specialization/5.Reproducible Research/Project1/Activity.csv"
myimport <- read.csv(destFile)
myimport$date = as.Date(as.POSIXct(myimport$date),format = "%d/%m/%Y")
mydata <- myimport [which(myimport$steps!= "NA"), ]
```


## What is mean total number of steps taken per day?


```r
# Step3: Creates histogram of total steps by date
myplot1 <- mydata %>% group_by(date) %>% summarise(Steps=sum(steps))
png("plot1.png", width=480, height=480, units="px")
hist(myplot1$Steps, main="Total steps per day",xlab="steps per day")	
dev.off()

# Step4: mean and median of total number of steps taken per day
mean_of_steps <- mean(myplot1$Steps)
median_of_steps <- median(myplot1$Steps)
```

## What is the average daily activity pattern?


```r
# Step5: Creates plot of average number of steps taken in PNG format
myplot2 <- mydata %>% group_by(interval) %>% summarise(Steps=mean(steps))
png("plot2.png", width=480, height=480, units="px") 
plot(myplot2$interval, myplot2$Steps, type="l", xlab=" 5-minute interval", ylab="average number of steps taken") 
dev.off()

# Step6: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
int_w_max_of_steps <- select(filter(myplot2,Steps==max(Steps)),interval)
```
## Imputing missing values

```r
# Step7:Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
count_of_na <- sum(is.na(myimport$steps))

# Step8:Devise a strategy for filling in all of the missing values in the dataset. 
# Inputing missing values by using the mean of that 5-minute interval

MD1 <- mydata %>% group_by(interval) %>% summarise(steps=mean(steps))
MD2a <- filter(myimport, is.na(steps))
MD2b <- merge(MD2a[,2:3],MD1,by = "interval",all.x = TRUE)
MD <- rbind(mydata,MD2b)
checksum <- sum(MD$steps)

# Step9: Creates histogram of total steps by date
myplot3 <- MD %>% group_by(date) %>% summarise(Steps=sum(steps))
png("plot3.png", width=480, height=480, units="px")
hist(myplot3$Steps, main="Total steps per day",xlab="steps per day")	
dev.off()

mean_of_steps <- mean(myplot3$Steps)
median_of_steps <- median(myplot3$Steps)
```

## Are there differences in activity patterns between weekdays and weekends?


```r
MD <- mutate(MD,myweekdays=weekdays(as.Date(MD$date)))
MD <- mutate(MD,myweekdayssplit=ifelse(MD$myweekdays %in% c("sobota", "neděle"),"weekend", "weekday"))
myplot4 <- MD %>% group_by(interval,myweekdayssplit) %>% summarise(Steps=mean(steps))
png("plot4.png", width=480, height=480, units="px")
xyplot(Steps ~ interval | myweekdayssplit, data = myplot4, layout = c(1, 2), type="l")	
dev.off()
```
