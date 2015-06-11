#   Load Libraries
    library(ggplot2);
    library(dplyr); library(plyr);

    
#   Loading & Processing the Data
    setwd("~/Data Science/05 Reproducible Research")
    unzip("~/Data Science/05 Reproducible Research/repdata_data_activity.zip");
    activity_full <- read.csv("~/Data Science/05 Reproducible Research/activity.csv",
        na.strings="NA");

    
#   What is the mean total number of steps taken per day?
    activity  <- subset(activity_full, steps != "NA");
    
#   1. Total Number of Steps Taken per Day
    activity_total <- ddply(activity, "date", summarise, total=sum(steps));
    head(activity_total, n=10);

#   2. Histogram of the Total Number of Steps Taken per Day
    g <- ggplot(activity_total, aes(total))
    g <- g + geom_histogram(aes(total), binwidth=2500, color="black", fill="green", 
        alpha=.5)
    g <- g + ggtitle("Number of Steps Taken per Day") + labs(x="Steps", y="Frequency")
    g <- g  + theme_bw()
    g

#   3. Mean & Median of the Total Number of Steps taken per Day
    total <- sum(activity_total$total);
    mean <- mean(activity_total$total);
    median <- median(activity_total$total);
    total;  mean;  median;
    

#   What is the average daily activity pattern?
    activity_mean <- ddply(activity, "interval", summarise, mean=mean(steps));
    head(activity_mean, n=10);
    
#   1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#   and the average number of steps taken, averaged across all days (y-axis).
    g <- ggplot(activity_mean, aes(x=interval, y=mean))
    g <- g + geom_line(stat="identity", color="blue")
    g <- g + ggtitle("Avg Steps by 5-Minute Interval Accross All Days") 
    g <- g + labs(x="Interval", y="Steps")
    g <- g  + theme_bw()
    g

#   2. Which 5-minute interval, on average across all the days in the dataset, 
#   contains the maximum number of steps?
    activity_mean$interval[which.max(activity_mean$mean)];
    

#   Imputing Missing Values
#   Note that there are a number of days/intervals where there are missing values 
#   (coded as NA).  The presence of missing days may introduce bias into some 
#   calculations or summaries of the data.
    
#   1. Calculate and report the total number of missing values in the dataset 
#   (i.e. the total number of rows with NAs).
    length(which(is.na(activity_full)));
    
#   2. Devise a strategy for filling in all of the missing values in the dataset. 
#   The strategy does not need to be sophisticated. For example, you could use 
#   the mean/median for that day, or the mean for that 5-minute interval, etc.
    # See part #3 below.
    
#   3. Create a new dataset that is equal to the original dataset but with the 
#   missing data filled in.
    missing_values <- data.frame(activity_full$steps);
    missing_values[is.na(missing_values), ] <- ceiling(tapply(X=activity_full$steps, INDEX=activity_full$interval, FUN=mean, na.rm=TRUE));
    activity_missing_values <- cbind(missing_values, activity_full[, 2:3]);
    colnames(activity_missing_values) <- c("steps", "date", "interval");    
    
#   4. Make a histogram of the total number of steps taken each day and Calculate 
#   and report the mean and median total number of steps taken per day.  
    activity_missing_total <- ddply(activity_missing_values, "date", summarise, total=sum(steps));
    head(activity_missing_total, n=10);
    
    g <- ggplot(activity_missing_total, aes(total))
    g <- g + geom_histogram(aes(total), binwidth=2500, color="black", fill="blue", 
        alpha=.5)
    g <- g + ggtitle("Number of Steps Taken per Day") + labs(x="Steps", y="Frequency")
    g <- g  + theme_bw()
    g

#   Do these values differ from the estimates from the first part of the assignment? 
    total_missing <- sum(activity_missing_total$total);
    mean_missing <- mean(activity_missing_total$total);
    median_missing <- median(activity_missing_total$total);
    total_missing;  mean_missing;  median_missing;

    total_variance <- total_missing - total;
    mean_variance <- mean_missing - mean;
    median_variance <- median_missing- median;
    total_variance;  mean_variance;  median_variance;
    
    
#   What is the impact of imputing missing data on the estimates of the total 
#   daily number of steps?
    print(
        paste("Imputing the missing data caused the total daily number of steps", 
        "to increase by", total_variance)
    );
    
    
#   Are there differences in activity patterns between weekdays and weekends?
#   For this part the weekdays() function may be of some help here. Use the dataset 
#   with the filled-in missing values for this part.
    
#   1. Create a new factor variable in the dataset with two levels – “weekday” 
#   and “weekend” indicating whether a given date is a weekday or weekend day.
    
#   2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
#   5-minute interval (x-axis) and the average number of steps taken, averaged 
#   across all weekday days or weekend days (y-axis). See the README file in the 
#   GitHub repository to see an example of what this plot should look like using 
#   simulated data.
    
    
