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
    mean <- mean(activity_total$total);
    median <- median(activity_total$total);
    mean;  median;
    

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
