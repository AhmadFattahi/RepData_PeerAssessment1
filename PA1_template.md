# Reproducible Research Course - Week 2 Project
Ahmad Fattahi  
April 22, 2017  



## Reading Data and Formatting
Let's first read the data into a dataframe and format the date variable into date in R using as.Date. The other transofmration is to extract hour and minute intervals from the *interval* variable: 


```r
rawd <- read.csv("activity.csv")
rawd$date <- as.Date(as.character(rawd$date))
rawd$hour <- rawd$interval %/% 100
rawd$minute <- rawd$interval %% 100
```

##Daily Summary

Let's use *tapply* to caculate daily activity summary:


```r
dailys <- tapply(rawd$steps, rawd$date, sum)
hist(dailys, breaks = 10, xlab = "Number of daily steps", ylab = "Number of days", main = "Distribution of daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_total_daily <- mean(dailys, na.rm = TRUE)
median_total_daily <- median(dailys, na.rm = TRUE)
mean_total_daily
```

```
## [1] 10766.19
```

```r
median_total_daily
```

```
## [1] 10765
```

##Analyzing 5-Minute Intervals on Average
In this section we look at different 5-minute intervals on average across all days.


```r
int_avg <- tapply(rawd$steps, as.factor(rawd$interval), mean, na.rm = TRUE)
plot(names(int_avg), int_avg, type = "l", xlab = "Interval", ylab = "Average steps per 5 minutes", main = "Average of sums of 5-minute steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Now let's look at the 5-minute interval with the highest number of steps on average:

```r
max_int <- which.max(int_avg)
max_int
```

```
## 835 
## 104
```
So, the 835 interval has the highest numebr of steps on average among all 5-minute intervals. 

##Dealing with Missing Values

Let's first calculate the number of missing values in the dataset:

```r
sum(is.na(rawd$steps))
```

```
## [1] 2304
```
The next step is to handle missing values by imputing the data. We choose to replace any missing value by the median of that day before imputing.


```r
#The function imp takes a vector and imputes all missing values (NA) by its median. If all are missing values it returns the same vector of all missing values.
imp <- function(v){
    if (sum(is.na(v)) != length(v)){
        w <- replace(v, is.na(v), median(v, na.rm = TRUE)) }
    else {w <- v}
    w
}
e <- c() #Create a null vector as aggregator
for (i in levels(as.factor(rawd$date))){
    imputedv <- imp(subset(rawd$steps, rawd$date == i))
    e <- c(e, imputedv)
}
#Vector e contains the vector of all steps except missing values are replaced by the mean of the same day. If a day has no good values e will also report all missing values.

rawd1 <- rawd
rawd1$steps <- e #rawd1 is equal to the original dataset except it has the steps vector imputed
```
Let's take a look at the daily activity with imputed data and compare that with those of original data.

```r
dailys1 <- tapply(rawd1$steps, rawd1$date, sum)
hist(dailys1, breaks = 10, xlab = "Number of daily steps", ylab = "Number of days", main = "Distribution of daily steps with imputed values")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean_total_daily1 <- mean(dailys1, na.rm = TRUE)
median_total_daily1 <- median(dailys1, na.rm = TRUE)
mean_total_daily1
```

```
## [1] 10766.19
```

```r
median_total_daily1
```

```
## [1] 10765
```
So it appears the results are identical with those before impting the data. **This is expected!** Looking more closely at the NAs in the original dataset it appears NAs only appear throughout data related to one whole day. Other days have no NAs and a number of days are all NA. We imputed the data by replacing NAs with the median of that same day; obviously if there is no good value for the whole day the result remains NA. This makes sense because if there is no good data for the whole day we may want to ignore that day instead of imputing it with values from other days.

##Weekdays vs. Weekends

We like to compare weekends vs. weekdays activities. We first calculate a new variable that indicates if a certain date is weekend or weekday and assign it to a new variable in the dataset.

```r
dofw <- weekdays(rawd1$date)
WD <- function(i){
    if (i %in% c("Saturday", "Sunday")) w <- "weekend"
    else w <- "weekday"
    w
} 
dayfac <- tapply(dofw, 1:length(dofw), WD)
rawd1$wdwe <- dayfac

#Subsetting weekday and weekend rows in two dataframes
rawd1we <- subset(rawd1, rawd1$wdwe == "weekend")
rawd1wd <- subset(rawd1, rawd1$wdwe == "weekday")

#Calculating means of intervals across all days in each data frame and plotting
int_avg_we <- tapply(rawd1we$steps, as.factor(rawd1we$interval), mean, na.rm = TRUE)
int_avg_wd <- tapply(rawd1wd$steps, as.factor(rawd1wd$interval), mean, na.rm = TRUE)

par(mfrow = c(2, 1), mar = c(2, 2, 1, 3))
plot(names(int_avg_we), int_avg_we, type = "l", xlab = "Interval", ylab = "Mean steps - Weekend", main = "Average of 5-minute steps - Weekends", lwd = 2)
plot(names(int_avg_wd), int_avg_wd, type = "l", xlab = "Interval", ylab = "Mean steps - Weekday", main = "Average of 5-minute steps - Weekdays", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

