# Loading and Preprocessing Data

library( dplyr)

fileUrl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
	# URL from project page; using URL from GitHub results in:
	# "Warning message:
	# In unzip(filePath, exdir = "./") : error 1 in extracting from zip file"
	
filePath = "./data.zip"

#download.file( fileUrl, filePath, method = "curl")
	# download .zip file to working directory as data.zip

setwd( "~/Documents/School/Coursera/Data Science Specialization/Reproducible Research/R") # remove for knitr

unzip( filePath,  exdir = "./")
	# unzip data.zip file

dataset = read.csv( "./activity.csv")
	# import data into data frame

unlink( c( filePath, "./activity.csv"))
	# delete data.zip, activity.csv from working directory

dataset$date = as.Date( dataset$date)

datatbl.full = tbl_df( dataset)
	# convert dataset to data table

datatbl = datatbl.full[ complete.cases( datatbl.full),]
	# remove incomplete cases from data
	




# WHAT IS THE MEAN NUMBER OF STEPS TAKEN PER DAY?


daily = group_by( datatbl, date)
	# group data by date


# Calculate the total number of steps taken per day

daily.steps.total = summarize( daily, sum( steps))
	# create data table with date column (unique rows) and total steps column

daily.steps.total = select( daily.steps.total, date, total.steps = starts_with( "sum"))
	# renaming second column; could not get rename function to work as expected,
	#	possibly due to the column name being in format of function call.

daily.steps.total
	

# Make a histogram of the total number of steps taken each day

hist( daily.steps.total$total.steps, main = "Histogram of total steps taken daily", xlab = "Total steps", ylab = "Frequency (in days)", ylim = range( 0:40))


# Calculate and report the mean and median of the total number of steps taken
#	per day

mean( daily.steps.total$total.steps)

median( daily.steps.total$total.steps)
	




# WHAT IS THE AVERAGE DAILY ACTIVITY PATERN?


# Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval
#	(x-axis) and the average number of steps taken, averaged across all days
#	(y-axis)

by.interval = group_by( datatbl, interval)
	# group data by interval

interval.mean = summarize( by.interval, mean( steps))
	# create 2 column table, interval and mean steps

interval.mean = select( interval.mean, interval, mean.steps = starts_with( "m"))
	# provide better column names

dev.new() # remove for knitr

plot( interval.mean$interval, interval.mean$mean.steps, type = "l")
	# line plot with x = interval and y = mean number of steps


# Which 5-minute interval, on average across all the days in the dataset,
#	contains the maximum number of steps?

arrange( interval.mean, desc( mean.steps))[ 1,]






# IMPUTING MISSING VALUES


# Calculate and report the total number of missing values in the dataset (i.e.
#	the total number of rows with 𝙽𝙰s)

sum( ! complete.cases( datatbl.full))
	# This method checks all columns for missing values and avoids double
	#	counting if the same row has more than one column with missing values.
	
as.data.frame( summarize( group_by( datatbl.full, date), sum( is.na( steps))))
	# How are the data's missing values clustered?


# Devise a strategy for filling in all of the missing values in the dataset. The
#	strategy does not need to be sophisticated. For example, you could use the
#	mean/median for that day, or the mean for that 5-minute interval, etc.
#	Create a new dataset that is equal to the original dataset but with the
#	missing data filled in.


datatbl.full$steps = as.numeric( datatbl.full$steps)

for (i in 1 : length( datatbl.full$steps)) {

	if ( is.na( datatbl.full$steps[ i])) {
	
		x = datatbl.full$interval[ i] # obtain interval number
		
		datatbl.full$steps[ i] = filter( interval.mean, interval == x)$mean.steps # replace NA with interval's mean value
		
	}	

}
	# here we've created a for loop that replaces NA values for the steps variable with the mean interval value.




# Make a histogram of the total number of steps taken each day and Calculate and
#	report the mean and median total number of steps taken per day. Do these
#	values differ from the estimates from the first part of the assignment? What
#	is the impact of imputing missing data on the estimates of the total daily
#	number of steps?


# first we rename some previously used variables so that we can save the values
#	and also recycle code without modification.

daily.na.rm = daily

daily.steps.total.na.rm = daily.steps.total

dailytbl.na.rm = datatbl

datatbl = datatbl.full



# Now we copy and paste from a previous section.

daily = group_by( datatbl, date)
	# group data by date

# Calculate the total number of steps taken per day

daily.steps.total = summarize( daily, sum( steps))
	# create data table with date column (unique rows) and total steps column

daily.steps.total = select( daily.steps.total, date, total.steps = starts_with( "sum"))
	# renaming second column; could not get rename function to work as expected.

daily.steps.total
	

# Make a histogram of the total number of steps taken each day

dev.new() # remove for knitr

hist( daily.steps.total$total.steps, main = "Histogram of total steps taken daily", xlab = "Total steps", ylab = "Frequency (in days)", ylim = range( 0:40))


# Calculate and report the mean and median of the total number of steps taken
#	per day

mean( daily.steps.total$total.steps)

median( daily.steps.total$total.steps)








# ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?


# Create a new factor variable in the dataset with two levels – “weekday” and
#	“weekend” indicating whether a given date is a weekday or weekend day.

datatbl$day = ifelse( weekdays( datatbl$date) %in% c( "Saturday", "Sunday"), "Weekend", "Weekday")
	# Checks to see if date falls on a Saturday or Sunday and assigns value of
	#	weekend or weekday appropriately.

datatbl$day = factor( datatbl$day)
	# convert new variable to factor.
	



# Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the
#	5-minute interval (x-axis) and the average number of steps taken, averaged
#	across all weekday days or weekend days (y-axis). See the README file in the
#	GitHub repository to see an example of what this plot should look like using
#	simulated data.

weekend = filter( datatbl, day == "Weekend")

weekday = filter( datatbl, day == "Weekday")
	# create seperate data frames for weekends and weekdays


by.interval.we = group_by( weekend, interval)

by.interval.wd = group_by( weekday, interval)
	# group data by interval
	

interval.mean.we = summarize( by.interval.we, mean( steps))

interval.mean.wd = summarize( by.interval.wd, mean( steps))
	# create 2 column table, interval and mean steps


interval.mean.we = select( interval.mean.we, interval, mean.steps = starts_with( "m"))
	
interval.mean.wd = select( interval.mean.wd, interval, mean.steps = starts_with( "m"))
	# provide better column names


dev.new() # remove for knitr

par( mfrow = c( 2, 1))
	# prepare for two plots

plot( interval.mean.we$interval, interval.mean.we$mean.steps, type = "l", main = "Weekend", ylab = "Mean step count", xlab = "Time interval", ylim = range( 0:250))

plot( interval.mean.wd$interval, interval.mean.wd$mean.steps, type = "l", main = "Weekday", ylab = "Mean step count", xlab = "Time interval", ylim = range( 0:250))
	# line plot with x = interval and y = mean number of steps





















