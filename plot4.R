# Exploratory data analysis
# Course Project One  - Plot 4.

# Note the first part of this code - reading and cleaning data - is identical to plot 1. 

library(dplyr)
library(lubridate)

# Download the data
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
              destfile = "powercons.zip", method="curl")
unzip("powercons.zip")
# Resulting file is "household_power_consumption.txt"
file_name <- list.files(pattern = "*.txt")[1] # use a pattern to avoid other files I have there

# File starts 16/12/06, we want two days 1/2/07 - 2/2/07.
# To read in that data only, assume 1. File is in date order (confirmed)
# 1 obs per min = 1440 per day. start about 44 days in and stop 47 days in (a few extra lines
# will be removed later by subsetting.)
line_start <- 46 * 1440
line_end <- 49 * 1440
lines_to_read = line_end - line_start
consum <- scan(file_name, 
               what = list("", "", "", "", "", "", "", "", ""), 
               sep=";", 
               skip = line_start, 
               nlines=lines_to_read, 
               na.strings = c("?"))

str(consum)
# We get a list of 9 (corresponding to columns) and about half a day either side of our window.

# Convert list of characters to a character matrix
house <- do.call(cbind, consum) %>% as.data.frame(stringsAsFactors = FALSE)
dim(house) 
str(house)
head(house)
# looking good.

# Add names. Note to use the subset operator "[[ ]]" with pipe %>% we have to use a "dot" (meaning the result output from strsplit). 
# See http://www.r-statistics.com/2014/08/simpler-r-coding-with-pipes-the-present-and-future-of-the-magrittr-package/
headers <- readLines(file_name, 1) %>% strsplit(split=";") %>% .[[1]]
names(house) <- headers

# Convert to a data frame. importantly do not allow factors to form. Just a chr data frame.
house <- as.data.frame(house, stringsAsFactors = FALSE)

# Now convert each column to the right data type, merging DateTime to an extra column.
# use lubridate methods (dmy_hms() and dmy()) to convert dates.
good_house <- house %>% 
    mutate(DateTime = dmy_hms(paste(Date, Time))) %>%
    mutate(Date = as.Date(dmy(Date))) %>%
    mutate(Global_active_power = as.numeric(Global_active_power)) %>%
    mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
    mutate(Voltage = as.numeric(Voltage)) %>%
    mutate(Global_intensity = as.numeric(Global_intensity)) %>%
    mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
    mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
    mutate(Sub_metering_3 = as.numeric(Sub_metering_3))


# Finally... subset house by Date comparison to get just the values for our dates: first and second of Feb, 07
final_house <- good_house %>%
    filter(Date > as.Date("2007-01-31")) %>%
    filter(Date < as.Date("2007-02-03"))

dim(final_house)                        # gives us 2880, as expected
unique(as.Date(final_house$DateTime))   # gives us exactly the two dates. 

#
# New stuff below here
#

# Plot out a panel of plots. Some are similar to plot 2 and 3.

# Prepare output file, defaults to 480 x 480
png("plot4.png")

# Set up our grid
par(mfrow = c(2, 2))

# First plot: same as plot 2, Global power v time
plot(final_house$DateTime, final_house$Global_active_power, 
     type="l", xlab = "", ylab = "Global Active Power")

# Second plot, time v voltage
plot(final_house$DateTime, final_house$Voltage, 
     type="l", xlab = "datetime", ylab = "Voltage")

# Third plot: same as plot 3.  Start with an empty plot and add lines by colour.
plot(final_house$DateTime, final_house$Sub_metering_1, 
     main = "", type = "n", xlab = "", ylab = "Energy sub metering")
lines(final_house$DateTime, final_house$Sub_metering_1, col= "black")
lines(final_house$DateTime, final_house$Sub_metering_2, col= "red")
lines(final_house$DateTime, final_house$Sub_metering_3, col= "blue")
legend("topright", lty = 1, col = c("black", "red", "blue"), bty = "n",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Fourth plot: 
plot(final_house$DateTime, final_house$Global_reactive_power, 
     type="l", xlab = "datetime", ylab = "Global_reactive_power")

# Write out a png file. 
dev.off()

