library(tidyverse)
library(lubridate)

data <- read_csv("Tamar_smolt_counts.csv")


# transform date column into date type
data$date <- lubridate::as_date(lubridate::parse_date_time(data$date,'dby'))
data <- data %>% arrange(date)

# remove duplicate days by summing daily count
data <- data %>% group_by(date) %>% mutate(count = sum(count))
# and removing duplicates based on date and total count
data <- data[!duplicated(data[c('date','count')]),]

# split out month, week, day
data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data$week <- lubridate::week(data$date)

Daily_Counts <- data

# sum up counts and percentages
Daily_Counts <- data %>% group_by(year) %>% mutate(dailyCountCum = cumsum(count))
Daily_Counts <- Daily_Counts %>% group_by(year) %>% mutate(dailyPercentageCum = dailyCountCum/sum(count)*100)

# extract quartiles

A <- Daily_Counts %>% group_by(year) %>% filter(dailyPercentageCum >= 25) %>% filter(abs(dailyPercentageCum-25)==min(abs(dailyPercentageCum-25)))
B <- Daily_Counts %>% group_by(year) %>% filter(dailyPercentageCum >= 50) %>% filter(abs(dailyPercentageCum-50)==min(abs(dailyPercentageCum-50)))
C <- Daily_Counts %>% group_by(year) %>% filter(dailyPercentageCum >= 75) %>% filter(abs(dailyPercentageCum-75)==min(abs(dailyPercentageCum-75)))

A$quartile <- "25"
B$quartile <- "50"
C$quartile <- "75"

Quartiles <- rbind(A,B,C)
Quartiles <- arrange(Quartiles,year)

# remove duplicates - default behaviour keeps first instance of duplicate therefore as long as data is arranged by date ascending this should
# always keep the earliest date that a particular quartile occurs

Quartiles <- Quartiles[!duplicated(Quartiles[c('year','quartile')]),]


# export to csv
write_csv(Daily_Counts,"Smolt_Tamar_processed.csv")
write_csv(Quartiles,"Smolt_Tamar_Timing_Quartiles.csv")