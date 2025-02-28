library(tidyverse)
library(lubridate)


data <- read_csv("burrishoole_smolt_counts.csv")

data$date <- lubridate::as_date(data$time)
# add week number to data
data$week <- lubridate::week(data$time)
#
data <- data %>% arrange(date)

# drop 0 counts
data <- data[data$salmon_smolt_count_per_day != 0,]

# sum up counts and percentages
Daily_Counts <- data %>% group_by(year) %>% mutate(dailyCountCum = cumsum(salmon_smolt_count_per_day))
Daily_Counts <- Daily_Counts %>% group_by(year) %>% mutate(dailyPercentageCum = dailyCountCum/sum(salmon_smolt_count_per_day)*100)

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
write_csv(Daily_Counts,"Smolt_Burrishoole_processed.csv")
write_csv(Quartiles,"Smolt_Burrishoole_Timing_Quartiles.csv")
