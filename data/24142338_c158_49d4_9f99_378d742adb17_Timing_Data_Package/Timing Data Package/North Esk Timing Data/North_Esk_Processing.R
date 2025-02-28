library(tidyverse)
library(lubridate)

data <- read_csv("KMT_smolt_weekly_counts_1975-2013.csv")

# Data for the North Esk downloaded from Marine Science Data Server https://doi.org/10.7489/12384-1

# The counts are in weekly reports, no daily information like others
# sum up counts and percentages
Weekly_Counts <- data %>% group_by(year) %>% mutate(weeklyCountCum = cumsum(count))
Weekly_Counts <- Weekly_Counts %>% group_by(year) %>% mutate(weeklyPercentageCum = weeklyCountCum/sum(count)*100)
# split out month, week, day
Weekly_Counts$start_wk_date <- lubridate::parse_date_time(Weekly_Counts$start_wk,'dmy')
Weekly_Counts$wk_start_dayOfYear <- lubridate::yday(Weekly_Counts$start_wk_date)

# extract quartiles

A <- Weekly_Counts %>% group_by(year) %>% filter(weeklyPercentageCum >= 25) %>% filter(abs(weeklyPercentageCum-25)==min(abs(weeklyPercentageCum-25)))
B <- Weekly_Counts %>% group_by(year) %>% filter(weeklyPercentageCum >= 50) %>% filter(abs(weeklyPercentageCum-50)==min(abs(weeklyPercentageCum-50)))
C <- Weekly_Counts %>% group_by(year) %>% filter(weeklyPercentageCum >= 75) %>% filter(abs(weeklyPercentageCum-75)==min(abs(weeklyPercentageCum-75)))

A$quartile <- "25"
B$quartile <- "50"
C$quartile <- "75"

Quartiles <- rbind(A,B,C)
Quartiles <- arrange(Quartiles,year)
# remove duplicates - default behaviour keeps first instance of duplicate therefore as long as data is arranged by date ascending this should
# always keep the earliest date that a particular quartile occurs

Quartiles <- Quartiles[!duplicated(Quartiles[c('year','quartile')]),]

# export to csv
write_csv(Weekly_Counts,"Smolt_North_Esk_processed.csv")
write_csv(Quartiles,"Smolt_North_Esk_Timing_Quartiles.csv")
