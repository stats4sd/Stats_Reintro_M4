library(tidyverse)
library(lubridate)

#data <- read_csv("Dee - Ceiriog.csv")
#data <- read_csv("Dee - Manly Hall.csv")
#data <- read_csv("Dee - Shocklach.csv")
data <- read_csv("Dee - Worthenbury.csv")

names(data) <- c("date","day","count","dailyCountCum","dailyPercentageCum")

# transform date column into date type
data$date <- lubridate::as_date(lubridate::parse_date_time(data$date,'dby'))

# remove duplicate days by summing daily count
data <- data %>% group_by(date) %>% mutate(count = sum(count))
# and removing duplicates based on date and total count
data <- data[!duplicated(data[c('date','count')]),]

# split out month, week, day
data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data$week <- lubridate::week(data$date)

Daily_Counts <- data

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
write_csv(Daily_Counts,"Smolt_Dee_Worthenbury_processed.csv")
write_csv(Quartiles,"Smolt_Dee_Worthenbury_Timing_Quartiles.csv")