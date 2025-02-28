# Processing new files from Mathieu Buoro for Oir, Scorff and Bresle
filepath <- './'

library(tidyverse)

data <- read_csv(paste0(filepath,"Smolt_Oir_Cerisel.csv"))

# Data supplied in a sparse matrix form, first column julien day of year, remaining columns are the observation years. NA means no count 
# took place or zero fish observed

# target format is yearly quartiles of total smolt run 25% and 50%
# change NA to 0

data <- data %>% mutate_all(~replace(., is.na(.), 0))

# transform from wide to long format
# Oir data from 1985 to 2021, dropping 1985 as only 2 smolts were recorded that year
data = subset(data, select = -c(`1985`))
data <- gather(data,year,count,`1986`:`2021`)

# translate day of year to julian day then to date

data$intDate <- (data$julianday - 1) + (as.integer(lubridate::date(paste0(data$year,'-01-01'))))
data$date <- lubridate::as_date(data$intDate)
# split out month, week, day
data$month <- lubridate::month(data$date)
data$week <- lubridate::week(data$date)
data$day <- lubridate::yday(data$date)
#

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
write_csv(Daily_Counts,"Smolt_Oir_Cerisel_processed.csv")
write_csv(Quartiles,"Smolt_Oir_Cerisel_Timing_Quartiles.csv")
