# Processing new files from Mathieu Buoro for Oir, Scorff and Bresle
filepath <- './'

library(tidyverse)

Data <- read_csv(paste0(filepath,"Smolt_Scorff_Lesle.csv"))

# Data supplied in a sparse matrix form, first column julien day of year, remaining columns are the observation years. NA means no count 
# took place or zero fish observed

# 2020 no counts were taken so R detected this column as logical, convert to dble
Data$`2020` <- as.numeric(Data$`2020`)

# target format is yearly quartiles of total smolt run 25% and 50%
# change NA to 0

Data <- Data %>% mutate_all(~replace(., is.na(.), 0))

# transform from wide to long format

Data <- gather(Data,year,count,`1997`:`2021`)

# translate day of year to julian day then to date

Data$intDate <- (Data$julianday - 1) + (as.integer(lubridate::date(paste0(Data$year,'-01-01'))))
Data$date <- lubridate::as_date(Data$intDate)
# split out month, week, day
Data$month <- lubridate::month(Data$date)
Data$week <- lubridate::week(Data$date)
Data$day <- lubridate::yday(Data$date)
#

# sum up counts and percentages
Daily_Counts <- Data %>% group_by(year) %>% mutate(dailyCountCum = cumsum(count))
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
write_csv(Daily_Counts,"Smolt_Scorff_Lesle_processed.csv")
write_csv(Quartiles,"Smolt_Scorff_Lesle_Timing_Quartiles.csv")
