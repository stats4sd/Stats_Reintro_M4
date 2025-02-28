# The Bush timing data was provided mostly pre-processed with start, 25%, Peak and end dates
# This script brings it in to line with the other timing datasets with year, month, week, day and quartile columns
library(tidyverse)
library(lubridate)

bush <- readxl::read_xlsx('Bush smolt timing from Richard.xlsx',col_types = c('numeric','numeric','date','text','date','date'),na = 'na')

# readxl converts some month-day data in the file to year-month-day, using 2021 as the year
# the year is in fact in the year column so these need to be fixed

bush$FIRST <- lubridate::as_date(lubridate::parse_date_time(paste0(lubridate::day(bush$FIRST),'-',lubridate::month(bush$FIRST),'-',bush$YEAR),'dmy'))

bush$q1 <- paste0(bush$q1,'-',bush$YEAR)
bush$q1 <- lubridate::as_date(parse_date_time(bush$q1,'dby'))

bush$PEAK <- lubridate::as_date(lubridate::parse_date_time(paste0(lubridate::day(bush$PEAK),'-',lubridate::month(bush$PEAK),'-',bush$YEAR),'dmy'))

bush$LAST <- lubridate::as_date(lubridate::parse_date_time(paste0(lubridate::day(bush$LAST),'-',lubridate::month(bush$LAST),'-',bush$YEAR),'dmy'))

# change the data frame to a long format
quartiles <- tidyr::pivot_longer(bush,cols = c('FIRST','q1','PEAK','LAST'),names_to = 'quartile')

names(quartiles) <- c('year','count','quartile','date')

quartiles$month <- lubridate::month(quartiles$date)
quartiles$week <- lubridate::week(quartiles$date)
quartiles$day <- lubridate::yday(quartiles$date)

quartiles$quartile <- str_replace(quartiles$quartile,'q1','25')


write_csv(quartiles,"Smolt_Bush_Timing_Quartiles.csv")
