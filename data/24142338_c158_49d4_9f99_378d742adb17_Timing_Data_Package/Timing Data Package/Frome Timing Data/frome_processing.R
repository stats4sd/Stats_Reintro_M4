library(tidyverse)
library(lubridate)

data <- readxl::read_xlsx("SmoltNumbersAndTiming.xlsx")

names(data) <- c("year","25","50","count","ci95")

# readxl converts some month-day data in the file to year-month-day, using 2021 as the year
# the year is in fact in the year column so these need to be fixed

data$`25` <- lubridate::as_date(lubridate::parse_date_time(paste0(lubridate::day(data$`25`),'-',lubridate::month(data$`25`),'-',data$year),'dmy'))
data$`50` <- lubridate::as_date(lubridate::parse_date_time(paste0(lubridate::day(data$`50`),'-',lubridate::month(data$`50`),'-',data$year),'dmy'))


quartiles <- tidyr::pivot_longer(data,cols = c(`25`,`50`),names_to = 'quartile')

names(quartiles) <- c("year","count","ci95","quartile","date")

quartiles$month <- lubridate::month(quartiles$date)
quartiles$week <- lubridate::week(quartiles$date)
quartiles$day <- lubridate::yday(quartiles$date)


# export to csv
write_csv(quartiles,"Smolt_Frome_Timing_Quartiles.csv")
