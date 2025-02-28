#salmon data

library(tidyverse)
library(openxlsx)

dir<-"data/24142338_c158_49d4_9f99_378d742adb17_Timing_Data_Package/Timing Data Package"
files<-data.frame(file=list.files(dir,recursive = TRUE)) %>%
  filter(str_detect(file,"_processed"))

fun1<-function(x){

  in1<-read.csv(paste(dir,files$file[x],sep="/")) 
  if(!"julianday" %in% colnames(in1)){
  colnames(in1)[colnames(in1)=="day"]<-"julianday"
  colnames(in1)[colnames(in1)=="wk_start_dayOfYear"]<-"julianday"
  }
  if(!"dailyCountCum" %in% colnames(in1)){
    colnames(in1)[colnames(in1)=="weeklyCountCum"]<-"dailyCountCum"
  }
  in1 %>%
  group_by(year) %>%
      filter(dailyCountCum >0) %>%
    summarise(firstday=min(julianday),total=max(dailyCountCum)) %>%
      mutate(site=str_split_fixed(files$file[x],"/",2)[,2] %>% str_remove("_processed.csv"))%>%
    mutate(river=str_split_fixed(files$file[x],"/",2)[,1] %>% str_remove(" Timing Data")) }


1:10 %>% map_df(fun1) ->summary_data

table(summary_data$site)
table(summary_data$river)

table(summary_data$year)

summary_data %>%
  filter(year>2000 & year< 2014) %>%
  filter(site!="Smolt_Burrishoole" & site!="Smolt_Dee_ManlyHall") %>%
  ggplot(aes(y=site,x=firstday ,fill=river))+
  geom_boxplot()


summary_data %>%
  filter(year>1999 & year<2008 ) %>%
  filter(site!="Smolt_Burrishoole" & site!="Smolt_Dee_ManlyHall") %>%
  ggplot(aes(x=year,y=firstday ,group=year))+
  geom_boxplot()+
  stat_summary(geom="line",group=1,col="red")

summary_data %>%
  filter(year>1999 & year< 2008) %>%
  filter(site!="Smolt_Burrishoole" & site!="Smolt_Dee_ManlyHall") %>%
  ggplot(aes(x=year,y=firstday ,group=site,col=site))+
  geom_line()+
  geom_point()
