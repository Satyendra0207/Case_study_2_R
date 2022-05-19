library(ggplot2)
library(dplyr) #for pipeline and for printing table info like summary,row ,columns etc
install.packages("skimr")
library(skimr) # for generating summary of the dataset 
install.packages("janitor")
library(janitor)#for cleaning the dirty dataset 
library(tidyverse)
library(lubridate)
library(stringr)
install.packages("here") # to locate file 
library(here)


#importing the required dataset 
daily_activity<-read_csv("D:/google cert/case_2/dailyActivity_merged.csv")
View(daily_activity)

daily_sleep<-read_csv("D:/google cert/case_2/sleepDay_merged.csv")
View(daily_sleep)

hourlyintensities<-read_csv("D:/google cert/case_2/hourlyIntensities_merged.csv")
View(hourlyintensities)

Metsperminute<-read_csv("D:/google cert/case_2/minuteMETsNarrow_merged.csv")
View(Metsperminute)

weightloginfo<-read_csv("D:/google cert/case_2/weightLogInfo_merged.csv")
View(weightlog)


#getting summary of data
colnames(daily_activity)
colnames(daily_sleep)
colnames(hourlyintensities)
colnames(Metsperminute)
colnames(weightlog)

#structure of datasets
str(daily_activity)  #summary() to generate statistical summary of each column
str(daily_sleep)
str(hourlyintensities)
str(Metsperminute)
str(weightloginfo)

#Removing Duplicates 
daily_activity %>% distinct(Id)
daily_sleep %>% distinct(Id)
hourlyintensities %>% distinct(Id)
Metsperminute %>% distinct(Id)
weightloginfo %>% distinct(Id)

#making new dataFrame 
Daily_sleep<-daily_sleep %>% distinct(Id,SleepDay,.keep_all = TRUE)
str(Daily_sleep)

Weightloginfo<-weightloginfo %>% distinct(Id,.keep_all = TRUE)
str(Weightloginfo)

#Changing format of date time column using lubridate library
Daily_activity<-daily_activity
str(Daily_activity)

#changing format of data 
Daily_activity$ActivityDate1<-as.Date(Daily_activity$ActivityDate,format="%m/%d/%Y")
str(Daily_activity)

#fetching weekdays 
Daily_activity$DayName<-weekdays(Daily_activity$ActivityDate1)
str(Daily_activity)


#taking only required columns 
Activitylog<-Daily_activity %>%  select(Id,ActivityDate,DayName,TotalSteps,Calories,SedentaryMinutes)
str(Activitylog)

sleeplog<-Daily_sleep %>% select(Id,SleepDay,TotalTimeInBed,TotalMinutesAsleep)
str(sleeplog)


weightlog<-Weightloginfo %>% select(Id,BMI)
str(weightlog)

#Merging data
MetPerMinutes1<-separate(Metsperminute,ActivityMinute,into=c("Date","Time"),sep=" ")
str(MetPerMinutes1)

#STAGE --4
MetLogs<-MetPerMinutes1 %>%
  group_by(Id,Date) %>% 
  drop_na() %>% #dropping Null values 
  summarise(AvgMETsparday=mean(METs))
str(MetLogs)

#Rename 
colnames(Activitylog)[colnames(Activitylog)=="ActivityDate"]<-"Date"
colnames(sleeplog)[colnames(sleeplog)=="SleepDay"]<-"Date1"
sleeplog$Date<-strtrim(sleeplog$Date1,9)
sleeplog$Date1<-NULL
str(sleeplog)
str(Activitylog)

#merging of required columns

mergedata1<-merge(Activitylog,sleeplog,by=c('Id',"Date"))
str(mergedata1)
mergedata2<-merge(mergedata1,MetLogs,by=c("Id","Date"))
str(mergedata2)

#changing units and values in variables
#changing minutes to hour
mergedata2$Sedentaryhrs<-mergedata2$SedentaryMinutes/60
mergedata2$SedentaryMinutes<-NULL
mergedata2$TotalsleepHrs<-mergedata2$TotalMinutesAsleep/60
mergedata2$TotalMinutesAsleep<-NULL
mergedata2$TotalTimeInBedHrs<-mergedata2$TotalTimeInBed/60
mergedata2$TotalTimeInBed<-NULL
str(mergedata2)#
#merge weights
mergeWt<-merge(mergedata2,weightlog,by="Id")
str(mergeWt)


#extracting 
hourlyintensities
hourlyintensities$Time<-substr(hourlyintensities$ActivityHour,start = 11,stop = 21)
hourlyintensities$Date<-substr(hourlyintensities$ActivityHour,1,9)
hourlyintensities$Date1<-as.Date(hourlyintensities$Date,format = ("%m/%d/%Y"))
hourlyintensities$DayName<-weekdays(hourlyintensities$Date1)
str(hourlyintensities)


#VIZUALIZATION

#VIZ.1:Total Steps on each day of the week
ggplot(mergedata2)+geom_point(mapping=aes(x=DayName,y=TotalSteps),color="Purple")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  labs(title = "Total Steps On Each Day of the Week",
       subtitle = "From Data of 24 Fitbit users")

#VIZ-2--
mergedata2 %>% 
  filter(TotalSteps>8000) %>% 
  ggplot(mapping=aes(x=DayName,y=TotalSteps,fill=DayName))+
  geom_bar(stat="summary",fun="mean")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  labs(title="Avg Total Steps on each day of the week",
       subtitle = "among the people takes more then 8000 daily steps",
       y="Avg Total Steps")+
  theme(legend.position="none")

#VIZ-3 less then 8000
theme_set(theme_bw())
mergedata2%>% 
  filter(TotalSteps<8000) %>% 
  ggplot(mapping=aes(x=DayName,y=TotalSteps,fill=DayName))+
  geom_bar(stat="summary",fun="mean")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  labs(title="Avg Total Steps on each day of the week",
       subtitle = "among the people takes less then 8000 daily steps",
       y="Avg Total Steps")+
  theme(legend.position="none")

#VIZ-4 less then 5000
mergedata2%>% 
  filter(TotalSteps<5000) %>% 
  ggplot(mapping=aes(x=DayName,y=TotalSteps,fill=DayName))+
  geom_bar(stat="summary",fun="mean")+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  labs(title="Avg Total Steps on each day of the week",
       subtitle = "among the people takes less then 5000 daily steps",
       y="Avg Total Steps")+
  theme(legend.position="none")

#VIZ-5---BMI
mergeWt %>% 
  ggplot(aes(DayName,TotalSteps,color=BMI,size=Date))+
  geom_point(alpha=0.5)+
  scale_x_discrete(limits = c("Monday" , "Tuesday", "Wednesday",
                              "Thursday" , "Friday" , "Saturday" , "Sunday"))+
  facet_wrap(~Id)+
  scale_color_viridis_b(option = "C")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Users consistancy in steps and their BMI",
       subtitle = " 22 days Data of 6 Fitbit Users")

#VIZ--6

ggplot(data=mergedata2) +
  geom_point(mapping = aes(x=TotalsleepHrs,y=TotalSteps,
                           color=DayName),size=3) +
  labs(title = "Users consistancy in steps and their Sleep Pattern",
       subtitle = " 22 days Data of 24 Fitbit Users")

#VIZ-7
mergeWt %>% 
  ggplot(aes(DayName,TotalSteps,color=TotalsleepHrs))+
  geom_point(size=3)+
  scale_x_discrete(limits = c("Monday" , "Tuesday", "Wednesday",
                              "Thursday" , "Friday" , "Saturday" , "Sunday"))+
  facet_wrap(~Id)+
  scale_color_viridis_b(option = "C")+
  theme(axis.text.x = element_text(angle = 25))+
  labs(title = "Users consistancy in steps and their Sleep Pattern",
       subtitle = " 22 days Data of 6 Fitbit Users")

#VIZ-8 Total Bed time and Total Sleep time 
ggplot(data=mergedata2) +
  geom_jitter(mapping = aes(x=TotalTimeInBedHrs,y=TotalsleepHrs,color=DayName)) +
  labs(title = "Total bed time and total sleep time On Each Day of the Week", 
       subtitle = "From Data of 24 Fitbit users of 22 Days")+
  annotate("text",x=8,y=3,label="Less Sleeping Set",color="purple",
           fontface="bold",angle=30,size=4)

#VIZ -9Relation between users daily calories burned and avg METs
ggplot(data=mergedata2) +
  geom_point(mapping = aes(x=Calories,y=AvgMETsparday,color=DayName)) +
  theme(legend.position = "none") +
  labs(title = "Relation between users daily calories burned and avg METs",
       subtitle = " 22 Days Data of 24 Fitbit users ",x="Daily Calories Burned")

#VIZ-10-Relation between users daily calories burned and Daily Sedentary Hrs.
ggplot(data=mergedata2) +
  geom_smooth(mapping = aes(x=Sedentaryhrs ,y=Calories)) +
  labs(title = "Relation between users daily calories burned and Daily Sedentary Hrs",
       subtitle = " 22 Days Data of 24 Fitbit users ")

#viz-11 Mean Workout Intensity Per Hour in a Day with 31 Days Data of 33 Fitbit Users.

ggplot(data = hourlyintensities,aes(x=Time,y=TotalIntensity,fill=DayName)) +
  geom_bar(stat = "summary",fun="mean") +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits = c("5:00:00 AM","6:00:00 AM","7:00:00 AM",
                              "8:00:00 AM","9:00:00 AM","10:00:00 AM",
                              "11:00:00 AM","12:00:00 PM","1:00:00 PM",
                              "2:00:00 PM","3:00:00 PM","4:00:00 PM",
                              "5:00:00 PM","6:00:00 PM","7:00:00 PM",
                              "8:00:00 PM","9:00:00 PM","10:00:00 PM" )) +
  labs(title = "Mean Workout Intensity Per Hour in a Day",y="MeanTotalIntensity",
       subtitle="31 Days Data of 33Fitbit Users")
