#loading the libraries
library(tidyverse)
library(lubridate)
library(janitor)

#creating a data frame named daily_activity and loading the data
daily_activity <- read.csv("C:/Users/begba/Desktop/Capstone2B/dailyActivity_merged.csv")
class(daily_activity)

#creating another dataframe for sleep data
sleep_day <- read.csv("C:/Users/begba/Desktop/Capstone2B/sleepDay_merged.csv")

head(daily_activity)
colnames(daily_activity)

head(sleep_day)
colnames(sleep_day)

#verifying in which data more participants
n_distinct(daily_activity$Id)#35
n_distinct(sleep_day$Id)#24

#verifying howmany observations are there in each data frame
nrow(daily_activity)#457
nrow(sleep_day)#413

#summary statistics
daily_activity%>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes)%>%
  summary()

sleep_day%>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed)%>%
  summary()

ggplot(data = daily_activity,aes(x=TotalSteps,y=SedentaryMinutes))+
  geom_point()

ggplot(data=sleep_day,aes(x=TotalMinutesAsleep, y= TotalTimeInBed))+
  geom_point()

combined_data<-merge(sleep_day,daily_activity,by="Id")
n_distinct(combined_data$Id)

#Scatter plot to visualize the relationship
ggplot(combined_data,aes(x= TotalMinutesAsleep,y=TotalSteps))+
  geom_point(alpha=0.4)+
  geom_smooth(method = "lm",col="blue")+
  labs(title="Relationship between sleep duration and Steps per Dat",
       x= "Total minutes asleep",
       x= "Total Steps")+
  theme_minimal()

#Calculating the correlation coefficient
correlation <-cor(combined_data$TotalMinutesAsleep,combined_data$TotalSteps,use = "complete.obs")
print(paste("Correlation between minutes asleep and steps",round(correlation, 3)))#-0.052 correlation is negative so it means that more sleep fewer steps

# Summary statistics based on groups
combined_data <- combined_data%>%
  mutate(SleepGroup = case_when(
    TotalMinutesAsleep<360~"Short",
    TotalMinutesAsleep>=360 & TotalMinutesAsleep <= 480~"Normal",
    TotalMinutesAsleep>480 ~"Long"
  ))
combined_data%>%
  group_by(SleepGroup) %>%
  summarise(AvgSteps = mean(TotalSteps, na.rm=TRUE),
            Count=n())%>%
  arrange(desc(AvgSteps))#these tells us if people who sleep more or less tend to walk more or less
write.csv(combined_data, "C:/Users/begba/Desktop/Capstone2B/cleaned_combined_data.csv")
            