bike_data <- read.csv("/cloud/project/bike_data/Divvy_Trips_2020_Q1.csv")
View(bike_data)
summary(bike_data)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("janitor")
library(janitor)
str(bike_data)
bike_data <- bike_data %>%
  drop_na()
bike_data_new <- subset(bike_data, select= -c(start_lat, start_lng, end_lat, end_lng))
View(bike_data_new)
bike_data_new <- bike_data_new %>%
  rename(start_time= started_at, end_time= ended_at, users= member_casual)
summary(bike_data_new)
bike_data_new <- bike_data_new %>%
  mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
  )
View(bike_data_new)
bike_data_new <- bike_data_new %>%
  mutate(bike_data_new, ride_duration= end_time-start_time)
View(bike_data_new)
summary(bike_data_new)
bike_data_new <- subset(bike_data_new,ride_duration>=0)
library(lubridate)
bike_data_new$ride_length <- seconds_to_period(bike_data_new$ride_duration) %>% 
  as.duration() %>% 
  hms::hms()
View(bike_data_new)
bike_data_new <- bike_data_new %>%
  mutate(bike_data_new,day_of_week= wday(start_time, label=FALSE, week_start=1))
View(bike_data_new)
result_summary <- bike_data_new %>%
  group_by(users)%>%
  summarize(mean_ride_length=mean(ride_length), median_ride_length=median(ride_length), max_ride_length=max(ride_length), min_ride_length=min(ride_length))
View(result_summary)
library(ggplot2)
ggplot(result_summary, aes(x=users, y=mean_ride_length))+
  geom_bar(stat = "identity", fill="blue", color="black")+
  labs(title= "Mean ride length by users", x="users", y="Mean ride length")
result_summary_1 <- bike_data_new %>%
  group_by(day_of_week)%>%
  summarize(mean_ride_length=mean(ride_length), median_ride_length=median(ride_length), max_ride_length=max(ride_length), min_ride_length=min(ride_length))
View(result_summary_1)
ggplot(result_summary_1, aes(x=day_of_week, y=mean_ride_length))+
  geom_line(color="blue")+
  geom_point(color="blue", size=3)+
  labs(title="Average ride length per day",x="day_of_week", y="Mean ride length")
rides_per_user <- bike_data_new %>%
  group_by(users)%>%
  summarize(number_of_rides=n())
View(rides_per_user)
ggplot(rides_per_user, aes(x=users, y=number_of_rides))+
  geom_bar(stat="identity", fill="yellow", color="black")+
  labs(title="Number of rides per user category", x="users", y="Number of rides")
rides_per_day <- bike_data_new %>%
  group_by(users,day_of_week)%>%
  summarize(number_of_rides=n())
View(rides_per_day)
summary(rides_per_day)
ggplot(rides_per_day, aes(x=day_of_week, y=number_of_rides, fill=users))+
  geom_bar(stat="identity", position="dodge")+
  labs(title="Number of rides per day for each user", x="Day of Week", y="Number of rides")+
  scale_fill_manual(values= c("member"="blue", "casual"="yellow"))+
  theme_minimal()
  