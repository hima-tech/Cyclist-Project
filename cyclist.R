## combine all the csv files of last 12 month 

csv1 <- read.csv("./csv/202004-divvy-tripdata.csv")
csv2 <- read.csv("./csv/202005-divvy-tripdata.csv")
csv3 <- read.csv("./csv/202006-divvy-tripdata.csv")
csv4 <- read.csv("./csv/202007-divvy-tripdata.csv")
csv5 <- read.csv("./csv/202008-divvy-tripdata.csv")
csv6 <- read.csv("./csv/202009-divvy-tripdata.csv")
csv7 <- read.csv("./csv/202010-divvy-tripdata.csv")
csv8 <- read.csv("./csv/202011-divvy-tripdata.csv")
csv9 <- read.csv("./csv/202012-divvy-tripdata.csv")
csv10 <- read.csv("./csv/202101-divvy-tripdata.csv")
csv11 <- read.csv("./csv/202102-divvy-tripdata.csv")
csv12 <- read.csv("./csv/202103-divvy-tripdata.csv")

## using rbind function to combine them into biker_ride   
biker_ride <- rbind(csv1, csv2, csv3, csv4, csv5, csv6, csv7, csv8, csv9, csv10, csv11, csv12)


# using lubridate function to change the type of data from chr to date/days
biker_ride$started_at <- lubridate::ymd_hms(biker_ride$started_at)
#biker_ride$ended_at <- lubridate::ymd_hms(biker_ride$ended_at)
biker_ride$day_of_week <- lubridate::wday(biker_ride$day_of_week, label = TRUE)
biker_ride$ride_length <- lubridate::hms(biker_ride$ride_length)
biker_ride$started_hour <- lubridate::hm(biker_ride$started_at)


biker_ride_length <- biker_ride %>% 
add_column(ride_length_min = as.numeric(biker_ride$ride_length)/ 60)  



biker_ride_hour <- biker_ride %>% 
  separate(started_at, c("date", "started_hour"), " ") %>% 
  separate(date,c("started_month","started_day","started_year"), "/") %>% 
  separate(started_hour, c("started_hour","started_minutes"), sep = ":") 
   
biker_ride_hour$started_month <- lubridate::month(biker_ride_hour$started_month, label = TRUE)

View(teast_final)


View(biker_ride_hour)
View(biker_ride_length)
View(biker_ride)
str(biker_ride_hour)




# using summarize and group_by function to know what the total number of every bike model
total_bike <- biker_ride %>% 
  group_by(rideable_type) %>% 
  summarize(bike_total_use = n())
View(total_bike)


# ?tableu
ggplot(data = biker_ride)+
  geom_bar(mapping = aes(x = rideable_type, fill =  rideable_type))+
  labs(x = " bike type ", y = "total number of customers bike usages")




# usinge summarize and group_by function to know total number of bike used by every customer type
biker_ride_type <- biker_ride %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(member_casual_total_use = n())

 View(biker_ride_type)  

# visualization to show the diff  between every customer type bike use 
ggplot(data = biker_ride) +
  geom_bar(mapping = aes( x = rideable_type, fill = member_casual), position = "dodge")+
  labs(title = "bike usage per model last 12 month",subtitle = "difference between casual and member usage over bikes", caption = "this data from the last 12 month")+
  scale_y_continuous(labels = comma)+
  labs(x= "bike type ", y = "total number of bikes usage")



# using summarize and group by function to see the diff between member and casual over days
bike_ride_week <- biker_ride %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise( member_vs_casual_per_day = n()) 
  #arrange(day_of_week)

View(bike_ride_week)


# visualization of every type of customer usage over days
ggplot(data = biker_ride)+
  geom_bar(mapping = aes(x = day_of_week, fill = member_casual), position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(title = "bike usage per day last 12 month", subtitle = "difference between casual and member usage per day", caption = "this data from last 12 month")+
  labs(x = "day", y = "bike usage of customer type per day")




# revised please ??????????
biker_ride_duration <- biker_ride_length %>% 
  group_by(member_casual, day_of_week)  %>% 
  summarize(minimum_minutes = min(ride_length_min), max_minutes = max(ride_length_min), average_minutes = mean(ride_length_min), test = sum(ride_length_min))

View(biker_ride_duration)

ggplot(data = biker_ride_duration)+ 
geom_col( mapping = aes(x = day_of_week , y = average_minutes , fill = member_casual), position = "dodge")+
  labs(x = "day", y = "average bike ridding duration")+
  labs(title = "average duration minutes  ridding a bike last 12 month per day", subtitle = "minutes spent ridding a bike between customer type over days", caption = "this data from last 12 month")+
  scale_y_continuous(labels = label_number( suffix = " AVG"))

