# load tidyverse package helps wrangle data
library(tidyverse)
# load conflicted package
library(conflicted)

# set deplyr filter and dplyr lag as default preference
conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")

# UPLOAD DIVVY datasets (xlsx file) here
q1_2019 = readxl::read_excel("Divvy_Trips_2019_Q1.xlsx")
q1_2020 = readxl::read_excel("Divvy_Trips_2020_Q1.xlsx")

# wrangle and combine the dataset.
# column name for check column
colnames(q1_2019)
colnames(q1_2020)

# change column name for combine

q1_2019 = rename(q1_2019, ride_id = trip_id,
                 rideable_type = bikeid, started_at = start_time,
                 ended_at = end_time, start_station_name = from_station_name,
                 start_station_id = from_station_id, end_station_name = 
                   to_station_name, end_station_id = to_station_id,
                 member_casual = usertype)
#check the data type in the frame for combine files
str(q1_2019)
str(q1_2020)

# change variable ride_id and rideable_type type to chr 

q1_2019 = mutate(q1_2019,ride_id = as.character(ride_id),
                 rideable_type = as.character(rideable_type))
# combine two datasets and remove some irrelevent columns (tripduration
#,gender,birthyear, start_lat, start_lng, end_lat, end_lng)

all_trips = bind_rows(q1_2019,q1_2020)

all_trips = all_trips %>% 
  select(-c(tripduration,gender,birthyear,start_lat, start_lng, end_lat, end_lng))
#inspect new dataset after combine
head(all_trips) # see the tibble of the dataset
last(all_trips) # to inspect data in last row
colnames(all_trips)# inspect column name
dim(all_trips)  #Dimensions of the data frame?
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members 
#("member" and "Subscriber") and two names for casual riders 
#("Customer" and "casual"). We will need to consolidate that from four 
#to two labels.


# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips = all_trips %>% 
  mutate(member_casual = recode(member_casual, "Subscriber" = "member", 
                                "Customer" = "casual"))

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ...
#before completing these operations we could only aggregate at the ride level

all_trips$date = as.Date(all_trips$started_at)# default date format yyyy-mm-dd
all_trips$month = format(as.Date(all_trips$started_at), "%m")# only month
all_trips$year = format(as.Date(all_trips$started_at), "%y")
all_trips$day = format(as.Date(all_trips$started_at), "%d")
all_trips$day_of_week = wday(all_trips$started_at,1)

# add a row ride_length

all_trips$ride_length = difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations 
#on the data
is.factor(all_trips$ride_length)
all_trips$ride_length = as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# inspect ride_length that has negative value
print(-all_trips$ride_length)# The dataframe includes a few hundred entries
#when bikes were taken out of docks and checked for quality by Divvy or 
#ride_length was negative

#remove those negative from the dataset
all_trips_v2 = all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$
                             ride_length < 0),]

# analyse mean median max min specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual+all_trips_v2$
            day_of_week, FUN = mean)
# Let's visualize the number of rides by rider type
all_trips_v2 %>% group_by(member_casual,day_of_week) %>%  summarise(num_of_rides 
                            = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual,day_of_week) %>% 
  ggplot(mapping = aes(x=day_of_week,y=num_of_rides,fill = member_casual))+geom_col(position = "dodge")+
  labs(title ="no of rides per day") 

# Let's visualize the average duration taken per day
all_trips_v2 %>% group_by(member_casual,day_of_week) %>%  summarise(num_of_rides 
 = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual,day_of_week)%>%  
  ggplot(aes(x=day_of_week,y=average_duration,fill = member_casual))+geom_col(position = "dodge")+
  labs(title = "average duration per day")+annotate()
