# your code g# 1. Download the libraries needed
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(lubridate)
library(geosphere)


bike_rides <- read_csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/bike_rides_cleaned-v2.csv")


# Trips distribution per type of rider

bike_rides %>%
  group_by(member_casual) %>%
  summarize(number_trips=n()) %>%
  mutate(percentage=round(number_trips/sum(number_trips),3))%>%
  ggplot(aes(x="", y=percentage, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual("Type of Rider", values = c("casual" = "#97CFD0", "member" = "#CF3E53"))+
  coord_polar("y", start=0) +
  geom_text(aes(label = percent(percentage)),
            position = position_stack(vjust = 0.5)) +
  labs(x ="", y = "", title = "Trips distribution per type of rider", fill="Type of rider") +
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

# Bike usage between casual and annual riders

bike_rides %>%
  group_by(member_casual,rideable_type) %>%
  summarize(number_trips=n()) %>%
  ggplot() +
  geom_bar(aes(x=member_casual, y=number_trips, fill=member_casual),stat="identity") +
  facet_wrap(~rideable_type) +
  labs(x ="Type of Rider", y = "Number of trips", title = "Bike usage between casual and annual riders", fill="Type of rider") +
  scale_fill_manual("Type of Rider", values = c("casual" = "#97CFD0", "member" = "#CF3E53"))+
  theme_bw()

# Rides distribution per season and type of riders
# between April 2020 and March 2021

bike_rides$start_date <- as.Date(bike_rides$started_at, format = "%Y-%m-%d")

getSeason <- function(DATES) {
  WS <- as.Date("2020-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2020-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2020-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2020-9-21",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2020-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

bike_rides$season <- getSeason(bike_rides$start_date)

# Season riding distribution per member and casual
#members 

bike_rides %>%
  filter(member_casual=="member") %>%
  group_by(season) %>%
  summarize(number_trips=n()) %>%
  mutate(percentage=round(number_trips/sum(number_trips),3))%>%
  ggplot(aes(x="", y=percentage, fill=season)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(label = percent(percentage)),
            position = position_stack(vjust = 0.5)) +
  labs(x ="", y = "", title = "Rides distribution per season \nfor members between April 2020 and March 2021", fill="Season") +
  scale_fill_manual("Season", values = c("Fall" = "#F5C47F", "Spring" = "#B9CA5D", "Summer"="#E15759", "Winter"="#97CFD0"))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

# Casuals

bike_rides %>%
  filter(member_casual=="casual") %>%
  group_by(season) %>%
  summarize(number_trips=n()) %>%
  mutate(percentage=round(number_trips/sum(number_trips),3))%>%
  ggplot(aes(x="", y=percentage, fill=season)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(label = percent(percentage)),
            position = position_stack(vjust = 0.5)) +
  labs(x ="", y = "", title = "Rides distribution per season \nfor casuals between April 2020 and March 2021", fill="Season") +
  scale_fill_manual("Season", values = c("Fall" = "#F5C47F", "Spring" = "#B9CA5D", "Summer"="#E15759", "Winter"="#97CFD0"))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

# Average riding length per month

bike_rides$start_month <- lubridate::month(bike_rides$started_at, abbr = TRUE, label=TRUE)

bike_ride_avg_month <- bike_rides %>%
  group_by(start_month, member_casual) %>%
  summarise(average_ride_length = mean(ride_length_mins), across())


bike_rides %>%
  group_by(start_month, member_casual) %>%
  summarise(average_ride_length = mean(ride_length_mins), across()) %>%
  ggplot()+
  geom_line(aes(x = start_month,
                y = average_ride_length,
                colour=member_casual, group=1)) +
  labs(title = "Compare Riding Time Between Annual Members and Casual Riders",
       x = "Month",
       y = "Average riding time in minutes",
       colour="Type of Rider") +
  scale_color_manual("Type of Rider", values = c("casual" = "#97CFD0", "member" = "#CF3E53"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=12, margin=margin(0,0,15,0)))+
  facet_grid(member_casual ~ .,labeller = labeller(average_ride_length="Average riding time in minutes", start_month = "Month")) 


# Calculate the average distance for casuals and members

bike_rides$distance_km <-distHaversine(cbind(bike_rides$start_lng, bike_rides$start_lat), cbind(bike_rides$end_lng, bike_rides$end_lat))/1000

bike_rides %>%
  group_by(member_casual, start_month) %>%
  summarize(average_distance_km=mean(distance_km)) %>%
  ggplot() +
  geom_line(aes(x = start_month,
                y = average_distance_km,
                colour=member_casual, group =1)) +
  labs(title = "Average Riding distance Between Annual Members and \n Casual Riders Accross the year",
       x = "Month",
       y = "Average riding distance (KM)",
       colour="Type of Rider") +
  scale_color_manual("Type of Rider", values = c("casual" = "#97CFD0", "member" = "#CF3E53"))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=12, margin=margin(0,0,15,0)))+
  facet_grid(member_casual ~ .,labeller = labeller(average_distance_km="Average riding distance (KM)", start_month = "Month")) 
oes here