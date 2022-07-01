##
# 1. Download the libraries needed
##

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(skimr)

##
# 2. Remove any data from the environment
##

rm(list=ls())

##
# 3. Reading data sets
##

df1 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202004-divvy-tripdata.csv")
colnames(df1)
df2 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202005-divvy-tripdata.csv")
colnames(df2)
df3 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202006-divvy-tripdata.csv")
colnames(df3)
df4 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202007-divvy-tripdata.csv")
colnames(df4)
df5 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202008-divvy-tripdata.csv")
colnames(df5)
df6 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202009-divvy-tripdata.csv")
colnames(df6)
df7 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202010-divvy-tripdata.csv")
colnames(df7)
df8 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202011-divvy-tripdata.csv")
colnames(df8)
df9 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202012-divvy-tripdata.csv")
colnames(df9)
df10 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202101-divvy-tripdata.csv")
colnames(df10)
df11 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202102-divvy-tripdata.csv")
colnames(df11)
df12 <- read.csv("C:/Users/Snow/Documents/Data analytics/divvy_trip_analysis/Divyy_Dataset/202103-divvy-tripdata.csv")
colnames(df12)

##
# 4. Make sure that the the columns have the same type
##

compare_df_cols(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,return = "mismatch")

##
# 5. Notice that in df1 until df8  the end_station_id & start_station_id are integer so we parse them into characters
##

df1<-mutate(df1,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df2<-mutate(df2,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df3<-mutate(df3,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df4<-mutate(df4,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df5<-mutate(df5,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df6<-mutate(df6,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df7<-mutate(df7,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
df8<-mutate(df8,start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

##
# 6. Comparing again to make sure that the resulted dataset is consistent
##
compare_df_cols(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,return = "mismatch")

##
# 7. Combining Data sets and remove empty rows and columns if any
##

bike_rides_global <- rbind(df1,df2,df3,df4, df5, df6,df7,df8,df9,df10,df11,df12)
dim(bike_rides_global)
bike_rides_global <- janitor::remove_empty(bike_rides_global,which = c("cols"))
bike_rides_global <- janitor::remove_empty(bike_rides_global,which = c("rows"))
dim(bike_rides_global)

##
# 8. Summary of the data frame
##

skim_without_charts(bike_rides_global)

##
# 9. Processing datetime
##

bike_rides_global$started_at <- lubridate::ymd_hms(bike_rides_global$started_at)
bike_rides_global$ended_at <- lubridate::ymd_hms(bike_rides_global$ended_at)

##
# 10. Creating start and end hour fields
##

bike_rides_global$start_hour <- lubridate::hour(bike_rides_global$started_at)
bike_rides_global$end_hour <- lubridate::hour(bike_rides_global$ended_at)

##
# 11. Creating ride length field
##

bike_rides_global$ride_length_hours <- difftime(bike_rides_global$ended_at,bike_rides_global$started_at,units="hours")
bike_rides_global$ride_length_mins <- difftime(bike_rides_global$ended_at,bike_rides_global$started_at,units="mins")


##
# 11. Creating day of the week fields
##

bike_rides_global$day_of_week_letter <- lubridate::wday(bike_rides_global$started_at,abbr = TRUE,label = TRUE)
bike_rides_global$day_of_week_number <- lubridate::wday(bike_rides_global$started_at)

##
## summary of data
##

skim_without_charts(bike_rides_global)

##
# 12. Remove Na
##
bike_rides_global_no_na  <- drop_na(bike_rides_global)
rm(bike_rides_global)

##
# 13. Remove Duplicates
##

bike_rides_global_no_na <- distinct(bike_rides_global_no_na)

##
# 14. Remove negative ride length and quality check rows
##

bike_rides_global_no_na_length_correct <- bike_rides_global_no_na %>% filter(ride_length_mins>0)
rm(bike_rides_global_no_na)


rm(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

skim_without_charts(bike_rides_global_no_na_length_correct)

write_csv(bike_rides_global_no_na_length_correct,"divvy_dataset_cleaned-v1.csv")
