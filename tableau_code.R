#installs packages
install.packages('tidyverse')

#loads libraries
library(tidyverse)
library(lubridate) #dates
library(hms) #time
library(data.table) #exporting data frame

#creates a new data frame for manipulating
cyclistic_tableau <- cyclistic_2022

#calculates ride length as minutes
cyclistic_tableau$ride_length <- difftime(cyclistic_2022$ended_at, cyclistic_2022$started_at, units = "mins")

#changes member_casual column name
cyclistic_tableau <- cyclistic_tableau %>% 
  rename("membership" = "member_casual")

#creates and formats columns
cyclistic_tableau$date <- as.Date(cyclistic_tableau$started_at) #creates new column for date
cyclistic_tableau$month <- format(as.Date(cyclistic_tableau$date), "%m") #creates column for month
cyclistic_tableau$day <- format(as.Date(cyclistic_tableau$date), "%d") #creates column for day
cyclistic_tableau$year <- format(as.Date(cyclistic_tableau$date), "%Y") #creates column for year
cyclistic_tableau$hour <- hour(cyclistic_tableau$started_at) #create new column for hour

#creates column for quarters of the year
cyclistic_tableau <- cyclistic_tableau %>% mutate(quarter = 
                                              case_when(month == "01" ~ "1Q",
                                                        month == "02" ~ "1Q",
                                                        month == "03" ~ "1Q",
                                                        month == "04" ~ "2Q",
                                                        month == "05" ~ "2Q",
                                                        month == "06" ~ "2Q",
                                                        month == "07" ~ "3Q",
                                                        month == "08" ~ "3Q",
                                                        month == "09" ~ "3Q",
                                                        month == "10" ~ "4Q",
                                                        month == "11" ~ "4Q",
                                                        month == "12" ~ "4Q")
)

#creates column for time of day
cyclistic_tableau <- cyclistic_tableau %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)

#changes month column content from number to full month name
cyclistic_tableau <- cyclistic_tableau %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December"
                                             )
)

#extra cleaning the data
cyclistic_tableau <- distinct(cyclistic_tableau) #remove duplicate rows
cyclistic_tableau <- cyclistic_tableau %>% #removes unnecessary columns
  select(-c(started_at, ended_at, ride_id, rideable_type, start_station_id, start_station_name, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng))

#view the final data
View(cyclistic_tableau)

#download data as a .csv file
fwrite(cyclistic_tableau, "cyclistic_data.csv")
