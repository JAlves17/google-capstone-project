#installs packages
install.packages('tidyverse')

#loads libraries
library(tidyverse)
library(lubridate) #dates
library(hms) #time
library(data.table) #exporting data frame
library(readxl) #read excel files

#loads .xlsx files
jan_01 <- read_xlsx("2022_01_tripdata.xlsx")
feb_02 <- read_xlsx("2022_02_tripdata.xlsx")
mar_03 <- read_xlsx("2022_03_tripdata.xlsx")
apr_04 <- read_xlsx("2022_04_tripdata.xlsx")
may_05 <- read_xlsx("2022_05_tripdata.xlsx")
jun_06 <- read_xlsx("2022_06_tripdata.xlsx")
jul_07 <- read_xlsx("2022_07_tripdata.xlsx")
aug_08 <- read_xlsx("2022_08_tripdata.xlsx")
sep_09 <- read_xlsx("2022_09_tripdata.xlsx")
oct_10 <- read_xlsx("2022_10_tripdata.xlsx")
nov_11 <- read_xlsx("2022_11_tripdata.xlsx")
dec_12 <- read_xlsx("2022_12_tripdata.xlsx")

#merges all of the data frames into a year view
cyclistic_2022 <- rbind (jan_01, feb_02, mar_03, apr_04, may_05, jun_06, jul_07, aug_08, sep_09, oct_10, nov_11, dec_12)

#removes individual month data frames to clear up space in the environment
remove(jan_01, feb_02, mar_03, apr_04, may_05, jun_06, jul_07, aug_08, sep_09, oct_10, nov_11, dec_12)

#creates a new data frame where calculations will take place
cyclistic_data <- cyclistic_2022

#calculates ride length as minutes
cyclistic_data$ride_length <- difftime(cyclistic_2022$ended_at, cyclistic_2022$started_at, units = "mins")

#changes member_casual column name
cyclistic_data <- cyclistic_data %>% 
  rename("membership" = "member_casual")

#creates and formats columns
cyclistic_data$date <- as.Date(cyclistic_data$started_at) #creates new column for date
cyclistic_data$month <- format(as.Date(cyclistic_data$date), "%m") #creates column for month
cyclistic_data$day <- format(as.Date(cyclistic_data$date), "%d") #creates column for day
cyclistic_data$year <- format(as.Date(cyclistic_data$date), "%Y") #creates column for year
cyclistic_data$time <- format(as.Date(cyclistic_data$date), "%H:%M:%S") #formats time as HH:MM:SS
cyclistic_data$time <- as_hms((cyclistic_2022$started_at)) #creates new column for time
cyclistic_data$hour <- hour(cyclistic_data$time) #create new column for hour

#creates column for quarters of the year
cyclistic_data <- cyclistic_data %>% mutate(quarter = 
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
cyclistic_data <-cyclistic_data %>% mutate(time_of_day = 
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

#extra cleaning the data
cyclistic_data <- distinct(cyclistic_data) #removes duplicate rows 
cyclistic_data <- cyclistic_data %>%  #removes unnecessary columns
  select(-c(ride_id, rideable_type, start_station_id, start_station_name, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng))

#view final data
View(cyclistic_data)

#----------------------------------------NUMBER OF RIDES----------------------------------------

#total number of rides
nrow(cyclistic_data)

#by member type
cyclistic_data %>%
  group_by(membership) %>% 
  count(membership)

#-----HOUR-----

#total rides by member type 
cyclistic_data %>%
  group_by(membership) %>% 
  count(hour) %>% 
  print(n = 48) #to view the entire tibble

#total rides
cyclistic_data %>%
  count(hour) %>% 
  print(n = 24) #to view the entire tibble

#----------------------TIME OF DAY-----------------------
#-----morning-----
#total rides by member type 
cyclistic_data %>%
  group_by(membership) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
cyclistic_data %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-----
#total rides by member type 
cyclistic_data %>%
  group_by(membership) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
cyclistic_data %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-----
#total rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
cyclistic_data %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-----
#number of rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
cyclistic_data %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#-----all times of day-----
#total rides by member type 
cyclistic_data %>%
  group_by(membership) %>% 
  count(time_of_day)

#number of rides
cyclistic_data %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------

#total rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  count(day_of_week)

#total rides 
cyclistic_data %>%
  count(day_of_week)

#----------------DAY OF THE MONTH---------------

#total rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  count(day) %>% 
  print(n = 62) #to view the entire tibble

#total rides
cyclistic_data %>%
  count(day) %>% 
  print(n = 31) #to view the entire tibble

#--------------------MONTH----------------------

#total rides by member type 
cyclistic_data %>%
  group_by(membership) %>% 
  count(month) %>% 
  print(n = 24) #to view the entire tibble

#total rides
cyclistic_data %>%
  count(month) 

#-------------------QUARTERS--------------------

#-----Q1-----

#total rides by member type 
cyclistic_data %>%
  group_by(membership) %>% 
  filter(quarter == "1Q") %>% 
  count(quarter)

#total rides
cyclistic_data %>%
  filter(quarter == "1Q") %>% 
  count(quarter)

#-----Q2-----

#total rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  filter(quarter == "2Q") %>% 
  count(quarter)

#total rides
cyclistic_data %>%
  filter(quarter == "2Q") %>% 
  count(quarter)

#-----Q3-----

#total rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  filter(quarter == "3Q") %>% 
  count(quarter)

#total rides
cyclistic_data %>%
  filter(quarter == "3Q") %>% 
  count(quarter)

#-----Q4-----

#total rides by member type
cyclistic_data %>%
  group_by(membership) %>% 
  filter(quarter == "4Q") %>% 
  count(quarter)

#total rides 
cyclistic_data %>%
  filter(quarter == "4Q") %>% 
  count(quarter)

#-----all quarters-----

#total rides by member type
cyclistic_data %>%
  group_by(quarter, membership) %>% 
  count(quarter)

#total rides
cyclistic_data %>%
  group_by(quarter) %>% 
  count(quarter)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_data$ride_length) #saves in variable
print(as_hms(cyclistic_avgRide)) #to get the result in mm:ss format

#------------------MEMBER TYPE--------------------

#average ride_length
avgMember <- cyclistic_data %>% group_by(membership) %>% #saves table on environment
  summarise_at(vars(ride_length),
               list(time = mean))
avgMember$time <- as_hms(avgMember$time) #formats time column as mm:ss
print(avgMember) #to view the result

#-----------------------HOUR-------------------------

#average ride_length by member type
cyclistic_data %>% group_by(hour, membership) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #to view entire tibble

#average ride_length
cyclistic_data %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #to view entire tibble

#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all times of day----

#average ride length by member type
cyclistic_data %>% 
  group_by(time_of_day, membership) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_data %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
cyclistic_data %>% group_by(membership, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
cyclistic_data %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
cyclistic_data %>% group_by(day, membership) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #to view entire tibble

#average ride_length
cyclistic_data %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #to view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
cyclistic_data %>% group_by(month, membership) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
cyclistic_data %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------------QUARTERS----------------------

#-----Q1------

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(quarter == "1Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for the quarter
cyclistic_data %>% 
  filter(quarter == "1Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----Q2------

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(quarter == "2Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for the quarter
cyclistic_data %>% 
  filter(quarter == "2Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----Q3------

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(quarter == "3Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for the quarter
cyclistic_data %>% 
  filter(quarter == "3Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----Q4-----

#average ride length by member type
cyclistic_data %>% 
  group_by(membership) %>% 
  filter(quarter == "4Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for the quarter
cyclistic_data %>% 
  filter(quarter == "4Q") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all quarters----

#average ride length by member type
cyclistic_data %>% 
  group_by(quarter, membership) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for all quarters
cyclistic_data %>% 
  group_by(quarter) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))