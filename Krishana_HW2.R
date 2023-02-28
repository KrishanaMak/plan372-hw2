# Load libraries
library(tidyverse)
library(lubridate)
        
# load data
fire_incidents = read_csv('Fire_Incidents.csv')

# 1. How long does it take Wake County Fire to respond to incidents, on average 
# (i.e. the time between when an incident is dispatched and when firefighters arrive 
# on the scene)? (hint: you can subtract lubridate date columns from each other).

# Try to use lubridate library to parse the dates
fire_incidents = mutate(fire_incidents, arrive_date_time = ymd_hms(arrive_date_time))

fire_incidents = mutate(fire_incidents, dispatch_date_time = ymd_hms(dispatch_date_time))

# After using lubridate, calculate the mean of the difference.

fire_incidents = mutate(fire_incidents, time_difference = arrive_date_time - dispatch_date_time)

mean(fire_incidents$time_difference, na.rm = T)

# Time difference of 318.7497 secs

# 2. Does this response time vary by station? What stations have the highest and 
# lowest average response times? 

# First I wrote a code to create a new table called station avg.I wanted to group the
# data by the station number so I can figure out the average response time for each
# station. Then I used the time_difference column from the fire_incidents table to 
# find the average reponse time for each station. I titled this new column in the 
# station_avg as average_station_response. Finally, I added an extra line of code to
# sort the average station response times so that I could better view the results.
# I ignored the NA values. 

station_avg = group_by(fire_incidents, station) %>%
  summarize(
    average_station_response=mean(time_difference, na.rm = T))

# The time does vary by station. The station with the lowest average response time is station 13 with 223 seconds and the station 
# with the highest response time is station 29 with 495.7640 seconds.

# 3. Have Wake County Fire’s response times been going up or down over time? What 
# might be the reason for these changes?

# First I used the floor_date function to get the years of the dispatch time. 
# I called this new column year. 
fire_incidents$year = floor_date(fire_incidents$dispatch_date_time, unit="year")

# I created another table called yearly_avg which grouped the fire_incidents table 
# by year. Then I found the mean of the time_difference column and called it
# average_time_per_year. I ignored the NA values.
yearly_avg = group_by(fire_incidents, year) %>%
  summarize(
    average_time_per_year=mean(time_difference, na.rm = T))

# Finally, I create a line plot using the year as the x axis and using the average_time_per_year
# column as the y axis.
ggplot(yearly_avg, aes(x=year, y=average_time_per_year)) +
  # and add a line plot to it
  geom_line()

# The response times have varied through out the years. There are some periods
# where the response time went up and there are others when the response time
# went down. The response time went down during the periods of 2008-2009, 2012-2013,
# 2013-2014, 2014-2015,2016-2017, 2019-2020, 2020-2021, and 2021-2022. For the other
# period response time went up, there was a huge spike in response time from 2018-2019.
# I am not too sure about what could have increased the response time so much during
# the 2018-2019 period, it could possibly be because a station might have been closed 
# for a while. The decreases could be that more fire stations might have been created,
# more first responders were hired, and more vehicles became available. 

# 4. At what times of day are fire calls most likely to occur? 

# First I used the hour function to create a new column of all the hours with in 
# the dispatch date time column.
fire_incidents$hour = hour(fire_incidents$dispatch_date_time)

# Next I create a new table called time_trends. Here I grouped the table by hour
# Then I summarized it based on the number of occurances that these fire calls occured
# with in that given hour.
time_trends = group_by(fire_incidents, hour) %>%
  summarize(
    number_of_fire_calls = n())

# Finally, I create a bar chart to visualize this data. I used hours as my x axis and 
# number_of_fire_calls as my y axis.
ggplot(time_trends, aes(x=hour, y=number_of_fire_calls)) +
  geom_col()

# The most fire calls seem to be made during the night. Usually they are around the times of 9-11 pm. 

# 5. The dataset contains all types of fire department calls, other than emergency 
# medical services (which are removed to protect privacy). The codes for the 
# different incident types can be found on page 3-22 of the National Fire Incident 
# Reporting System Complete Reference Guide. How many calls to Wake County Fire are 
# recorded in this dataset, and how many of them are actual fires? 

# To answer this question I first found all the codes that respond to actual fires.
# I assigned this to a 'list' called vals. I am not sure if c() creates a list, but
# this is a method I found on stackoverflow.
vals <- c(111,112,113,114,115,116,117,118,121,122,123,120,131,132,133,134,135,136,137,138,130,141,142,143,140,151,152,153,154,155,150,161,162,163,164,160,171,172,173,170,100)

# Then I created a new table called fire_call_occurances where the only data that showed was ones that
# had the incident types of actual fire codes. I did this by using the filter function.
fire_call_occurances = filter(fire_incidents, incident_type %in% vals)

# Then I found the total amount of calls where they were only for actual fire calls vs all calls.
# I did this by using the nrow() function.
nrow(fire_call_occurances)
nrow(fire_incidents)

# There are a total of 229047 calls recorded in the data. However, of those calls 
# only 17230 are actual fire calls.

# 6. It is reasonable that firefighters might respond more quickly to some types 
# of incidents than others (e.g., a building fire, code 111 might be higher priority 
# than a cat stuck in a tree, code 542). Using the reference guide linked above to 
# determine appropriate incident codes, evaluate the average response time to actual 
# fires. Is this response time faster than the average response time for all incidents?

# First I created a new table called response_time_actual_fires. Here is used data
# from the fire_call_occurances table that I had created which filtered for actual
# fire codes. Then I grouped the average time differences based on the incident type
# and called that column incident_response_time. I ignored the NA values
response_time_actual_fires = group_by(fire_call_occurances) %>%
  summarize(incident_response_time = mean(time_difference, na.rm = T))

# Finally, I calculated the mean of the incident_response_time, ignoring the NA values.
mean(response_time_actual_fires$incident_response_time, na.rm = T)


# The average response time for calls with actual fires is 310.9914 seconds,while
# the average for all calls was 318.7497 secs

# 7. Repeat the analysis for questions 2-4 for actual fires, rather than all incidents.

# 7-2
# Question 2 asked about whether response time varies by station and which station
# Had the highest and lowest response times. To do this I create a new table
# title station_avg_actual_fire. I used the data from fire_call_occurances since
# it was already filtered with actual fire calls. I then grouped it by station.
# Finally, I found the average response time for each station and made sure to exclude
# NA values so that it did not mess up my results. 

station_avg_actual_fire = group_by(fire_call_occurances, station) %>%
  summarize(
    average_station_response=mean(time_difference, na.rm = T))

# Looking at the station_avg_actual_fire table the lowest response time was
# 232.7666 seconds for station 3 and the highest response time was 587.3797 secs
# for station 23.


# 7-3

# Question 3 asked if response times had been going up or down over time? What might 
# be the reason for these changes. To do this I created a new table called yearly_avg_actual_fire.
# I grouped it by the year column that I had originally created in question three. 
# I used the data from fire_call_occurance. I summarized it by finding the mean of
# the time_difference column and calling it average_time_per_year.

yearly_avg_actual_fire = group_by(fire_call_occurances, year) %>%
  summarize(
    average_time_per_year = mean(time_difference, na.rm = T))

# Then I created a line plot with year in the x axis and average_time_per_year on the
# y axis. 
ggplot(yearly_avg_actual_fire, aes(x=year, y=average_time_per_year)) +
  # and add a line plot to it
  geom_line()

# There were many fluctuations within this line graph as well. Response times
# increased from 2007-2009, 2011-2012, 2013-2014, 2015-2016, and 2017-2019.
# During the other times frames response time decreased. As I stated earlier increases
# could have been because a station might have been closed for a while. The decreases 
# could be that more fire stations might have been created,more first responders 
# were hired, and more vehicles became available.

# 7-4

# Question 4 asked about which time of day do fires occur the most. To answer this
# question I created a new table titled time_trends_actual_fire. It used data from
# the fire_call_occurances dataframe which was already filtered. Then I grouped the 
# data by hour and finally summarized it using the n() function to see how many instances
# of those hours there were. I titled that column number_of_fire_calls.

time_trends_actual_fire = group_by(fire_call_occurances, hour) %>%
  summarize(
    number_of_fire_calls = n())

# Finally, I created a bar chart with hour as the x axis and number_of_fire_calls 
# as the y axis.
ggplot(time_trends_actual_fire, aes(x=hour, y=number_of_fire_calls)) +
  geom_col()

# Looking at the graph most fires seem to occur at night usually from 8-10 pm.


