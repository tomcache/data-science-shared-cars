# -----------------------------------------------------------------
# 
# PH125 - Capstone II. Data Visualization and Analysis of Shared Car
# data from Tel Aviv
#
# This project uses as its data source a dataset provided on Kaggle.
# 
# The section is used to download and process a current version of this
# data, process it into a form suitable for analysis for this project,
# and upload the modified dataset to github.
#
# Note: in order to access the dataset on Kaggle, you must authenticate
# (sign in) to the site on the computer you are using to download!
#
#
# Load required packages: 
#
#
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")



dl <- tempfile()
download.file("https://www.kaggle.com/doit-intl/autotel-shared-car-locations/downloads/autotel-shared-car-locations.zip", dl)

sharedAutos <- read.csv("autotel/sample_table.csv", stringsAsFactors = FALSE)

#------------------------------------------------------------------
#
# Data Exploration and Visualization

# Take a look at our data:
glimpse(sharedAutos)

summary(sharedAutos)

length(unique(sharedAutos$carsList))

length(unique(sharedAutos$latitude))

length(unique(sharedAutos$longitude))

length(unique(sharedAutos$total_cars))

# -----------------------------------------------------------------
#
# Data formatting - we will add some metrics to make our analysis easier

db <- read_csv("autotel//sample_table.csv") %>% 
  mutate(timestamp = as.POSIXct(timestamp)) %>% 
  mutate(timestamp = timestamp + 3600*2, # local time = GMT + 2
         Hour = lubridate::hour(timestamp),
         Minute = lubridate::minute(timestamp),
         Weekday = lubridate::wday(timestamp)
  )

dbgrid <- db %>%
  mutate(Grid_lat = round(latitude,4),
         Grid_long = round(longitude,4)
 )

dbgrid_day1 <- dbgrid %>% filter(as_date(timestamp) == sample_day1 )

# Take a look at the Timestamp and get it into the correct format
class(sharedAutos$timestamp)
sharedAutos$timestamp <- ymd_hms(sharedAutos$timestamp)
class(sharedAutos$timestamp)

# What is the range of dates in our database?
min(sharedAutos$timestamp)
max(sharedAutos$timestamp)

# Create some filters: day, week , month - perhaps even hour by hour

#For our first sample, we choose December 15, 2018:
sample_day1 <- ymd("2018-12-15")

sharedAutos_day <- sharedAutos %>% filter(as_date(timestamp) == sample_day1 )

# We'll add a column for hour-by-hour analysis:

sharedAutos_day <- mutate(sharedAutos_day, Hour = hour(timestamp))

# sharedAutos_day %>% ggplot(aes(longitude, latitude, color = total_cars)) + geom_point() +
#   scale_color_gradientn(colours = rainbow(5))

#We can plot this data, to see the distribution of locations, as well
# as the distribution of cars. But as a summary, this doesn't really
# tell us much.

sharedAutos_day %>% ggplot(aes(longitude, latitude, color = total_cars)) + geom_point() +
  scale_color_gradient(low="blue", high="red", guide = "legend")

# So we will facet by hour, and only show where the cars are:

sharedAutos_day %>% filter(total_cars > 0) %>% 
  ggplot(aes(longitude, latitude, color = total_cars)) + geom_point() +
  scale_color_gradient(low="blue", high="red", guide = "legend") +
  facet_wrap(~Hour)
  

# and contrast this with a map of where the cars aren't:
sharedAutos_day %>% filter(total_cars == 0) %>% 
  ggplot(aes(longitude, latitude, color = total_cars)) + geom_point() +
  facet_wrap(~Hour)

# Let's add a column that tells us when a trip originates from a location:

sharedAutos_day <- mutate(sharedAutos_day, trip_start = (total_cars - lag(total_cars) > 0))

# OK - lets plot trip originations:
sharedAutos_day %>% filter(trip_start == TRUE) %>% 
  ggplot(aes(longitude, latitude, color = total_cars)) + geom_point() +
  facet_wrap(~Hour)

# Not for the faint of heart (or CPU) - lets plot the location data:
sharedAutos %>% ggplot(aes(longitude, latitude, color = total_cars)) + geom_point()

# Lets count the number of samples with different car total counts:
carCounts_day <- sharedAutos_day %>% group_by(total_cars) %>% tally()
carCounts_day

# Lets count by location the distribution of car counts:
carCounts_loc <- sharedAutos_day %>% group_by(longitude, latitude) %>%
  count(total_cars, sort = TRUE)
carCounts_loc

hist(carCounts_loc$n)

carCounts_aloc <- sharedAutos_day %>% group_by(longitude, latitude) %>%
  tally(total_cars)

head(carCounts_aloc)

hist(carCounts_loc$n)


# to better understand car demand, this analysis will first create a grid 
# over the city of approximately 100M square, and aggregate the statistics 
# for the number of cars parked within each grid.
#
# We will then use these data to compute the number of trip originations
# vs. number of cars available to get a utilization, or demand factor. We 
# want to assess the number of grids with high utilization as a way to 
# understand where there aren't enough cars, as well as grids with low
# utilization, where there is an excess of cars. 
#
# For the purposes of this study, resolving the grid to a precision of 
# 0.001 unit of latitude and longitude will give us the size (~ 110 x 90M)
# we are looking for:

dbgrid_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>% summarize(grid_total_cars = sum(total_cars)) %>%
  ggplot(aes(Grid_long, Grid_lat, color = grid_total_cars)) + geom_point() +
  facet_wrap(~Hour)

cars_by_grid <- dbgrid_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>% 
  summarize(grid_total_cars = sum(total_cars))

(max(cars_by_grid$grid_total_cars))

(max(db$total_cars))

(sum(db$total_cars))

(sum(cars_by_grid$grid_total_cars))

hist(db$total_cars, col = "slategray4")

hist(cars_by_grid$grid_total_cars, 
     xlim = c(0,100),
     col = "slategray4",
     breaks = c(0,1,2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,3000))

plot(density(cars_by_grid$grid_total_cars))

# Calculate ho many cars leave and enter a grid by timestamp:
dbcars <-  db %>% 
  select(carsList) %>% 
  distinct() %>% 
  mutate(cars = sub(carsList, pattern = "\\[", replacement = "")) %>% 
  mutate(cars = sub(cars, pattern = "\\]", replacement = "")) %>% 
  separate(cars, sep = ",", into = c("car1", "car2", "car3", "car4", "car5", "car6", "car7", "car8", "car9", "car10"), fill = "right") %>% 
  mutate_at(2:11, as.numeric)
# And, let's have a list of all cars "names" (which is a number)
CarsList <- (1:max(dbcars$car1, na.rm = T))[1:max(dbcars$car1, na.rm = T) %in% dbcars$car1]
# While there are 261 cars, some numbers are not in use (probably anymore), and we'll find to have the last car as # 272

# Now we can create a dataframe where each row is only for a specifc car, whose number will be in a new column: "Car"
# Also, we don't need to have the data on each car every 2 minutes, as most of the day it does not move
# So we'll have a row only for each change in location of a car

dbByCar <- left_join(db, dbcars) %>% 
  gather(CarI, Car, car1:car10)  %>% 
  filter(!is.na(Car)) %>% 
  group_by(Car) %>% 
  arrange(timestamp) %>% 
  # let's disregard small changes in location which are probably due to GPS errors
  mutate(Delta = abs(latitude - lag(latitude, default = 0)) + 
           abs(longitude - lag(longitude, default = 0)) > 0.002) %>% 
  mutate(
    LastRideTime = ifelse(Delta == TRUE, as.numeric(difftime(timestamp, lag(timestamp), units = "mins")), 0) # assuming ride time = time car is not available in the db
  ) %>% 
  filter(Delta == TRUE) %>% # only take rows where the location changed
  mutate(
    StartTime = timestamp - 60*LastRideTime,
    StartLng = lag(longitude), StartLat = lag(latitude),
    Day = lubridate::day(StartTime)
  ) %>% 
  ungroup()


dbByCarGrid <- dbByCar %>%
  mutate(Grid_lat = round(latitude,4),
         Grid_long = round(longitude,4)
  )

dbByCar %>% group_by(Grid_lat, Grid_long,)


sample_summary <- sharedAutos_day %>% group_by(latitude, longitude) %>% summarize(n = n())

hist(sample_summary$n)







#------------------------------------------------------------------
# 
# Prediction system



# We should quantize the location data at different levels of granularity.
# For simplicity, we will generate a table for each, so that we can keep
# track



# There's a lot of granularity in the location data

# What are some of the questions / visualizations that we can extract
# from these data?

# TIME

# First we need to look at the timestamp format: 2019-01-10 11:45:55.070781 UTC

# POSITION

# FLOW

#------------------------------------------------------------------
#
# Presentations
#
# FUN WITH MAPS

# First, define the box for Tel Aviv:
# tav_top <- 32.1500
# tav_left <- 34.7000
# tav_bot <- 32.0000
# tav_right <- 34.8500

tav_top <- 32.3500
tav_left <- 34.6000
tav_bot <- 31.8000
tav_right <- 35.1000

height <- tav_top - tav_bot
width <- tav_right - tav_left
sac_borders <- c(bottom  = tav_bot  - 0.1 * height, 
                 top     = tav_top  + 0.1 * height,
                 left    = tav_left - 0.1 * width,
                 right   = tav_right + 0.1 * width)

map <- get_stamenmap(sac_borders, zoom = 10, maptype = "watercolor")

ggmap(map)

tav_borders <- c(left = tav_left,
                  right = tav_right,
                  top = tav_top,
                  bottom = tav_bot)

tav_map <- get_stamenmap(bbox = tav_borders, zoom = 11)

ggmap(tav_map)

telaviv <- geocode("Tel Aviv")

telaviv_ggl_road_map <- qmap("telaviv", zoom = 12, source = "google", maptype = "roadmap")
