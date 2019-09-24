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
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")



dl <- tempfile()

# From github:

dURL <- "https://github.com/tomcache/data-science-shared-cars/raw/master/datasource/autotel-shared-car-locations.zip"

download.file(dURL, destfile = dl, method = "wininet")

unzip(dl, overwrite = TRUE, exdir = "autotel")

# Create our database file from the supplied sample data:

db <- read_csv("autotel//sample_table.csv")
  

#------------------------------------------------------------------
#
# Data Exploration and Visualization
#
# Section 1: Data preparation and organization

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
# We need to fix the timestamp format, we set to local time, add metrics for time of day / week

db <- read_csv("autotel//sample_table.csv") %>% 
  mutate(timestamp = as.POSIXct(timestamp)) %>% 
  mutate(timestamp = timestamp + 3600*2, # local time = GMT + 2
         Hour = lubridate::hour(timestamp),
         Minute = lubridate::minute(timestamp),
         Weekday = lubridate::wday(timestamp)
  )

# We next add a grid identifier, using a grid size of approximately 10m square

dbgrid <- db %>%
  mutate(Grid_lat = round(latitude,4),
         Grid_long = round(longitude,4),
         gridKey = (Grid_lat*10000000000 + Grid_long*10000)
 )

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

# create a list of entries separated by car, and add grid information:

# this grid is coarse, roughly 1km^2 in area:

dbByCarGridL <- left_join(db, dbcars) %>% 
  gather(CarI, Car, car1:car10)  %>% 
  filter(!is.na(Car)) %>% 
  group_by(Car) %>% 
  arrange(timestamp) %>% 
  mutate(Grid_lat = round(latitude,2),
         Grid_long = round(longitude,2)
  ) %>%
  mutate(gridKey = (Grid_lat*10000000000 + Grid_long*10000)) %>%
  ungroup()
  
  # this is a finer-grained grid, 10m x 9m
  
  dbByCarGridF <- left_join(db, dbcars) %>% 
    gather(CarI, Car, car1:car10)  %>% 
    filter(!is.na(Car)) %>% 
    group_by(Car) %>% 
    arrange(timestamp) %>% 
    mutate(Grid_lat = round(latitude,4),
           Grid_long = round(longitude,4)
    ) %>%
    mutate(gridKey = (Grid_lat*10000000000 + Grid_long*10000)) %>%
  ungroup()  
  
# range of full dataset sample:
min(db$timestamp)
max(db$timestamp)
  
#------------------------------------------------------------------
#
# Demand analysis
db %>% group_by(timestamp) %>% summarize(cars_available = sum(total_cars)) %>%
    ggplot(aes(timestamp, cars_available)) + geom_line(color = "slategray4", size = 1.0) +
    geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

dbg_day1 %>% group_by(timestamp) %>% summarize(cars_available = sum(total_cars)) %>%
  ggplot(aes(timestamp, cars_available)) + geom_line(color = "slategray4", size = 1.0)

db_week %>% group_by(timestamp) %>% summarize(cars_available = sum(total_cars)) %>%
  ggplot(aes(timestamp, cars_available)) + geom_line(color = "slategray4", size = 1.0) +
  geom_smooth(method="glm", se=TRUE, fullrange=TRUE, level=0.95)
  
#------------------------------------------------------------------
#
# Create cars summary by grid:
dbGridSum <- dbgrid %>% group_by(timestamp, gridKey) %>% summarize(carTotal = sum(total_cars)) 
  
# dbGridSum %>% ggplot(aes(gridKey, carTotal, group = timestamp)) + geom_line()  


#------------------------------------------------------------------
#
# Create train & test sets, as well as single day set for rapid testing and development
#
# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = dbgrid$total_cars, times = 1, p = 0.1, list = FALSE)
db_train <- dbgrid[-test_index,]
db_test <- dbgrid[test_index,]

# We will now create a single-day data set, and do the same thing:

sample_day1 <- ymd("2018-12-15")

dbg_day1 <- dbgrid %>% filter(as_date(timestamp) == sample_day1 )

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = dbg_day1$total_cars, times = 1, p = 0.1, list = FALSE)
dbg1_train <- dbg_day1[-test_index,]
dbg1_test <- dbg_day1[test_index,]

# and a 1-week data set, from 12/20/18 through 12/26/18:
week_day1 <- ymd("2018-12-20")
week_day7 <- ymd("2018-12-26")

db_week <- dbgrid %>% filter(timestamp >= week_day1 & timestamp <= week_day7 )



#------------------------------------------------------------------
#
# For our initial prediction model, we need to compute the average car availability
# by grid:

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


dbTrainMean <- db_train %>% summarize(Mean = mean(total_cars)) 
mu_dbTrain <- mean(db_train$total_cars)

dbG1TrainMean <- dbg1_train %>% summarize(Mean = mean(total_cars)) 
mu_dbg1Train <- mean(dbg1_train$total_cars)

(baseline_rmse <- RMSE(db_test$total_cars, mu_dbTrain))

(baseline_rmse1 <- RMSE(dbg1_test$total_cars, mu_dbTrain))


# Just for giggles, lets try the whole enchilada:
# First on our dev set:
mod0 <- lm(total_cars ~ Hour + gridKey + Weekday, data = dbg1_train)

predictions0 <- predict(mod0, dbg1_test)

RMSE(dbg1_test$total_cars, predictions0)

# Then the full set:
mod1 <- lm(total_cars ~ Hour + gridKey + Weekday, data = db_train)

predictions <- predict(mod1, db_test)

RMSE(db_test$total_cars, predictions)

# WE can now explore some additional features of the caret package:

# first we need to convert our outcome to a factor:

dbg1_train[["total_cars"]] = factor(dbg1_train[["total_cars"]])


train_glm1 <- train(total_cars ~ Hour + gridKey + Weekday, method = "glm", data = dbg1_train)
train_knn1 <- train(total_cars ~ Hour + gridKey + Weekday, method = "knn", data = dbg1_train)



y_hat_glm <- predict(train_glm1, dbg1_test, type = "raw")
y_hat_knn <- predict(train_knn1, dbg1_test, type = "raw")
  
confusionMatrix(y_hat_glm, dbg1_test$total_cars)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, dbg1_test$total_cars)$overall[["Accuracy"]]

(glm_RMSE <- RMSE(dbg1_test$total_cars, y_hat_glm))

(knn_RMSE <- RMSE(dbg1_test$total_cars, y_hat_knn))

#------------------------------------------------------------------
# 
# Single-day analysis

#For our first sample, we choose December 15, 2018 (Tuesday):
sample_day1 <- ymd("2018-12-15")

# dbgrid_day1 <- dbgrid %>% filter(as_date(timestamp) == sample_day1 )

#For our second sample, we choose December 18, 2018 (Friday):
sample_day2 <- ymd("2018-12-21")

#For our third sample, we choose December 15, 2018 (Wednesday):
sample_day3 <- ymd("2019-01-09")


dbByCarGridL_day1 <- dbByCarGridL %>% filter(as_date(timestamp) == sample_day1 )

dbByCarGridL_day2 <- dbByCarGridL %>% filter(as_date(timestamp) == sample_day2 )

dbByCarGridL_day3 <- dbByCarGridL %>% filter(as_date(timestamp) == sample_day3 )

# we will also create 1 fine-grained map for comparison:

dbByCarGridS_day3 <- dbByCarGridS %>% filter(as_date(timestamp) == sample_day2 )

#------------------------------------------------------------------
#
# Flow analysis - to understand where the demand is, lets look at the movement of 
# cars between grids

# First we calculate how many cars are in each grid for each hour of the day:

flowByGrid <- dbByCarGridL_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))

flowByGrid2 <- dbByCarGridL_day2 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))

flowByGrid3 <- dbByCarGridL_day3 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))

flowByGrid4 <- dbByCarGridS_day3 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))

# lets take a look at where the cars are:
flowByGrid %>% ggplot(aes(Grid_long, Grid_lat, color = inventory)) + 
  geom_point(size = 3) +
  scale_color_gradient2(midpoint = 1, low = "blue",  mid = "gold",
                        high = "red", space = "Lab" ) +
  facet_wrap(~Hour)

# Calculate the change in inventory by grid per hour:

flowByGrid <- 
  flowByGrid %>%
  group_by(Grid_lat, Grid_long) %>%
  mutate(carFlow = inventory - dplyr::lag(inventory, n = 1, default = NA, order_by = Hour))

flowByGrid2 <- 
  flowByGrid2 %>%
  group_by(Grid_lat, Grid_long) %>%
  mutate(carFlow = inventory - dplyr::lag(inventory, n = 1, default = NA, order_by = Hour))

flowByGrid3 <- 
  flowByGrid3 %>%
  group_by(Grid_lat, Grid_long) %>%
  mutate(carFlow = inventory - dplyr::lag(inventory, n = 1, default = NA, order_by = Hour))

flowByGrid4 <- 
  flowByGrid4 %>%
  group_by(Grid_lat, Grid_long) %>%
  mutate(carFlow = inventory - dplyr::lag(inventory, n = 1, default = NA, order_by = Hour))
# Plot the results:

flowByGrid %>% 
  filter(carFlow != 0) %>%
  ggplot(aes(Grid_long, Grid_lat, color = carFlow)) + geom_point(size = 2) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "lightyellow",
                        high = "red", space = "Lab" ) +
  labs(title = "Hourly flow Day 1 Large  Grid") +
  theme_dark() + facet_wrap(~Hour)

flowByGrid2 %>% 
  filter(carFlow != 0) %>%
  ggplot(aes(Grid_long, Grid_lat, color = carFlow)) + geom_point(size = 2) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "lightyellow",
                        high = "red", space = "Lab" ) +
  labs(title = "Hourly flow Day 2 Large  Grid") +
  theme_dark() + facet_wrap(~Hour)

flowByGrid3 %>% 
  filter(carFlow != 0) %>%
  ggplot(aes(Grid_long, Grid_lat, color = carFlow)) + geom_point(size = 2) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "lightyellow",
                        high = "red", space = "Lab" ) +
  labs(title = "Hourly flow Day 3 Large Grid") +
  theme_dark() + facet_wrap(~Hour)

flowByGrid4 %>% 
  filter(carFlow != 0) %>%
  ggplot(aes(Grid_long, Grid_lat, color = carFlow)) + geom_point(size = 2) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "lightyellow",
                        high = "red", space = "Lab" ) +
  labs(title = "Hourly flow Day 3 Fine Grid") +
  theme_dark() + facet_wrap(~Hour)


# First, define the box for Tel Aviv:
# tav_top <- 32.1500
# tav_left <- 34.7000
# tav_bot <- 32.0000
# tav_right <- 34.8500

tav_top <- 32.1500
tav_left <- 34.7200
tav_bot <- 32.0000
tav_right <- 34.8500

tav_borders <- c(left = tav_left,
                 right = tav_right,
                 top = tav_top,
                 bottom = tav_bot)

tav_map <- get_stamenmap(bbox = tav_borders, zoom = 11)

p <- ggmap(tav_map)

f4 <- flowByGrid4 %>% filter(carFlow != 0 & Hour == 15)

f3 <- flowByGrid3 %>% filter(carFlow != 0 & Hour == 8)

p + geom_point(aes(Grid_long, Grid_lat, color = carFlow), data = f4, size = 3) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "slategray4",
                        high = "red", space = "Lab" ) +
  labs(title = "Tel Aviv car flow 1/9/19 3pm Fine Grid")

p + geom_point(aes(Grid_long, Grid_lat, color = carFlow), data = f3, size = 3) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "slategray4",
                        high = "red", space = "Lab" ) +
  labs(title = "Tel Aviv car flow 1/9/19 8am Large Grid")

# Take a look at the Timestamp and get it into the correct format
class(sharedAutos$timestamp)
sharedAutos$timestamp <- ymd_hms(sharedAutos$timestamp)
class(sharedAutos$timestamp)

# What is the range of dates in our database?
min(sharedAutos$timestamp)
max(sharedAutos$timestamp)

# Create some filters: day, week , month - perhaps even hour by hour


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

flow_start <- dbgrid_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>% filter(Minute == 0) %>%
  summarize(carsList)

flow_end <- dbgrid_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>% filter(Minute == 59) %>%
  summarize(carsList)

flow_start <- mutate(flow_start, gridCars = unique(carsList))

levels(factor(flow_start$carsList))

flow <- flow_end - flow_start

flow %>% ggplot(aes(Grid_long, Grid_lat, color = grid_total_cars)) + geom_point() +
  facet_wrap(~Hour)

# Calculate ho many cars leave and enter a grid by timestamp:

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






flow_start <- dbByCarA_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>% filter(Minute == 0) %>%
  summarize(beginning = n_distinct(Car))


flow_end <- dbByCarA_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>% filter(Minute == 59) %>%
  summarize(end = n_distinct(Car))

flow <- full_join(flow_start, flow_end, by = NULL)

flow[is.na(flow)] <- 0

flow <- mutate(flow, delta = end - beginning)

hist(flow$delta)

flow %>% 
  ggplot(aes(Grid_long, Grid_lat, color = delta)) + geom_point() +
  facet_wrap(~Hour)


# Another approach to looking at flow:

# find the number of cars / grid
flowByGrid <- dbByCarA_day1 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))

flowByGrid2 <- dbByCarA_day2 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))

flowByGrid3 <- dbByCarA_day3 %>% group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car))



# and conversely, where the cars aren't: (Nees more work, as flowByGrid has inventory > 1 always with
# the current grid size)



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
