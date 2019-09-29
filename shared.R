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
if(!require(gganimate)) install.packages("gganimate", repos = "http://cran.us.r-project.org")
if(!require(gifski)) install.packages("gifski", repos = "http://cran.us.r-project.org")



# -----------------------------------------------------------------
# 
# Data
#
# We will provide a copy of the database on our project github repo in order
# to make it easier to access.

# Load and convert our dataset:

dl <- tempfile()

# From github:

dURL <- "https://github.com/tomcache/data-science-shared-cars/raw/master/datasource/autotel-shared-car-locations.zip"

download.file(dURL, destfile = dl, method = "wininet")

unzip(dl, overwrite = TRUE, exdir = "autotel")

# Create our database file from the supplied sample data
# This dataset sample contains one months worth of data on parked cars in the format shown below.


db <- read_csv("autotel//sample_table.csv")
  

#------------------------------------------------------------------
#
# Data Exploration and Visualization
#
# Section 1: Data preparation and organization

# Take a look at our data:

glimpse(db)

summary(db)

length(unique(db$carsList))

length(unique(db$latitude))

length(unique(db$longitude))

length(unique(db$total_cars))

# range of full dataset sample:
min(db$timestamp)
max(db$timestamp)

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

# We next add a grid identifier, using a grid size of approximately 90m x 110m square

db <- db %>%
  mutate(Grid_lat = round(latitude,3),
         Grid_long = round(longitude,3),
         gridKey = (Grid_lat*10000000000 + Grid_long*10000)
 )

# For some of our analysis, we will need to know which cars
# are located in the parkings spots. To do this, we need to 
# convert the cars list into an array of numbers.

dbcars <-  db %>% 
  select(carsList) %>% 
  distinct() %>% 
  mutate(cars = sub(carsList, pattern = "\\[", replacement = "")) %>% 
  mutate(cars = sub(cars, pattern = "\\]", replacement = "")) %>% 
  separate(cars, sep = ",", into = c("car1", "car2", "car3", "car4", "car5", "car6", "car7", "car8", "car9", "car10"), fill = "right") %>% 
  mutate_at(2:11, as.numeric)
# And, let's have a list of all cars "names" (which is a number)
CarsList <- (1:max(dbcars$car1, na.rm = T))[1:max(dbcars$car1, na.rm = T) %in% dbcars$car1]

# We also can use a calculation of the number of cars in use / available at any given time:

dbCarUse <- db %>%
  group_by(timestamp) %>% summarise(total_cars = sum(total_cars)) 

dbCarUse <- dbCarUse %>%
  mutate(timestamp = timestamp + 3600*2, # local time = GMT/UTC + 2
         Hour = lubridate::hour(timestamp),
         Minute = lubridate::minute(timestamp),
         label_wday = lubridate::wday(timestamp, label = TRUE, abbr = FALSE)
  )

glimpse(db)

# We will plot the availability of cars in our dataset over time. We will also superimpose
# a smoothed plot so that we can see the variablity between days as well.

db %>% group_by(timestamp) %>% summarize(cars_available = sum(total_cars)) %>%
  ggplot(aes(timestamp, cars_available)) + geom_line(color = "slategray4", size = 1.0) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  ggtitle("Shared Car Availability in Tel Aviv") 

all_cars <- db %>% group_by(timestamp) %>% summarize(cars_available = sum(total_cars))

# We'd like to know the total number of cars in our fleet:

(fleet <-  max(all_cars$cars_available))



  
#------------------------------------------------------------------
#
# Demand analysis
# 
# We're going to zoom in and look at a single day in order to better understand the patterns of use.
#For our first sample, we choose December 15, 2018, which is a Tuesday:

sample_day1 <- ymd("2018-12-15")

# We'll filter our data and take a look at it's variation over the day

db_day1 <- db %>% filter(as_date(timestamp) == sample_day1 )


db_day1 %>% group_by(timestamp) %>% summarize(cars_available = sum(total_cars)) %>%
  ggplot(aes(timestamp, cars_available)) + geom_line(color = "slategray4", size = 1.0) +
  ggtitle("Car Availability for Tuesday, December 15, 2018")



dbCarUse <- dbCarUse %>% mutate(utilize = 1 - total_cars/fleet)

# We can also provide other ways of looking at the data. Here we look at the distribution of utilization,
# and compare this by day of the week.

dbCarUse %>% group_by(Hour) %>% summarize(cars_available = sum(total_cars)) %>%
  ggplot(aes(Hour, cars_available)) + 
  geom_line(color = "slategray4", size = 2) +
  ggtitle("Shared Car Availability in Tel Aviv by Hour of the Day") 


dbCarUse %>% group_by(label_wday) %>% 
  ggplot(aes(label_wday, total_cars, fill = label_wday)) + 
  geom_boxplot(aes(group = label_wday)) +
  ggtitle("Shared Car Availability in Tel Aviv by Weekday") 

# We can also take a look at the utilization of the fleet - i.e., how much of the fleet is in use 
# at any given time? 

# We can plot the utilization of the fleet over time:

(Utilize_min <- min(dbCarUse$utilize))

(Utilize_max <- max(dbCarUse$utilize))


dbCarUse %>% ggplot(aes(x = utilize, fill = label_wday)) + 
  geom_density(position = "identity", alpha = 0.6) +
  ggtitle("Density Plot of Shared Car Fleet Utilization") 



(Utilize_min <- min(dbCarUse$utilize))

(Utilize_max <- max(dbCarUse$utilize))
  
#------------------------------------------------------------------
#
# create a list of entries separated by car, and add grid information:

dbByCar <- left_join(db, dbcars) %>% 
  gather(CarI, Car, car1:car10)  %>% 
  filter(!is.na(Car)) %>% 
  group_by(Car) %>% 
  arrange(timestamp) %>%
  ungroup()

#n We'll limit our data to a single day to keep it from becoming too unweildy. We will also calculate a summary of the number of cars visited in each grid per hour:


flowByGrid <- dbByCar %>% 
  filter(as_date(timestamp) == sample_day1 ) %>%
  group_by(Grid_lat, Grid_long, Hour) %>%
  summarize(inventory = n_distinct(Car)
  )

flowByGrid %>% ggplot(aes(Grid_long, Grid_lat, color = inventory, size = inventory)) + 
  geom_point() +
  scale_color_gradient2(midpoint = 2.5, low = "blue",  mid = "gold",
                        high = "red", space = "Lab" )

f <- flowByGrid %>% ggplot(aes(Grid_long, Grid_lat, color = inventory, size = inventory, frame = Hour)) + 
  geom_point() +
  scale_color_gradient2(midpoint = 2.5, low = "blue",  mid = "gold",
                        high = "red", space = "Lab" ) +
  theme_set(theme_bw())

f + transition_time(Hour) +
  labs(title = "Hour: {frame_time}")

# First, define the box for Tel Aviv:
# tav_top <- 32.1500
# tav_left <- 34.7000
# tav_bot <- 32.0000
# tav_right <- 34.8500

tav_top <- 32.1500
tav_left <- 34.7350
tav_bot <- 32.0300
tav_right <- 34.8500

tav_borders <- c(left = tav_left,
                 right = tav_right,
                 top = tav_top,
                 bottom = tav_bot)

tav_map <- get_stamenmap(bbox = tav_borders, zoom = 12)

p <- ggmap(tav_map)

p + 
  geom_point(aes(Grid_long, Grid_lat, color = inventory, size = inventory), data = flowByGrid) +
  scale_color_gradient2(midpoint = 2.5, low = "blue",  mid = "gold",
                        high = "red", space = "Lab" ) +
  ggtitle("Shared Car Distribution in Tel Aviv 12/18/2018")


# Let's look at the distribution of the total number of cars by location. We can see from this chart that once we remove the geographic clustering information, the distribution is not random but follows a power law distribution, with some locations having many more cars fill spots than others. 

db_cars <- db_day1 %>% group_by(gridKey) %>% summarize(cars_available = sum(total_cars))

db_cars$gridKey <- factor(db_cars$gridKey, levels = db_cars$gridKey[order(-db_cars$cars_available)])

ggplot(data = db_cars, aes(x = gridKey, y = cars_available)) + 
  geom_bar(stat = "identity", color = "slategray4")


#------------------------------------------------------------------
#
# Create train & test sets, as well as single day set for rapid testing and development
#
# Validation set will be 10% of MovieLens data


set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = db$total_cars, times = 1, p = 0.1, list = FALSE)
db_train <- db[-test_index,]
db_test <- db[test_index,]

db_test <- db_test %>% 
  semi_join(db_train, by = "gridKey") %>%
  semi_join(db_train, by = "Hour") %>%
  semi_join(db_train, by = "Weekday")

# We will now create a single-day data set, and do the same thing:

sample_day1 <- ymd("2018-12-15")

db_day <- db %>% filter(as_date(timestamp) == sample_day1 )

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = db_day$total_cars, times = 1, p = 0.1, list = FALSE)
db_day_train <- db_day[-test_index,]
db_day_test <- db_day[test_index,]

db_day_test <- db_day_test %>% 
  semi_join(db_day_train, by = "gridKey") %>%
  semi_join(db_day_train, by = "Hour") %>%
  semi_join(db_day_train, by = "Weekday")

# and a 1-week data set, from 12/20/18 through 12/26/18:
week_day1 <- ymd("2018-12-20")
week_day7 <- ymd("2018-12-26")

db_week <- db %>% filter(timestamp >= week_day1 & timestamp <= week_day7 )

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = db_week$total_cars, times = 1, p = 0.1, list = FALSE)
db_week_train <- db_week[-test_index,]
db_week_test <- db_week[test_index,]

db_week_test <- db_week_test %>% 
  semi_join(db_week_train, by = "gridKey") %>%
  semi_join(db_week_train, by = "Hour") %>%
  semi_join(db_week_train, by = "Weekday")

#------------------------------------------------------------------
#
# For our initial prediction model, we need to compute the average car availability
# by grid:

# function for calculating RMSE of prediction vs. actual

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Caclculation of simple means for each time epoch:

dbTrainMean <- db_train %>% summarize(Mean = mean(total_cars)) 
mu_dbTrain <- mean(db_train$total_cars)

db_dayTrainMean <- db_day_train %>% summarize(Mean = mean(total_cars)) 
mu_db_dayTrain <- mean(db_day_train$total_cars)

db_weekTrainMean <- db_week_train %>% summarize(Mean = mean(total_cars)) 
mu_db_weekTrain <- mean(db_week_train$total_cars)

(baseline_rmse <- RMSE(db_test$total_cars, mu_dbTrain))

(baseline_day_rmse <- RMSE(db_day_test$total_cars, mu_db_dayTrain))

(baseline_week_rmse <- RMSE(db_week_test$total_cars, mu_db_weekTrain))

# build up a model for the day data set

# First, the hour effect:


day_hour_avgs <- db_day_train %>% 
  group_by(Hour) %>% 
  summarize(b_i_day = mean(total_cars - mu_db_dayTrain))

# day_hour_avgs %>% qplot(b_i, geom ="histogram", data = ., color = I("slategray4"))


predicted_ratings_day <- mu_db_dayTrain + db_day_test %>% 
  left_join(day_hour_avgs, by='Hour') %>%
  pull(b_i_day)

model_1_day_rmse <- RMSE(predicted_ratings_day, db_day_test$total_cars)

# Then we add the location effect:

day_grid_avgs <- db_day_train %>% 
  left_join(day_hour_avgs, by = 'Hour') %>%
  group_by(gridKey) %>% 
  summarize(b_u_day = mean(total_cars - mu_db_dayTrain - b_i_day))

predicted_ratings_day <- db_day_test %>% 
  left_join(day_hour_avgs, by='Hour') %>%
  left_join(day_grid_avgs, by='gridKey') %>%
  mutate(pred = mu_db_dayTrain + b_i_day + b_u_day) %>%
  pull(pred)


model_2_day_rmse <- RMSE(predicted_ratings_day, db_day_test$total_cars)

# Now we do the same modeling for the week data set, and the full data set:

# First, the hour effect:


week_hour_avgs <- db_week_train %>% 
  group_by(Hour) %>% 
  summarize(b_i_week = mean(total_cars - mu_db_weekTrain))

# week_hour_avgs %>% qplot(b_i_week, geom ="histogram", data = ., color = I("slategray4"))


predicted_ratings_week <- mu_db_weekTrain + db_week_test %>% 
  left_join(week_hour_avgs, by='Hour') %>%
  pull(b_i_week)

model_1_week_rmse <- RMSE(predicted_ratings_week, db_week_test$total_cars)

# Then we add the location effect:

week_grid_avgs <- db_week_train %>% 
  left_join(week_hour_avgs, by = 'Hour') %>%
  group_by(gridKey) %>% 
  summarize(b_u_week = mean(total_cars - mu_db_weekTrain - b_i_week))

predicted_ratings_week <- db_week_test %>% 
  left_join(week_hour_avgs, by='Hour') %>%
  left_join(week_grid_avgs, by='gridKey') %>%
  mutate(pred = mu_db_dayTrain + b_i_week + b_u_week) %>%
  pull(pred)


model_2_week_rmse <- RMSE(predicted_ratings_week, db_week_test$total_cars)

# Then the weekday effect:

week_day_avgs <- db_week_train %>% 
  left_join(week_hour_avgs, by = 'Hour') %>%
  left_join(week_grid_avgs, by='gridKey') %>%
  group_by(Weekday) %>% 
  summarize(b_d_week = mean(total_cars - mu_db_weekTrain - b_i_week - b_u_week))

predicted_ratings_week <- db_week_test %>% 
  left_join(week_hour_avgs, by='Hour') %>%
  left_join(week_grid_avgs, by='gridKey') %>%
  left_join(week_day_avgs, by = 'Weekday') %>%
  mutate(pred = mu_db_dayTrain + b_i_week + b_u_week + b_d_week) %>%
  pull(pred)

model_3_week_rmse <- RMSE(predicted_ratings_week, db_week_test$total_cars)

# Now develop the model for the full data sample
# First, the hour effect:


hour_avgs <- db_train %>% 
  group_by(Hour) %>% 
  summarize(b_i = mean(total_cars - mu_dbTrain))

# hour_avgs %>% qplot(b_i, geom ="histogram", data = ., color = I("slategray4"))


predicted_ratings <- mu_dbTrain + db_test %>% 
  left_join(hour_avgs, by='Hour') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, db_test$total_cars)

# Then we add the location effect:

grid_avgs <- db_train %>% 
  left_join(hour_avgs, by = 'Hour') %>%
  group_by(gridKey) %>% 
  summarize(b_u = mean(total_cars - mu_dbTrain - b_i))

predicted_ratings <- db_test %>% 
  left_join(hour_avgs, by='Hour') %>%
  left_join(grid_avgs, by='gridKey') %>%
  mutate(pred = mu_dbTrain + b_i + b_u) %>%
  pull(pred)


model_2_rmse <- RMSE(predicted_ratings, db_test$total_cars)

# Then the weekday effect:

day_avgs <- db_train %>% 
  left_join(hour_avgs, by = 'Hour') %>%
  left_join(grid_avgs, by='gridKey') %>%
  group_by(Weekday) %>% 
  summarize(b_d = mean(total_cars - mu_dbTrain - b_i - b_u))

predicted_ratings <- db_test %>% 
  left_join(hour_avgs, by='Hour') %>%
  left_join(grid_avgs, by='gridKey') %>%
  left_join(day_avgs, by = 'Weekday') %>%
  mutate(pred = mu_dbTrain + b_i + b_u + b_d) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, db_test$total_cars)

# Now develop our knn model and prediction
# Model: day

train_knnD <- train(total_cars ~ Hour + gridKey, method = "knn", data = db_day_train)

train_knnD

plot(train_knnD)

train_knnD$bestTune

y_hat_knnD <- predict(train_knnD, db_day_test, type = "raw")

model_knn_day <- RMSE(y_hat_knnD, db_day_test$total_cars)

# We now put all of our results into a table so that we can easily compare them

rmse_results <- tibble(method = "One Day Baseline (mean)", RMSE = baseline_day_rmse)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="One Day Hour Effect Model",  
                                 RMSE = model_1_day_rmse)
)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="One Day Location + Hour Effects Model",  
                                 RMSE = model_2_day_rmse)
)

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "One Week Baseline (mean)", 
                                 RMSE = baseline_week_rmse)
)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="One Week Hour Effect Model",  
                                 RMSE = model_1_week_rmse)
)                                    

rmse_results <- bind_rows(rmse_results,
                          tibble(method="One Week Location + Hour Effects Model",  
                                 RMSE = model_2_week_rmse)
)                                    

rmse_results <- bind_rows(rmse_results,
                          tibble(method="One Week Weekday + Location + Hour Effects Model",  
                                 RMSE = model_3_week_rmse)
)                                    


rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Full Sample Baseline (mean)", 
                                 RMSE = baseline_rmse)
)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="Full Sample Hour Effect Model",  
                                 RMSE = model_1_rmse)
)                                    

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Full Sample Location + Hour Effects Model",  
                                 RMSE = model_2_rmse)
)       

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Full Sample Weekday + Location + Hour Effects Model",  
                                 RMSE = model_3_rmse)
)       

rmse_results <- bind_rows(rmse_results,
                          tibble(method="One Day knn Model",  
                                 RMSE = model_knn_day)
)       

rmse_results %>% knitr::kable()

# With our predictions in the first day model, we can create a plot:

m <- db_day_test %>% 
  left_join(day_hour_avgs, by='Hour') %>%
  left_join(day_grid_avgs, by='gridKey') %>%
  mutate(pred = mu_db_dayTrain + b_i_day + b_u_day) %>%
  select(Grid_long, Grid_lat, pred)


m %>% ggplot(aes(Grid_long, Grid_lat, color = pred, size = pred)) + 
  geom_point() +
  scale_color_gradient2(midpoint = 2.5, low = "blue",  mid = "gold",
                        high = "red", space = "Lab" ) +
  ggtitle("Car Availability Model ") 


# And now we can compare the result of our predictions from the knn model:

q <- m %>% select(-pred) %>% mutate(kpred = y_hat_knnD)

q %>% ggplot(aes(Grid_long, Grid_lat, color = kpred, size = kpred)) + 
  geom_point() +
  scale_color_gradient2(midpoint = 2.5, low = "blue",  mid = "gold",
                        high = "red", space = "Lab" ) +
  ggtitle("Car Availability Model Using knn") 





