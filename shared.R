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



dl <- tempfile()
download.file("https://www.kaggle.com/doit-intl/autotel-shared-car-locations/downloads/autotel-shared-car-locations.zip", dl)

sharedAutos <- read.csv("autotel/sample_table.csv", stringsAsFactors = FALSE)


# Take a look at our data:
glimpse(sharedAutos)

summary(sharedAutos)

length(unique(sharedAutos$carsList))

length(unique(sharedAutos$latitude))

length(unique(sharedAutos$longitude))

# Not for the faint of heart (or CPU) - lets plot the location data:
sharedAutos %>% ggplot(aes(latitude, longitude)) + geom_point()

# We should quantize the location data at different levels of granularity.
# For simplicity, we will generate a table for each, so that we can keep
# track

# There's a lot of granularity in the location data