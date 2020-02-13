library(here)
library(tm)
library(GGally)
library(SnowballC)
library(tidyverse)

# locate project root directory
data_root <- here()

# read in csv files from resources into dataframes
calendar <- read.csv(paste(data_root, "/resources/data/calendar.csv", sep=""))
listings <- read.csv(paste(data_root, "/resources/data/listings.csv", sep=""))
reviews <- read.csv(paste(data_root, "/resources/data/reviews.csv", sep=""))

# fixing price data type for listings data
listings$price <- as.numeric(gsub('\\$', '', listings$price))
listings$cleaning_fee <- as.numeric(gsub('\\$', '', listings$cleaning_fee))
listings$security_deposit = as.numeric(str_extract(listings$security_deposit, "(\\d)+"))
listings$weekly_price = as.numeric(str_extract(listings$weekly_price, "(\\d)+"))
listings$monthly_price = as.numeric(str_extract(listings$monthly_price, "(\\d)+"))
listings$extra_people = as.numeric(str_extract(listings$extra_people, "(\\d)+"))
listings$host_response_rate = as.numeric(str_extract(listings$host_response_rate, "(\\d)+"))

calendar$price <- as.numeric(gsub('\\$', '', calendar$price))

# adding some basic features
listings$has_wifi = grepl('Wireless Internet', listings$amenities, ignore.case=TRUE)
listings$has_fireplace = grepl('Indoor Fireplace', listings$amenities, ignore.case=TRUE)

listings$family_friendly = ifelse(grepl('Family/Kid Friendly', listings$amenities, ignore.case=TRUE)==TRUE, 1, 0)
listings$has_hottub = ifelse(grepl('Hot Tub', listings$amenities, ignore.cas=TRUE)==TRUE, 1, 0)

listings$has_parking = ifelse(grepl('Free Parking on Premises', listings$amenities, ignore.cas=TRUE)==TRUE, 1, 0)
listings$pets_allowed = ifelse(grepl('Pets Allowed', listings$amenities, ignore.cas=TRUE)==TRUE, 1, 0)


