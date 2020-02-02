library(here)
library(tidyverse)
library(GGally)

# locate project root directory
data_root <- here()

# read in csv files from resources into dataframes
#calender <- read.csv(paste(data_root, "/resources/data/calendar.csv", sep=""))
#listings <- read.csv(paste(data_root, "/resources/data/listings.csv", sep=""))
#reviews <- read.csv(paste(data_root, "/resources/data/reviews.csv", sep=""))

calender <- read.csv("resources/data/calendar.csv")
listings <- read.csv("resources/data/listings.csv")
reviews <- read.csv("resources/data/reviews.csv")

#data cleaning
top_5_property_types = sort(table(listings$property_type), decreasing=TRUE)[1:5]
top_15_zipcode = sort(table(listings$zipcode), decreasing=TRUE)[1:15]
top_15_neighborhoods = sort(table(listings$neighbourhood_group_cleansed), decreasing=TRUE)[1:15]

# adding some basic features
listings$has_wifi = grepl('Wireless Internet', listings$amenities, ignore.case=TRUE)
listings$has_fireplace = grepl('Indoor Fireplace', listings$amenities, ignore.case=TRUE)
listings$kid_friendly = grepl('Family/Kid Friendly', listings$amenities, ignore.case=TRUE)
listings$has_hottub = grepl('Hot Tub', listings$amenities, ignore.cas=TRUE)
listings$has_parking = grepl('Free Parking on Premises', listings$amenities, ignore.cas=TRUE)
listings$allows_pets = grepl('Pets Allowed', listings$amenities, ignore.cas=TRUE)

listings_edited = listings %>%
  filter(property_type %in% names(top_5_property_types)) %>%
  filter(zipcode %in% names(top_15_zipcode)) %>%
  filter(neighbourhood_group_cleansed %in% names(top_15_neighborhoods)) %>%
  mutate(log_reviews = log(number_of_reviews+1)) %>%
  mutate(price = as.numeric(gsub('\\$', '', price)))  %>%
  mutate(log_price = log(price))%>%
  mutate(cleaning_fee = as.numeric(gsub('\\$', '', cleaning_fee)))
  
listings_edited$property_type = droplevels(listings_edited$property_type)
listings_edited$zipcode = droplevels(listings_edited$zipcode)
listings_edited$neighbourhood_group_cleansed = droplevels(listings_edited$neighbourhood_group_cleansed)

#pairs plots
listings_edited %>%
  select(price, log_price, host_is_superhost, availability_90, log_reviews, neighbourhood_group_cleansed) %>%
  ggpairs()
listings_edited %>%
  select(price, log_price, property_type, room_type, bed_type, guests_included) %>%
  ggpairs() 
listings_edited %>%
  select(price,log_price, accommodates, cleaning_fee, review_scores_value, review_scores_cleanliness, reviews_per_month) %>%
  ggpairs()
listings_edited %>%
  select(price, log_price, has_availability, has_hottub, has_wifi, has_fireplace, kid_friendly, has_parking,
         allows_pets) %>%
  ggpairs()
listings_edited %>%
  select(price, log_price, neighbourhood_group_cleansed, zipcode) %>%
  ggpairs()

#linear model
airbnb_price_lm = lm(data=listings_edited, log_price~neighbourhood_group_cleansed*has_parking+ 
                       host_is_superhost + availability_90 + log_reviews + property_type*has_parking*allows_pets  + 
                       room_type + bed_type + guests_included + accommodates + cleaning_fee + 
                       review_scores_value + review_scores_cleanliness + reviews_per_month +
                       has_hottub + has_wifi + has_fireplace + kid_friendly)     
summary(airbnb_price_lm)          
plot(airbnb_price_lm)




