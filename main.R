library(here)
library(tidyverse)

# locate project root directory
data_root <- here()

# read in csv files from resources into dataframes
calendar <- read.csv(paste(data_root, "/resources/data/calendar.csv", sep=""))
listings <- read.csv(paste(data_root, "/resources/data/listings.csv", sep=""))
reviews <- read.csv(paste(data_root, "/resources/data/reviews.csv", sep=""))

# find average price per night for each unit...join onto reviews
avg_calendar <- calendar %>% group_by(listing_id) %>% summarize(mean_price = mean(as.numeric(gsub("\\$", "", price)), na.rm=TRUE))
calendar_reviews <- reviews %>% left_join(avg_calendar, by=c('listing_id'))

# merge listing information onto pricing info and review
main <- calendar_reviews %>% left_join(listings, by=c('listing_id' = 'id'))
