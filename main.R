library(here)
library(tidyverse)

# locate project root directory
data_root <- here()

# read in csv files from resources into dataframes
calendar <- read.csv(paste(data_root, "/resources/data/calendar.csv", sep=""))
listings <- read.csv(paste(data_root, "/resources/data/listings.csv", sep=""))
reviews <- read.csv(paste(data_root, "/resources/data/reviews.csv", sep=""))

# fixing price data type for listings data
listings$price <- as.numeric(gsub('\\$', '', listings$price))
listings$cleaning_fee <- as.numeric(gsub('\\$', '', cleaning_fee))

# find average price per night for each unit...join onto reviews
avg_calendar <- calendar %>% group_by(listing_id) %>% summarize(mean_price = mean(price), na.rm=TRUE)
calendar_reviews <- reviews %>% left_join(avg_calendar, by=c('listing_id'))

# merge listing information onto pricing info and review
comments_main <- calendar_reviews %>% left_join(listings, by=c('listing_id' = 'id'))

# add some searches for positive keywords
comments_main$comments_is_clean <- grepl('.*clean.*', main$comments, ignore.case=TRUE)
comments_main$comments_good_location <- grepl('.*location*|.*close.*', main$comments, ignore.case=TRUE)
comments_main$comments_customer_service <- grepl('.*friendly*|.*welcome*', main$comments, ignore.case=TRUE)

# let's add comments scores back to the listing information
summarize_comments <- comments_main %>% group_by(listing_id) %>% summarize(total_is_clean = mean(comments_is_clean),
                                                                  total_good_location = mean(comments_good_location),
                                                                  total_customer_service = mean(comments_customer_service))

# main dataframe with all information aggregated and combined.
main <- listings %>% inner_join(summarize_comments, by=c('id'='listing_id'))



