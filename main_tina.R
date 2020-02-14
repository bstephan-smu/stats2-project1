
library(tidyverse)
library(GGally)
library(caret)

# locate project root directory

calendar <- read.csv("resources/data/calendar.csv")
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
  filter(!is.na(log_price)) %>%
  mutate(cleaning_fee = as.numeric(gsub('\\$', '', cleaning_fee)))
  
summary(listings$last_scraped)
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

listings_edited = listings_edited %>%
  select(c(log_price,neighbourhood_group_cleansed,host_is_superhost,
           property_type,has_parking,allows_pets, room_type, bed_type, 
           guests_included ,accommodates, cleaning_fee,review_scores_value,
           review_scores_cleanliness,reviews_per_month,has_hottub,has_wifi,
           has_fireplace, kid_friendly)) %>%
  drop_na()

airbnb_price_lm_cv = train(data=listings_edited, log_price~neighbourhood_group_cleansed*has_parking+ 
                             host_is_superhost + property_type*has_parking + property_type*allows_pets  + 
                             room_type + bed_type + guests_included + accommodates + cleaning_fee + 
                             review_scores_value + review_scores_cleanliness + reviews_per_month +
                             has_hottub + has_wifi + has_fireplace + kid_friendly,
                           method = "lm",
                           trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))

summary(airbnb_price_lm_cv)
plot(airbnb_price_lm_cv$finalModel)
airbnb_price_lm_cv
airbnb_price_lm_cv$finalModel

listings_edited %>%
  filter(is.na(log_price))


calendar$price <- as.numeric(gsub('\\$', '', calendar$price)) %>%
  mutate(price_num = )



calendar %>%
  filter(listing_id==241032) %>%
  mutate(dt = as.Date(date)) %>%
  ggplot(aes(x=date, y=price)) +
  geom_point()
calendar %>% 
  mutate(available_count = ifelse(available == "t", 1, 0)) %>% 
  group_by(date) %>% 
  summarize(total_available = sum(available_count))

review_grouped %>%
  ggplot(aes(x=date, y=listing_count)) +
  geom_point()

head(review_grouped)
mutate(logcount = log(n))
summary(review_grouped)
review_grouped %>%
  ggplot(aes(x=date, y=logcount)) +
  geom_point()

library(tseries)
summary(reviews)

head(reviews)
#number of listings by month year
review_grouped_mo <- reviews %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(review_count_mo = sum(n_distinct(listing_id)))
head(review_grouped_mo)

review_grouped_day <- reviews %>%
  mutate(date = as.Date(date)) %>%
  count(date)
head(review_grouped_day)
reviews_ts = review_grouped_day %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  left_join(review_grouped_mo, by=c("month", "year")) %>%
  mutate(normalized = n/review_count_mo)

tail(reviews_ts,20)

library(tswge)

review_ts <- reviews %>%
  mutate(date = as.Date(date))%>%
  count(date) 

head(reviews_ts, 20)
adf.test(reviews_ts$normalized) #pvalue <.05 indicates data is stationary
stationaryTS <- diff(reviews_ts$normalized, differences= 1)
stationaryTS
plot(stationaryTS, type="l", main="Differenced and Stationary")
airbnb_reviews_lm_cv = train(data=review_grouped, 
                           method = "lm",
                           trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
library(forecast)

write.csv(reviews_ts,"reviews_ts.csv", row.names = FALSE)

train_length = length(stationaryTS) - 20
ts = arima(stationaryTS[1:train_length], c(4,0,0))
ts
cast.ts<-forecast(ts,h=21)
cast.ts["Point Forecast"]
accuracy(cast.ts,stationaryTS[train_length:length(stationaryTS)])

plot(forecast(ts,h=21))
points(1:length(train),fitted(AR4),type="l",col="blue")
points(1:40,Bill,type="l")

