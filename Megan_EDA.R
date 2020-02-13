library(tidyverse)
library(GGally)
library(ggplot2)
library(corrplot)
library(cowplot)
library(readr)
library(glmnet)
library(caret)

#cant get here to work, just setting my local pathname
#comment this out for your local machine.
setwd("/Users/zmartygirl/data/MSDSR/6372/stats2-project1/resources/data")

listings = read_csv("listings.csv")
reviews = read_csv("reviews.csv")
calendar = read_csv("calendar.csv")


#Borrowing the listing of variables from Branum's original EDA
df <- listings %>% select('id', 'name', 'host_location', 'host_neighbourhood',
                          'neighbourhood_cleansed', 'zipcode', 'property_type', 'bedrooms', 'square_feet', 'security_deposit', 'minimum_nights',
                          'availability_30', 'review_scores_rating', 'review_scores_communication', 'license', 'summary', 'neighborhood_overview',
                          'host_about', 'host_is_superhost', 'host_listings_count', 'host_identity_verified', 'neighbourhood_group_cleansed',
                          'beds', 'price', 'cleaning_fee', 'maximum_nights', 'availability_60', 'number_of_reviews', 'review_scores_accuracy', 'review_scores_location',
                          'require_guest_phone_verification', 'host_name', 'host_response_time', 'host_total_listings_count', 'street', 'city',
                          'smart_location', 'accommodates', 'bed_type', 'weekly_price', 'guests_included', 'availability_90', 'first_review', 'review_scores_cleanliness',
                          'review_scores_value', 'instant_bookable', 'calculated_host_listings_count', 'description', 'transit', 'host_since', 'host_response_rate',
                          'neighbourhood', 'bathrooms', 'amenities', 'monthly_price', 'extra_people', 'has_availability', 'availability_365',
                          'last_review', 'review_scores_checkin', 'cancellation_policy', 'reviews_per_month')


#Any Var with $ is stored as a char, adjust to a double. 
df$cleaning_fee = as.double(str_extract(df$cleaning_fee, "(\\d)+"))
df$price = as.double(str_extract(df$price, "(\\d)+"))
df$security_deposit = as.double(str_extract(df$security_deposit, "(\\d)+"))
df$weekly_price = as.double(str_extract(df$weekly_price, "(\\d)+"))
df$monthly_price = as.double(str_extract(df$monthly_price, "(\\d)+"))
df$extra_people = as.double(str_extract(df$extra_people, "(\\d)+"))

#Experimenting with removal of some variables from df
df_limited = df %>% select(-c('name', 'host_location','square_feet', 'license', 'host_name', 'location','smart_location'))
#removing variables that we are unlikely to use as identifiers, predictors etc. also some have a lot of NAs

#Can remove description, host_about, summary neighborhood_overview, transit


###Edited Columns###



##Engineered Features

#Calculated Columns from Amenities
df$has_wifi = grepl("Wireless Internet", df$amenities, ignore.case = TRUE)
df$has_fireplace = grepl("Indoor Fireplace", df$amenities, ignore.case = TRUE)
df$kid_friendly = grepl('Family/Kid Friendly', df$amenities, ignore.case=TRUE)
df$has_hottub = grepl('Hot Tub', df$amenities, ignore.cas=TRUE)
df$has_parking = grepl('Free Parking on Premises', df$amenities, ignore.cas=TRUE)
df$allows_pets = grepl('Pets Allowed', df$amenities, ignore.cas=TRUE)


##Transformations 
df$log_price = log(df$price)
df$log_reviews = log(df$reviews_per_month)
df$log_reviews = log(df$number_of_reviews+1)
#Logged Price graph
df %>% ggplot() + geom_histogram(mapping = aes(x = log(df$price)), stat = "count")
#unlogged graph
df %>% ggplot() + geom_histogram(mapping = aes(x = df$price), stat = "count")


##NA fixes

df$cleaning_fee = ifelse(is.na(df$cleaning_fee), 0, df$cleaning_fee)


#borrowed

# find average price per night for each unit...join onto reviews
avg_calendar <- calendar %>% group_by(listing_id) %>% summarize(mean_price = mean(price), na.rm=TRUE)
calendar_reviews <- reviews %>% left_join(avg_calendar, by=c('listing_id'))

# merge listing information onto pricing info and review
comments_main <- calendar_reviews %>% left_join(listings, by=c('listing_id' = 'id'))


#simple model
#overall Research Question: What affect does pric
model.1 = lm(review_scores_rating ~ price + beds + cleaning_fee, data = df)


#rbloggers example using Lasso, can't get it to work right.
x <- model.matrix(price~ host_is_superhost + availability_90 + 
                          bed_type + guests_included + accommodates + cleaning_fee + 
                          review_scores_value + review_scores_cleanliness + reviews_per_month +
                          has_hottub + has_wifi + has_fireplace + kid_friendly, df)[,-1]
y <- df$price
lambda <- 10^seq(10, -2, length = 100)
set.seed(123)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

#lasso based model
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = 1, newx = x[test,])
mean((lasso.pred-ytest)^2)


#trying lasso alone
#not a great lone cowboy my lasso'in needs work
#lasso.model = glmnet(matrix(data = df, nrow = nrow(df), ncol = ncol(df)) ,df$price)

lasso.based = train()
summary(calendar$price)

calendar$price <- as.numeric(gsub('\\$', '', calendar$price))
view(calendar %>% filter(listing_id == 241032))

calendar %>% group_by(listing_id) %>% summarise(listing_id)

calendar %>% group_by(date) %>% summarise()

###
#time series on how many are available on a given day
###

calendar_new = calendar %>% mutate(available_count = ifelse(available == "TRUE", 1, 0)) %>% group_by(date) %>% summarize(total_available = sum(available_count))

calendar_new %>% ggplot(aes(x = date, y = total_available)) + geom_line()

calendar %>% mutate(available_count = ifelse(available == "TRUE", 1, 0)) %>% group_by(date) %>% summarize(total_available = sum(available_count))

reviews$date = as.date(reviews)

reviews_new = reviews %>% group_by(date) %>% summarize(total_booked = count(id))
reviews_new %>% filter(date > 2014-01-01) %>% ggplot(aes(x = date, y = total_booked)) + geom_line()

reviews_new %>% filter(date > 2014-01-01) %>% ggplot(aes(x = date, y = total_booked)) + geom_point()

reviews_new  %>% ggplot(aes(x = date, y = total_booked)) + geom_line()

#can group by month, can use calendar to forecast. 
#can predict what the useage will be, use the calendar as a barometer. 
# 

#corr plots

df %>% select(price, log_price, cleaning_fee,accommodates,beds,bedrooms,bathrooms, reviews_per_month, review_scores_location) %>% 
        ggcorr(label = TRUE, angle= -45) 


df_num = df %>% select_if(is.numeric) 
df_notnum = df %>% select_if(negate(is.numeric))

df_num_model = df_num %>% select(-c(id,price,availability_30, availability_60, availability_90, reviews_per_month, review_scores_checkin, log_reviews,host_total_listings_count, host_listings_count))
summary(lm(log_price ~ ., data = df_num_model))


df_num %>% select(-c(id, weekly_price, monthly_price, minimum_nights,maximum_nights)) %>% 
        ggcorr(label = TRUE, label_size = 2, hjust = 1, size = 3, color = "grey50", layout.exp = 4) 

summary(lm(log_price ~ beds + cleaning_fee + accommodates + calculated_host_listings_count + 
                   review_scores_value + property_type, data = df))

###On the subject of editing the DF down with certain levels
#Summary of the levels and #s for each of the variables limited in the listings_edited version
summary(as.factor(df$property_type))
summary(as.factor(df$zipcode))
summary(as.factor(df$neighbourhood_group_cleansed))


#A good bit of data is eliminated with listings_edited
###
#> dim(listings_edited)
#[1] 3121  100
#> dim(listings)
#[1] 3818   98
###

###########################
#Experimenting / Scratch

sum(grepl("Heating", df$amenities))
model.2 = lm(price ~ neighbourhood_group_cleansed, data = df)

#Lm based on cleaning fee and plots.
lm(price ~ cleaning_fee, data = df)

df %>% ggplot() + geom_point(mapping = aes(x = cleaning_fee, y = price))


###
summary(listings$square_feet)
#3721 Nas out of 3818 rows
summary(listings$license)
#all are NAs
summary(as.factor(listings$city))
#all but 6 are seattle, little informatin
summary(as.factor(listings$smart_location))
#Similar to above, all but 8 are Seattle,WA
###

#Exploring Logging the rating score
df %>% ggplot() + geom_histogram(mapping = aes(x = df$review_scores_rating), stat = "count")
df %>% ggplot() + geom_histogram(mapping = aes(x = log(df$review_scores_rating)), stat = "count")





