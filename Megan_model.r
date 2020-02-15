library(caret)
library(fastDummies)
library(skimr)
library(tidyverse)

# importing datasets from main.r and eda
source('branum_eda.R')

# importing the output eda model from eda.R
# define which column values to use for model and create dummies
listings$log_reviews <- log(listings$number_of_reviews)
listings$log_price <- log(listings$price)

# select variables from listings and create dummy variables and drop nulls
df <- listings  %>% select(log_price, reviews_per_month, review_scores_location, bedrooms, corrected_cleaning_fee, 
                           room_type, neighbourhood_class) %>% drop_na() %>% dummy_cols(remove_first_dummy = TRUE)

#Dropping wholly useless variables, as well as obvious low information variables 
#Also dropping variables found in eda to be fairly empty.
#Also dropping variables where we extracted what we want, amenities, etc
#Currently losing over 1/2 of the data, need to fix that. 
df_fullvar <- listings %>% select(-c('name', 'host_location','square_feet', 'license', 'host_name','smart_location',
                                     'id','listing_url','scrape_id', 'summary', 'space','description', 'experiences_offered',
                                     'neighborhood_overview', 'notes', 'transit','thumbnail_url','medium_url','picture_url',
                                     'xl_picture_url','host_id','host_url','host_name', 'host_location', 'host_about','host_thumbnail_url',
                                     'host_picture_url','host_neighbourhood', 'host_has_profile_pic','host_identity_verified',
                                     'street','neighbourhood','city','state','smart_location','country_code','country','latitude',
                                     'longitude','is_location_exact','amenities','weekly_price','monthly_price','minimum_nights',
                                     'maximum_nights','calendar_updated','calendar_last_scraped','requires_license','license',
                                     'jurisdiction_names','cancellation_policy','require_guest_profile_picture','require_guest_phone_verification',
                                     'cleaning_fee','last_scraped','host_verifications','host_acceptance_rate','market',
                                     'has_availability', 'neighbourhood_cleansed')) %>% drop_na() 

#Discovering major NA columns

na_count <-sapply(listings, function(y) sum(length(which(is.na(y)))))

#Host_response_rate - 523

#first_review - 627 - this and below essentially no reviews
#last_review - 627
#all review_scores ~ 600-650
#reviews_per_month - 627

#FIXED
#security_deposit - 1952
#cleaning_fee - 1030

#Pulling out neighborhood cleansed because it has levels that may appear in training but not test
#Ie several with 1 value per level,.

# take dummy output, etc... and use as model columns
model_df <- df %>% select(log_price, reviews_per_month, review_scores_location, bedrooms, corrected_cleaning_fee, 
                          'room_type_Private room', 'room_type_Shared room',
                          'neighbourhood_class_Lowest Demand', 'neighbourhood_class_Medium Demand')

# ---------------------STEP 1: SPLIT INTO TRAINING AND TEST SETS -------------------------------#
# Create the training and test datasets
set.seed(100)

# Get row numbers for the training data
trainRowNumbers <- createDataPartition(model_df$log_price, p=0.8, list=FALSE)

# Create the training  dataset
trainData <- model_df[trainRowNumbers,]

# Create the test dataset
testData <- model_df[-trainRowNumbers,]

# --- Fresh Split of Full 
trainRowNumbers_allvar <- createDataPartition(df_fullvar$log_price, p=0.8, list=FALSE)
trainData_allvar <- df_fullvar[trainRowNumbers_allvar,]
testData_allvar <- df_fullvar[-trainRowNumbers_allvar,]


# -------------------- STEP 2: CHECKING PRELIMINARY MODEL ASSUMPTIONS NO CV --------------------------------#

general_model_no_cv <- lm(log_price ~., data=trainData)

# residual plots
plot(general_model_no_cv)

# summary statistics and coefficients
summary(general_model_no_cv)

# AIC
extractAIC(general_model_no_cv)

rstudent(price_model_fullvar)

cooks.distance(price_model_fullvar)



# -------------------- STEP 3: TRAIN MODEL WITH CV -----------------------------------------------------#
# step 4: train the model
# define internal cross validation - 10 samples, 5 times
ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)

# train the model - lasso selected
price_model = train(log_price ~ ., data=trainData, method='lasso', 
                    na.action=na.exclude, preProcess=c("center", "scale"),
                    trControl=ctrl)

print(price_model)

#New Lasso Model
#full variables

#df_fullvar$log_price2 = log(df_fullvar$price, base = 2)
#df_fullvar = df_fullvar %>% select(-c('log_price', 'price')) 

price_model_fullvar = train(log_price ~ ., data=trainData_allvar, method='lasso', 
                            na.action=na.exclude, preProcess=c("center", "scale"),
                            trControl=ctrl)

print(price_model_fullvar)


resid_fullvar = resid(price_model_fullvar)
hist(resid_fullvar)
plot(resid_fullvar)

#Full Variables without lasso

price_model_fullvar_lm = train(log_price ~ ., data=trainData_allvar, method='lm', 
                            na.action=na.exclude, preProcess=c("center", "scale"),
                            trControl=ctrl)

price_model_fullvar_lm = train(price ~ ., data=trainData_allvar, method='lm', 
                               na.action=na.exclude, preProcess=c("center", "scale"),
                               trControl=ctrl)
print(price_model_fullvar_lm)

# try without lasso 
price_model_no_lasso = train(log_price ~ ., data=trainData, method='lm', 
                             na.action=na.exclude, preProcess=c("center", "scale"),
                             trControl=ctrl)

# display KPI of model
print(price_model_no_lasso)

# display general info of model
summary(price_model)

# store residuals for diagnostic plots
price_resids <- resid(price_model)

# check normality of residuals
hist(price_resids)


# check equality of variance of residuals
plot(trainData$log_price, price_resids)
abline(0, 0)

#check for variable importance - hasn't worked for LASSO model yet
plot(varImp(price_model_no_lasso))

plot(price_model_no_lasso)


# -------------------STEP 4: RUN PREDICTIONS ON TEST SET ----------------------------------------#

predict <- predict(price_model, testData)
# Let's see how we did. Plot predictions against actual
testData$predictions <- predict
testData %>% ggplot(aes(x=predictions, y=log_price)) + geom_point() + geom_smooth(method="lm") + 
  xlab('Predicted Price') + ylab('Actual Price') + ggtitle('Actual vs. Predicted: Log(Price [$])')

#Issues remain with some singular observations w/ a level not in training. 
#At the moment, I am removing those 1s from the test when they occur to run the test. 
testData_allvar = testData_allvar %>% filter(zipcode != "99\n98122")

#Predicting with thew new lambda model 
predict <- predict(price_model_fullvar, testData_allvar)
# Let's see how we did. Plot predictions against actual
testData_allvar$predictions <- predict
testData_allvar %>% ggplot(aes(x=predictions, y=log_price)) + geom_point() + geom_smooth(method="lm") + 
  xlab('Predicted Price') + ylab('Actual Price') + ggtitle('Actual vs. Predicted: Log(Price [$])')

###Why We logged price. 
predict <- predict(price_model_fullvar_lm, testData_allvar)
# Let's see how we did. Plot predictions against actual
testData_allvar$predictions <- predict
testData_allvar %>% ggplot(aes(x=predictions, y=price)) + geom_point() + geom_smooth(method="lm") + 
  xlab('Predicted Price') + ylab('Actual Price') + ggtitle('Actual vs. Predicted: Log(Price [$])')



