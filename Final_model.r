library(caret)
library(fastDummies)
library(skimr)
library(tidyverse)

# importing datasets from main.r and eda
source('main.r')


###First OLS Model###

df <- listings  %>% select(log_price, reviews_per_month, review_scores_location, bedrooms, corrected_cleaning_fee, 
                           room_type, neighbourhood_class) %>% drop_na() %>% dummy_cols(remove_first_dummy = TRUE)


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

# -------------------- STEP 2: CHECKING PRELIMINARY MODEL ASSUMPTIONS NO CV --------------------------------#


general_model_no_cv <- lm(log_price ~., data=trainData)

# residual plots
plot(general_model_no_cv)

# summary statistics and coefficients
summary(general_model_no_cv)

# AIC
extractAIC(general_model_no_cv)

# -------------------- STEP 3: TRAIN MODEL WITH CV -----------------------------------------------------#
# step 4: train the model
# define internal cross validation - 10 samples, 5 times
ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)

# try without lasso 
price_model = train(log_price ~ ., data=trainData, method='lm', 
                             na.action=na.exclude, preProcess=c("center", "scale"),
                             trControl=ctrl)

# display KPI of model
print(price_model)

#check for variable importance - hasn't worked for LASSO model yet
plot(varImp(price_model))

plot(price_model)

# -------------------STEP 4: RUN PREDICTIONS ON TEST SET ----------------------------------------#

predict <- predict(price_model, testData)
# Let's see how we did. Plot predictions against actual
testData$predictions <- predict
testData %>% ggplot(aes(x=predictions, y=log_price)) + geom_point() + geom_smooth(method="lm") + 
  xlab('Predicted Price') + ylab('Actual Price') + ggtitle('Actual vs. Predicted: Log(Price [$])')

#RMSE 
RMSE(predict, testData$log_price)



###-----------------------Lasso Selected Model----------------------------###

#Subset original listings by removing non applicable, or low information columns
#Includes removing columns with 1 or 2 levels and factor columns with too many levels to adequately split

#Dropping NA values leaves about 2800 observations to split into training and testings

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
                                     'has_availability', 'neighbourhood_cleansed', 'price')) %>% drop_na() 

# ---------------------STEP 1: SPLIT INTO TRAINING AND TEST SETS -------------------------------#
# Get row numbers for the training data
trainRowNumbers_allvar <- createDataPartition(df_fullvar$log_price, p=0.8, list=FALSE)

# Create the training  dataset
trainData_allvar <- df_fullvar[trainRowNumbers_allvar,]

# Create the test dataset
testData_allvar <- df_fullvar[-trainRowNumbers_allvar,]

# -------------------- STEP 3: TRAIN MODEL WITH CV -----------------------------------------------------#


price_model_fullvar = train(log_price ~ ., data=trainData_allvar, method='lasso', 
                            na.action=na.exclude, preProcess=c("center", "scale"),
                            trControl=ctrl)

print(price_model_fullvar)


resid_fullvar = resid(price_model_fullvar)
hist(resid_fullvar)
plot(resid_fullvar, ylab = "Residuals")

#Residuals by Actual 
plot(trainData_allvar$log_price, resid_fullvar, ylab = "Residuals", xlab = "Actual Log Training Price")


#Full Variables without lasso

price_model_fullvar_lm = train(log_price ~ ., data=trainData_allvar, method='lm', 
                               na.action=na.exclude, preProcess=c("center", "scale"),
                               trControl=ctrl)

price_model_fullvar_lm = train(price ~ ., data=trainData_allvar, method='lm', 
                               na.action=na.exclude, preProcess=c("center", "scale"),
                               trControl=ctrl)
print(price_model_fullvar_lm)



# -------------------STEP 4: RUN PREDICTIONS ON TEST SET ----------------------------------------#


testData_allvar = testData_allvar %>% filter(property_type != "Yurt")
#Predicting with thew new lambda model 
predict_fullvar <- predict(price_model_fullvar, testData_allvar)
# Let's see how we did. Plot predictions against actual
testData_allvar$predictions <- predict_fullvar
testData_allvar %>% ggplot(aes(x=predictions, y=log_price)) + geom_point() + geom_smooth(method="lm") + 
  xlab('Predicted Price') + ylab('Actual Price') + ggtitle('Actual vs. Predicted: Log(Price [$])')

RMSE(predict_fullvar, testData_allvar$log_price)

#-------------------- Predicting on Price, not Log_Price ----------------------#
df_fullvar_p <- listings %>% select(-c('name', 'host_location','square_feet', 'license', 'host_name','smart_location',
                                     'id','listing_url','scrape_id', 'summary', 'space','description', 'experiences_offered',
                                     'neighborhood_overview', 'notes', 'transit','thumbnail_url','medium_url','picture_url',
                                     'xl_picture_url','host_id','host_url','host_name', 'host_location', 'host_about','host_thumbnail_url',
                                     'host_picture_url','host_neighbourhood', 'host_has_profile_pic','host_identity_verified',
                                     'street','neighbourhood','city','state','smart_location','country_code','country','latitude',
                                     'longitude','is_location_exact','amenities','weekly_price','monthly_price','minimum_nights',
                                     'maximum_nights','calendar_updated','calendar_last_scraped','requires_license','license',
                                     'jurisdiction_names','cancellation_policy','require_guest_profile_picture','require_guest_phone_verification',
                                     'cleaning_fee','last_scraped','host_verifications','host_acceptance_rate','market',
                                     'has_availability', 'neighbourhood_cleansed', 'log_price')) %>% drop_na() 

# Get row numbers for the training data
trainRowNumbers_allvar_p <- createDataPartition(df_fullvar_p$price, p=0.8, list=FALSE)

# Create the training  dataset
trainData_allvar_p <- df_fullvar_p[trainRowNumbers_allvar_p,]

# Create the test dataset
testData_allvar_p <- df_fullvar_p[-trainRowNumbers_allvar_p,]

price_model_nolog = train(price ~ ., data=trainData_allvar_p, method='lasso', 
                          na.action=na.exclude, preProcess=c("center", "scale"),
                          trControl=ctrl)
testData_allvar_p = testData_allvar_p %>% filter(property_type !=  "Chalet")
plot(trainData_allvar_p$price,resid(price_model_nolog), ylab = "Residuals of Non - Logged Price Model", xlab = "Actual Price")

