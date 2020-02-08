library(caret)
library(fastDummies)
library(skimr)

# importing datasets from main.r and eda
source('branum_eda.R')

# importing the output eda model from eda.R
# define which column values to use for model and create dummies
listings$log_reviews <- log(listings$number_of_reviews)
listings$log_price <- log(listings$price)

df <- listings  %>% select(log_price, reviews_per_month, review_scores_location, bedrooms, corrected_cleaning_fee, 
                           room_type, is_cancellation_strict, neighbourhood_class) %>% drop_na() %>% dummy_cols(remove_first_dummy = TRUE)

model_df <- df %>% select(log_price, reviews_per_month, review_scores_location, bedrooms, corrected_cleaning_fee, 
                          'room_type_Private room', 'room_type_Shared room', 'is_cancellation_strict_is_strict',
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

# --------------------STEP 2: TRAIN MODEL -----------------------------------------------------#
# step 4: train the model
# define internal cross validation - 10 samples, 5 times
ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)

# train the model - lasso selected
price_model = train(log_price ~ ., data=trainData, method='lasso', 
                   na.action=na.exclude, preProcess=c("center", "scale"),
                   trControl=ctrl)

print(price_model)

# try without lasso 
price_model_no_lasso = train(log_price ~ ., data=trainData, method='lm', 
                    na.action=na.exclude, preProcess=c("center", "scale"),
                    trControl=ctrl)

print(price_model_no_lasso)

# -------------------STEP 3: RUN PREDICTIONS ON TEST SET ----------------------------------------#

predict <- predict(price_model, testData)
# Let's see how we did. Plot predictions against actual
testData$predictions <- predict
testData %>% ggplot(aes(x=predictions, y=log_price)) + geom_point() + geom_smooth(method="lm") + 
  xlab('Predicted Price') + ylab('Actual Price') + ggtitle('Actual vs. Predicted: Log(Price [$])')


