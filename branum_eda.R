# import data sources and functions, etc....
source('main.R')
library(tidyverse)
library(ggplot2)
library(corrplot)
library(cowplot)


# select only initially applicable columns - keep all but urls
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

# ensure categorical values are in fact stored as factors
factor_list <- c('bedrooms', 'minimum_nights', 'beds')
df <- df %>% mutate_each_(funs(factor), factor_list)

# function to do this all in one go
correlator  <-  function(df){
  df %>% keep(is.numeric) %>% tidyr::drop_na() %>% cor %>%
    corrplot("upper", addCoef.col = "white", number.digits = 2,
      number.cex = 0.5, method="square",
      order="hclust", title="Variable Corr Heatmap",
      tl.srt=45, tl.cex = 0.8)
}

# print out correlation graph for numerical values
correlator(df)

# assess categorical variables via density plots
target <- "review_scores_rating"
# step 2, save explanator variable names
numvars <- df %>% keep(is.numeric) %>% colnames


numplot <- function(df, explan, resp) {
  ggplot(data = df) + geom_density(aes_string(x = explan, fill = resp), alpha = 0.5)
}

plotlist <- lapply(numvars, function(x) numplot(df, x, target))
lapply(numvars, function(x) numplot(df, x, target))
plot_grid(plotlist = plotlist)

# check for distribution of review ratings
summary(main$review_scores_rating)
main %>% ggplot(aes(x=review_scores_rating)) + geom_histogram(bins=60)

# you can see here that there isn't really any correlation between price and rating
main %>% ggplot(aes(x=price, y=review_scores_rating)) + geom_point(aes(colour=property_type))

# let's see how the rating looks with the different comment scores - once again not much
main %>% ggplot(aes(x=total_is_clean, y=review_scores_rating)) + geom_point()

# we can try looking at a specific price point - maybe customers expect different for different price points?
# the spread is much larger for below 250... let's look there for a start
main %>% filter(price < 250) %>% ggplot(aes(x=total_good_location, y=review_scores_rating)) + geom_point()

