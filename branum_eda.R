# import data sources and functions, etc....
source('main.R')
library(tidyverse)

# select only initially applicable columns - keep all but urls
df <- listings %>% select('id', 'name', 'experiences_offered', 'host_id', 'host_location', 'host_acceptance_rate', 'host_neighbourhood',
                          'neighbourhood_cleansed', 'zipcode', 'property_type', 'bedrooms', 'square_feet', 'security_deposit', 'minimum_nights',
                          'availability_30', 'review_scores_rating', 'review_scores_communication', 'license', 'summary', 'neighborhood_overview',
                          'host_about', 'host_is_superhost', 'host_listings_count', 'host_identity_verified', 'neighbourhood_group_cleansed','market',
                          'beds', 'price', 'cleaning_fee', 'maximum_nights', 'availability_60', 'number_of_reviews', 'review_scores_accuracy', 'review_scores_location',
                          'jurisdiction_names', 'require_guest_phone_verification', 'host_name', 'host_response_time', 'host_total_listings_count', 'street', 'city',
                          'smart_location', 'accommodates', 'bed_type', 'weekly_price', 'guests_included', 'availability_90', 'first_review', 'review_scores_cleanliness',
                          'review_scores_value', 'instant_bookable', 'calculated_host_listings_count', 'description', 'transit', 'host_since', 'host_response_rate',
                          'host_verifications', 'neighbourhood', 'bathrooms', 'amenities', 'monthly_price', 'extra_people', 'has_availability', 'availability_365',
                          'last_review', 'review_scores_checkin', 'requires_license', 'cancellation_policy', 'reviews_per_month')

# check for correlation amongst numerical values
