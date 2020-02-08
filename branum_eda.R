# import data sources and functions, etc....
source('main.R')
library(tidyverse)
library(ggplot2)
library(corrplot)
library(cowplot)
library(caret)
library(skimr)


# function to do this all in one go
correlator  <-  function(df){
  df %>% keep(is.numeric) %>% tidyr::drop_na() %>% cor %>%
    corrplot("upper", addCoef.col = "white", number.digits = 2,
      number.cex = 0.5, method="square",
      order="hclust", title="Variable Corr Heatmap",
      tl.srt=45, tl.cex = 0.8)
}

# print out correlation graph for numerical values
cols_list <- c("host_total_listings_count", "accommodates", "bathrooms", "bedrooms", "beds", "square_feet", "price", "cleaning_fee", "guests_included",
               "minimum_nights", "maximum_nights", "number_of_reviews", "review_scores_rating", "review_scores_cleanliness", "review_scores_location", "review_scores_value",
               "reviews_per_month")
correlator(listings %>% select(cols_list))

# create some custom features
listings$is_cancellation_strict <- ifelse(listings$cancellation_policy == "strict", "is_strict", "not_strict")

# fixing cleaning fee to fill na values
listings <- listings %>% group_by(room_type) %>% mutate(corrected_cleaning_fee = 
                                                          ifelse(is.na(cleaning_fee),0,
                                                                 cleaning_fee))

# grouping areas by general price point
listings <- listings %>% mutate(neighbourhood_class = case_when(
  neighbourhood_group_cleansed == 'Magnolia' ~ "Highest Demand",
  neighbourhood_group_cleansed == 'Downtown' ~ "Highest Demand",
  neighbourhood_group_cleansed == "Queen Anne" ~ "Highest Demand",
  neighbourhood_group_cleansed == "Ballard" ~ "Medium Demand",
  neighbourhood_group_cleansed == "Capitol Hill" ~ "Medium Demand",
  neighbourhood_group_cleansed == "Cascade" ~ "Medium Demand",
  neighbourhood_group_cleansed == "Central Area" ~ "Medium Demand",
  neighbourhood_group_cleansed == "West Seattle" ~ "Medium Demand",
  TRUE ~ "Lowest Demand"
))



# create box plots for categorical vs. price
lyst <- c("is_cancellation_strict", "neighbourhood_group_cleansed",
          "room_type", "neighbourhood_class", "has_wifi", "has_fireplace",
          "family_friendly", "has_hottub", "has_parking", "pets_allowed")
lapply(lyst, function(i)ggplot(listings, aes_string(x=i, y="price")) + 
         theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_bar(stat = 'summary', fun.y='mean'))



# grouping location to make significant



