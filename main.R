library(here)

# locate project root directory
data_root <- here()

# read in csv files from resources into dataframes
calender <- read.csv(paste(data_root, "/resources/data/calendar.csv", sep=""))
listings <- read.csv(paste(data_root, "/resources/data/listings.csv", sep=""))
reviews <- read.csv(paste(data_root, "/resources/data/reviews.csv", sep=""))