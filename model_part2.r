library(tidyverse)
library(tseries)
library(forecast)
# ---------------------STEP 1: READ AND CLEAN DATA -------------------------------#
#get Review data
reviews <- read.csv("resources/data/reviews.csv")
head(reviews)
#keep only date and count of reviews
review_grouped_day <- reviews %>%
  mutate(date = as.Date(date)) %>%
  count(date)
head(review_grouped_day)
n = length(review_grouped_day$n)

# ---------------------STEP 2: STATIONARIZE THE DATA -------------------------------#

#is it stationary -- nope
#scatterplot
review_grouped_day %>%
  ggplot(aes(x=date, y=n)) +
  geom_point()
#ACF and PCF plots
Acf(review_grouped_day$n[1:n/2]) #ACF looks different for first half and second half of data
Acf(review_grouped_day$n[n/2:n])
Acf(review_grouped_day$n)
Pacf(review_grouped_day$n)

#try stationarize this by log transform -- doesnt work
review_grouped_day <- review_grouped_day %>%
  mutate(logn = log(n)) 
review_grouped_day %>%
  ggplot(aes(x=date, y=log(n))) +
  geom_point()
Acf(review_grouped_day$logn) #ACF never dies
Pacf(review_grouped_day$logn)

#get rid of pre-2012 and add predictor of year--doesn't work
#removing pre-2012 data
review_grouped_day <- review_grouped_day %>%
  filter(date > "2012-01-01")
#linear model with date and date squared
date2 = as.numeric(review_grouped_day$date)^2
reviews_lm = lm(n~date+date2, data=review_grouped_day)
lm_predictions <- predict(reviews_lm)
predictor = lm_predictions
plot(lm_predictions)
plot(lm_predictions,review_grouped_day$n, ylab = "review count")
#LM as predictor residual plots still show increasing variance--not stationary
simpleols<-arima(review_grouped_day$n,order=c(0,0,0),xreg=predictor)
tsdisplay(residuals(simpleols),lag.max=15,main="Resid. Diagnostics with Year Predictor")

#stationarize this by differencing-- log and difference does work
#just differencing shows increasing variance
diff.data<-arima(review_grouped_day$n,order=c(0,1,0))
tsdisplay(residuals(diff.data),lag.max=15,main="Resid. Diagnostics 1st Order Difference")
#log and then differencing looks like it has stationarized it
diff.data<-arima(log(review_grouped_day$n),order=c(0,1,0))
tsdisplay(residuals(diff.data),lag.max=15,main="Resid. Diagnostics Log 1st Order Difference")

#stationarize by doing percent number of listings by month year -- works better?
#counting up reviews by month
review_grouped_mo <- reviews %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(review_count_mo = sum(n_distinct(id)))
#wrangling to match each date with previous month's review count
review_grouped_mo = review_grouped_mo %>%
  mutate(monthnum = as.numeric(month)) %>%
  mutate(nextmonth = monthnum+1) %>%
  mutate(nextmonth=replace(nextmonth, monthnum==12, 1)) %>%
  mutate(year_alt = replace(year, monthnum==12, as.character(as.numeric(year)+1)))
head(review_grouped_mo, 15)
tail(review_grouped_mo, 15)
head(review_grouped_day)
reviews_percentized = review_grouped_day %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  mutate(month = as.numeric(month)) %>%
  left_join(review_grouped_mo, by = c("month" = "nextmonth", "year" = "year_alt")) %>%
  mutate(percentized = n/review_count_mo) %>%
  filter(year > 2012)
head(reviews_percentized, 15)
reviews_ts = reviews_percentized$percentized
#scatterplot is looking pretty stationary but ACF seems never to die
reviews_percentized %>%
  ggplot(aes(x=date, y=percentized)) +
  geom_point()
Acf(reviews_ts)
Pacf(reviews_ts)

# -------------------- STEP 3: FIT MODEL -----------------------------------------------------#

#log AR14 with year predictor shows good residual plots
logar14year <-arima(log(review_grouped_day$n),order=c(14,0,0),xreg=predictor)
tsdisplay(residuals(logar14year),lag.max=50,main="Resid. Diagnostics after log with ar14 and year predictor")
#log AR14 without year predictor also shows good residual plots but peaks in ACF at 14 and 28
logar14 <-arima(log(review_grouped_day$n),order=c(14,0,0))
tsdisplay(residuals(logar14),lag.max=28,main="Resid. Diagnostics after log with ar14")
#log AR31 residual plot looks good but is it too complex?
logar31 <-arima(log(review_grouped_day$n),order=c(31,0,0))
tsdisplay(residuals(logar31),lag.max=100,main="Resid. Diagnostics after log with ar31")
#log AR14 with difference of 1 looks good but also still peaks at 14 and 28 lags
logar14 <-arima(log(review_grouped_day$n),order=c(14,1,0))
tsdisplay(residuals(logar14),lag.max=28,main="Resid. Diagnostics after log with arima 14, 1, 0")
#Auto Arima picks arima(4,1,1)
ARIMA.fit<-auto.arima(reviews_ts,seasonal=TRUE,stepwise=FALSE)
ARIMA.fit
tsdisplay(residuals(ARIMA.fit),lag.max=15,main="Resid. Diagnostics Arima 4 1 1")
#arima(14,1,1) looks pretty good
ARIMA.fit <- arima(reviews_ts, order=c(14, 1, 1))
tsdisplay(residuals(ARIMA.fit),lag.max=33,main="Resid. Diagnostics Arima 14 1 1")

#forecast the future with our arima model
n = length(forecast(ARIMA.fit))
plot(forecast(ARIMA.fit,h=10),xlim=c(1000, 1100))
points(1:length(reviews_ts),fitted(ARIMA.fit),type="l",col="blue")
res_arima <- as.vector(residuals(ARIMA.fit))
