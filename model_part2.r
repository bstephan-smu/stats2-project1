library(tidyverse)
library(tseries)
library(forecast)

#get Review data and remove extraneous columns
reviews <- read.csv("resources/data/reviews.csv")
head(reviews)
review_grouped_day <- reviews %>%
  mutate(date = as.Date(date)) %>%
  count(date)
head(review_grouped_day)
n = length(review_grouped_day$n)
#is it stationary -- nope
review_grouped_day %>%
  ggplot(aes(x=date, y=n)) +
  geom_point()
Acf(review_grouped_day$n[1:n/2])
Acf(review_grouped_day$n[n/2:n])
Acf(review_grouped_day$n)
Pacf(review_grouped_day$n)
#try stationarize this by log transform -- doesnt work
review_grouped_day <- review_grouped_day %>%
  mutate(logn = log(n)) 
review_grouped_day %>%
  ggplot(aes(x=date, y=log(n))) +
  geom_point()
Acf(review_grouped_day$logn)
Pacf(review_grouped_day$logn)
#get rid of pre-2012 and add predictor of year--doesn't work
review_grouped_day %>%
  filter(date > "2012-01-01")
date2 = as.numeric(review_grouped_day$date)^2
reviews_lm = lm(n~date+date2, data=review_grouped_day)
lm_predictions <- predict(reviews_lm)
predictor = lm_predictions
plot(lm_predictions)
plot(review_grouped_day$n, lm_predictions)

simpleols<-arima(review_grouped_day$n,order=c(0,0,0),xreg=predictor)
tsdisplay(residuals(simpleols),lag.max=15,main="Resid. Diagnostics with Year Predictor")
ar14year <-arima(review_grouped_day$n,order=c(14,0,0),xreg=predictor)
tsdisplay(residuals(ar14year),lag.max=15,main="Resid. Diagnostics with AR14 and year predictor")
logar14year <-arima(log(review_grouped_day$n),order=c(14,0,0),xreg=predictor)
tsdisplay(residuals(logar14year),lag.max=50,main="Resid. Diagnostics after log with ar14 and year predictor")
logar14 <-arima(log(review_grouped_day$n),order=c(14,0,0))
tsdisplay(residuals(logar14),lag.max=28,main="Resid. Diagnostics after log with ar14")
logar31 <-arima(log(review_grouped_day$n),order=c(31,0,0))
tsdisplay(residuals(logar31),lag.max=100,main="Resid. Diagnostics after log with ar31")

logar14 <-arima(log(review_grouped_day$n),order=c(14,1,0))
tsdisplay(residuals(logar14),lag.max=28,main="Resid. Diagnostics after log with arima 14, 1, 0")


#stationarize this by differencing-- log and difference does work
diff.data<-arima(review_grouped_day$n,order=c(0,1,0))
tsdisplay(residuals(diff.data),lag.max=15,main="Resid. Diagnostics 1st Order Difference")
diff.data<-arima(log(review_grouped_day$n),order=c(0,1,0))
tsdisplay(residuals(diff.data),lag.max=15,main="Resid. Diagnostics Log 1st Order Difference")

#stationarize by doing percent number of listings by month year -- works better?
review_grouped_mo <- reviews %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(review_count_mo = sum(n_distinct(id)))
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
#plot is looking pretty stationary but ACF seems never to die
reviews_percentized %>%
  ggplot(aes(x=date, y=percentized)) +
  geom_point()
Acf(reviews_ts)
Pacf(reviews_ts)
ARIMA.fit<-auto.arima(reviews_ts,seasonal=TRUE,stepwise=FALSE)
ARIMA.fit #picked arima(4,1,1)
tsdisplay(residuals(ARIMA.fit),lag.max=15,main="Resid. Diagnostics Arima 4 1 1")

ARIMA.fit <- arima(reviews_ts, order=c(14, 1, 1))
tsdisplay(residuals(ARIMA.fit),lag.max=33,main="Resid. Diagnostics Arima 14 1 1")
n = length(forecast(ARIMA.fit))
plot(forecast(ARIMA.fit,h=10),xlim=c(1000, 1100))
points(1:length(reviews_ts),fitted(ARIMA.fit),type="l",col="blue")
res_arima <- as.vector(residuals(ARIMA.fit))
durbinWatsonTest(res_arima,max.lag=4)
