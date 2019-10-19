library(readxl)
library(tidyverse)
library(prophet)
#install.packages("MLmetrics",repos = "https://ftp.fau.de/cran/")
library(MLmetrics)

x <-  read.csv("anyfile.csv")
df <- data.frame(x,y)
colnames(df) <- c("ds","y")


tsData = ts(df$y, start = c(2014,2), frequency = 365)
plot(tsData,col='red',lty=2)
#Outliers
outliersValue<- boxplot.stats(tsData)$out
ts1 <- tsData[!tsData %in% outliersValue]

ts1 <-   ts(ts1,start=c(2014,2),frequency = 365)
plot(ts1,col='black',lwd=1,type='l')

#Trend Forecasting
########################################################################
x <- seq(1,length(ts1))

loess1 <- loess(ts1~x,
                span=0.4,
                control = loess.control(surface = "direct"))
trend_fitted <- predict(loess1)
lines(trend_fitted, col="yellow", lwd=3)


# Prediction
x_pred <- seq(max(x),max(x)+(390))
trend_pred <- predict(loess1, newdata = x_pred)
lines(c(trend_fitted,trend_pred), type="l", lty=2, lwd=3, col="grey")
tr <- c(trend_fitted,trend_pred)

########################################################################
#Seasonality forecasting - prophet
#Holidays
easter <- data_frame(holiday = 'Easter',
                     ds = as.Date(c('2015-04-03', '2016-03-25', '2017-04-14', '2018-03-30', '2019-04-19')),
                     lower_window = -7, upper_window = 7)
Himmel <- data_frame(holiday = 'Himmelfahrt',
                     ds = as.Date(c('2015-05-14', '2016-05-05', '2017-05-25', '2018-05-10', '2019-05-30','2019-10-03')),
                     lower_window = -4, upper_window = 3)
Pfingsten <- data_frame(holiday = 'pfingsten',
                        ds = as.Date(c('2015-05-25', '2016-05-16', '2017-06-05', '2018-05-21', '2019-06-10')),
                        lower_window = -5, upper_window = 2)
Corpus <- data_frame(holiday = 'Corpus',
                     ds = as.Date(c('2015-06-04', '2016-05-26', '2017-06-15', '2018-05-31', '2019-06-20', '2019-10-31')),
                     lower_window = -5, upper_window = 4)
xmas <- data_frame(holiday = 'Xmas',
                   ds = as.Date(c('2015-12-24', '2016-12-24', '2017-12-24', '2018-12-24', '2019-12-24')),
                   lower_window = -4, upper_window = 4)
NY <- data_frame(holiday = 'New_Year',
                 ds = as.Date(c('2014-12-31','2015-12-31','2016-12-31', '2017-12-31','2018-12-31', '2019-12-31')),
                 lower_window = -3, upper_window = 4)
HOL_MON <- data_frame(holiday = 'Holiday_monday',
                      ds = as.Date(c('2016-10-03', '2017-05-01','2016-10-31')),
                      lower_window = -7, upper_window = 7)
HOL_TUE <- data_frame(holiday = 'Holiday_tuesday',
                      ds = as.Date(c('2017-10-03','2017-10-31', '2018-05-01')),
                      lower_window = -7, upper_window = 7)
HOL_WED <- data_frame(holiday = 'Holiday_wednesday',
                      ds = as.Date(c('2018-10-03', '2018-10-31','2019-05-01')),
                      lower_window = -7, upper_window = 7)
HOL_FRI <- data_frame(holiday = 'Holiday_friday', 
                      ds = as.Date(c('2015-05-01', '2019-03-08')),
                      lower_window = -7, upper_window = 7)

holidays <- bind_rows(Allsaintday,ArmisticeDay,AscensionDay,AssumptionDay,xmas,easter,Independence_day,labourday,newyear)


detrended_data = ts1 - trend_fitted
ds <-  seq(as.Date('2014-01-01'),by="days",length.out = length(ts1))
y <-  detrended_data

df <-  data.frame(ds,y)


summer_sundays <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric((weekdays(dates) == "Sunday") & (month > 5 & month < 9))
}
df$summer_sundays <- summer_sundays(df$ds)

summer_saturdays <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric((weekdays(dates) == "Saturday") & (month > 5 & month < 9))
}
df$summer_saturdays <- summer_saturdays(df$ds)

m <- prophet(holidays = holidays, 
             yearly.seasonality = TRUE, 
             weekly.seasonality = TRUE, 
             daily.seasonality = T, 
             seasonality.mode = 'multiplicative',
             seasonality.prior.scale = 100, 
             changepoint.prior.scale = 0.6,  holidays.prior.scale = 100
)
m <- add_regressor(m, 'summer_sundays')
m <- add_regressor(m, 'summer_saturdays')
m <- fit.prophet(m,df)
FC_period <- as.Date(tail(df$ds,1) + 390,  format="%Y-%m-%d")
forecast_days <- difftime(FC_period,tail(df$ds,1),  units = c("days"))
future <- make_future_dataframe(m, periods = forecast_days)
future$summer_sundays <- summer_sundays(future$ds)
future$summer_saturdays <- summer_saturdays(future$ds)
forecast <- predict(m, future)

dyplot.prophet(m,forecast)
plot(m, forecast)
prophet_plot_components(m, forecast)

df.cv <- cross_validation(m, horizon =30 , initial =500,period = 120 ,units = 'days')
#df.p <- performance_metrics(df.cv)
#mean(df.p$mape)




