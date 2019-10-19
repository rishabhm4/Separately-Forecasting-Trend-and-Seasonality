# Forecasting Trend and Seasonality component separately

- Sometimes we need to weight the trend component more/less, when the model is not able to do so as per buisness case requirement
- Seasonality can be predicted by numerous models, however I will be using Prophet from Facebook (Do also try Kalman Filtering with ARIMA)

## Trend
- Used `loess` [function](https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/loess), where `span` argument is used for smoothning the time series
- Trend can be predicted using `predict()` function

## Seasonality
- For selecting the seasonality part, subracted the trend part from the data
- Forecasting it using time series models



##### Depending on the Slope, the Model can be:
- Additive       -> Trend + Seasonality Forecast
- Mulitplicative -> Trend x Seasonality Forecast
