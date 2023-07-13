# AMTRAK-Ridership-Forecasting
For this project, data from the United States Department of Transportation: Bureau of Transportation Statistics has been used to predict monthly ridership for the 12 months of the year 2014. The analysis is based on a single time series: 22 years. From analysis it was identified that the dataset has an upward trend and a seasonality, and the data is highly auto-correlated, as the autocorrelation coefficients in all 12 lags are significant.
 
The models described below were implemented in this project:

1. Autoregressive Integrated Moving Average (ARIMA) model.
   
2. Advanced Smoothing Method- Holt-Winters model.
   
3. Moving Average model.
   
4. ​​Regression model with Linear Trend and Seasonality + the Auto-Regressive model of its residuals
   
5. Regression model with Quadratic Trend and Seasonality + the Auto-Regressive model of its residuals
 
The model evaluation was performed based on the RMSE and MAPE accuracy metrics.

The study indicated that the best model to use was the Centered Moving Average.
