# Dataset : Amtrak Ridership (Jan'91 - May'13)

#Step-1: Define Goal
#Forecasting Amtrak ridership.



#Step-2: Get Data
#Packages & Libraries
install.packages("MASS")
install.packages("forecast")
library(forecast)
library(zoo)
library(readr)

#Se# setting up working directory
setwd("~/Desktop/Project/")

# creating data frame
ridership.data <- read.csv("amtrak_ridership.csv")

# display summary of the data
summary(ridership.data)

# display first 6 data of the frame.
head(ridership.data)

# display last 6 data of the frame.
tail(ridership.data)

# minimum value of Number_of_Passengers
min(ridership.data$Number_of_Passengers)

# maximum value of Number_of_Passengers
max(ridership.data$Number_of_Passengers)


#Creating the time series data set in R using the ts() function.
ridership.ts <- ts(ridership.data$Number_of_Passengers, start=c(1991,1), end=c(2013,5), freq=12)
ridership.ts



#Step-3: Explore & Visualize Series.
#Plotting the time series data using the function plot().
plot(ridership.ts, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", 
     ylim = c(1300000, 2900000), bty = "l",
     xaxt = "n", xlim = c(1991, 2013.5), main = "Amtrak Ridership", lwd = 3, col="blue") 
axis(1, at = seq(1991, 2013.5, 1), labels = format(seq(1991, 2013.5, 1)))


#Plotting the time series data with regression trend lines.
#Using the tslm() function to create linear trend and quadratic trend for time series data.
ridership.lin <- tslm(ridership.ts ~ trend)
ridership.quad <- tslm(ridership.ts ~ trend + I(trend^2))


#Using the plot() function to create plot with linear trend. 
plot(ridership.ts, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)",
     ylim = c (1300000, 2900000), main = "Amtrak Ridership with Linear Trendline", 
     col="blue")
lines(ridership.lin$fitted, lwd = 2)


#Using the plot() function to create plot with quadratic trend. 
plot(ridership.ts, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)",
     ylim = c (1300000, 2900000), main = "Amtrak Ridership with Quadratic Trendline", 
     col="blue")
lines(ridership.quad$fitted, lwd = 2)


#Zooming-in to visualize the trend in last 5 years.
ridership.ts.5yrs <- window(ridership.ts, start = c(2008, 6), end = c(2013, 5))
plot(ridership.ts.5yrs, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", 
     ylim = c (1300000, 2900000), main = "Amtrak Ridership in the last 5 years", 
     col = "blue")


#Add-on plot showing trends & seasonality.
ridership.stl <- stl(ridership.ts, s.window = "periodic")
autoplot(ridership.stl, main = "Amtrak Ridership - Trends & Seasonality")


#Computing the autocorrelation.
#Autocorrelation measures the linear relationship between lagged values of a time series.
#The function Acf() is used to identify the autocorrelation and plot the autocorrelation, 
#for different lags (up to maximum of 12).
autocor <- Acf(ridership.ts, lag.max=12, main = "Auto-Correlation for Amtrak Ridership")


#Display the autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)



#Step-4: Pre-Process Data.
#Pre-processing is done to detect potential issues and challenges with data.
#Since there were no issues & challenges associated with the dataset, this step is skipped.



#Step-5: Partition Time Series.
#Data Set Time Range : Jan'91 - May'13 -> 269 months.
#Training Partition = 80% = 0.8 * 269 = 215.2 ~ 215 months.
#Validation Partition = 20% = 269 - 215 = 54 months.
#Partitioning the data set with the validation partition of 54 monthly periods and 
#the training partition of 215 monthly periods.
nTrain <- round(length(ridership.ts)*0.8, 0)
nValid <- length(ridership.ts) - nTrain

nTrain
nValid


train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

train.ts
valid.ts


#Plotting the time series data & visualizing the partitions.
plot(train.ts, col = "green",
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)",  
     ylim = c (1300000, 2900000), bty = "l",
     xaxt = "n", xlim = c(1991, 2017), main = "Data Partition Visualization", lwd = 2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)))
lines(valid.ts, col = "blue", lty = 1, lwd = 2)

lines(c(2008.9167, 2008.9167), c(0, 3000000))
lines(c(2013.4167, 2013.4167), c(0, 3000000))


text(1998, 2950000, "Training")
text(1998, 2950000, "Validation")
text(2015, 2950000, "Future")

arrows(1991, 2895000, 2008.9167, 2895000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2008.9167, 2895000, 2013.4167, 2895000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2013.4167, 2895000, 2017, 2895000, code = 3, length = 0.1,lwd = 1, angle = 30)



#Step-6: Applying Forecasting Methods.
#Forecasting methods can be roughly divided into a)model-based methods & b)data-driven methods.

#A. Model-based methods apply statistical, mathematical, or other scientific model to forecast time series, 
#especially advantageous for time series with small number of records.
#The Model-based methods available are:
#1.Simple and multiple linear and  non-linear regression.
#2.Autoregressive models
#3.ARIMA
#4.Logistic regression
#5.Econometric models

#B. Data-driven methods  are data dependent and “learn” patterns from data 
#The Data-driven methods available are:
#1.    Naïve forecasts
#2.    Moving average 
#2.1   Centered MA
#2.2   Trailing MA
#2.3   Weighted MA
#3.    Exponential smoothing
#3.1   Simple exponential smoothing (SES)
#3.2   Advanced exponential smoothing (AES)
#3.2.1 Holt’s model for data with trend
#3.2.2 Winter’s (Holt-Winter’s) model for data with trend and seasonality
#4.    Neural nets

#In this work, we are implementing the following methods:
#1. Winter’s (Holt-Winter’s) model for data with trend and seasonality
#2. ARIMA
#3. Simple and multiple linear and  non-linear regression.
#4. <need to fill>
#5. <need to fill>


## ----- LINEAR TREND --------
# developing a regression model with linear trend (training data)
training.lin.trend <- tslm(train.ts ~ trend)
summary(training.lin.trend)
# forecasting in the validation period
training.lin.trend.pred <- forecast(training.lin.trend, 
                                    h = nValid, level = 0)
training.lin.trend.pred

# Plot predictions for linear trend forecast.
plot(training.lin.trend.pred$mean, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", ylim = c(1300000, 2900000), bty = "l",
     xlim = c(1991, 2017), main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(training.lin.trend$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(nValid, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2008.9167, 2008.9167), c(0, 3000000))
lines(c(2013.4167, 2013.4167), c(0, 3000000))

text(1998, 2950000, "Training")
text(2011, 2950000, "Validation")
text(2015, 2950000, "Future")

arrows(1991, 2895000, 2008.9167, 2895000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2008.9167, 2895000, 2013.4167, 2895000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2013.4167, 2895000, 2017, 2895000, code = 3, length = 0.1,lwd = 1, angle = 30)


## ----- LINEAR TREND AND SEASONALITY--------
# developing a regression model with linear trend and seasonality (training data)
training.lin.trend.seas <- tslm(train.ts ~ trend + season)
summary(training.lin.trend.seas)
# forecasting in the validation period
training.lin.trend.seas.pred <- forecast(training.lin.trend.seas, 
                                         h = nValid, level = 0)
training.lin.trend.seas.pred

# Plot predictions for linear trend and seasonality forecast.
plot(training.lin.trend.seas.pred$mean, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", ylim = c(1300000, 2900000), bty = "l",
     xlim = c(1991, 2017), main = "Linear Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(training.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(nValid, col = "black", lty = 1)



# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2008.882, 2008.882), c(0, 3000000))
lines(c(2013.364, 2013.364), c(0, 3000000))

text(1998, 2950000, "Training")
text(2011, 2950000, "Validation")
text(2015, 2950000, "Future")

arrows(1991, 2950000, 2008.882, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.882, 2950000, 2013.364, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.364, 2950000, 2017, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ----- QUADRATIC TREND--------
# developing a regression model with quadratic trend (training data)
training.quad.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(training.quad.trend)
# forecasting in the validation period
training.quad.trend.pred <- forecast(training.quad.trend, 
                                     h = nValid, level = 0)
training.quad.trend.pred

# Plot predictions for quadratic trend forecast.
plot(training.quad.trend.pred$mean, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", ylim = c(1300000, 2900000), bty = "l",
     xlim = c(1991, 2017), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(training.quad.trend$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(nValid, col = "black", lty = 1)



# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2008.861, 2008.861), c(0, 3000000))
lines(c(2013.343, 2013.343), c(0, 3000000))

text(1998, 2950000, "Training")
text(2011, 2950000, "Validation")
text(2015, 2950000, "Future")

arrows(1991, 2950000, 2008.861, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.861, 2950000, 2013.343, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.343, 2950000, 2017, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## ----- QUADRATIC TREND AND SEASONALITY--------
# developing a regression model with quadratic trend (training data)
training.quad.trend.seas <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(training.quad.trend.seas)
# forecasting in the validation period
training.quad.trend.seas.pred <- forecast(training.quad.trend.seas, 
                                          h = nValid, level = 0)
training.quad.trend.seas.pred

# Plot predictions for quadratic trend and seasonality forecast.
plot(training.quad.trend.seas.pred$mean, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", ylim = c(1300000, 2950000), bty = "l",
     xlim = c(1991, 2017), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(training.quad.trend.seas$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(nValid, col = "black", lty = 1)


# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2008.850, 2008.850), c(0, 2950000))
lines(c(2013.322, 2013.322), c(0, 2950000))

text(1998, 2950000, "Training")
text(2011, 2950000, "Validation")
text(2015, 2950000, "Future")

arrows(1991, 2950000, 2008.850, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.850, 2950000, 2013.322, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.322, 2950000, 2017, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Accuracy of regression model with linear trend (training data)
round(accuracy(training.lin.trend.pred$mean, nValid), 3)
# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, nValid), 3)
# Accuracy of regression model with quadratic trend (training data)
round(accuracy(training.quad.trend.pred$mean, nValid), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, nValid), 3)


#Moving Average

# Create centered moving average with window k = 2, 6, and 12.
ma.centered_2 <- ma(ridership.ts, order = 2)
ma.centered_6 <- ma(ridership.ts, order = 6)
ma.centered_12 <- ma(ridership.ts, order = 12)




# Plot original data and centered MA for window widths of k= 2 and 12. 
plot(ridership.ts, 
     xlab = "Time Period", ylab = "Ridership (in 000s)", ylim = c(1300000, 2900000), bty = "l",
     xlim = c(1991, 2013.5), main = "Centered Moving Average") 
axis(1, at = seq(1991, 2013.5, 1), labels = format(seq(1991, 2013.5, 1)))
lines(ma.centered_2, col = "brown", lwd = 2)
lines(ma.centered_12, col = "blue", lwd = 2)
legend(1991,2017, legend = c("Ridership", "Centered MA, k=2",
                             "Centered MA, k=12"), 
       col = c("black", "brown" , "blue"), 
       lty = c(1, 1, 1), lwd =c(1, 2, 2), bty = "n")



#Trailing MA

#trailing MA for k=2
trailing.ma_2 <- rollmean(train.ts, k = 2, align = "right")
#For K = 6
trailing.ma_6 <- rollmean(train.ts, k = 6, align = "right")
#For K = 12
trailing.ma_12 <- rollmean(train.ts, k = 12, align = "right")

#Use the forecast() function to create a trailing MA forecast for each window width
#For K = 2
ma.t_2.pred <- forecast(trailing.ma_2, h = nValid, level = 0)
#For K = 6
ma.t_6.pred <- forecast(trailing.ma_6, h = nValid, level = 0)
#For K = 12
ma.t_12.pred <- forecast(trailing.ma_12, h = nValid, level = 0)
#Displaying for K=2
ma.t_2.pred
#Displaying for K=6
ma.t_12.pred
#Displaying for K=12
ma.t_12.pred


## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & TRAILING MA OF RESIDUALS ------

# Plot residuals of the predictions with trend and seasonality.
plot(training.lin.trend.pred$residuals, 
     xlab = "Time Period", ylab = "Residuals", ylim = c(-1300000, 1500000), bty = "l",
     xaxt = "n", xlim = c(1991, 2017), 
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2)

axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)))
lines(nValid - training.lin.trend.pred$mean, col = "black", lwd = 2, lty = 1)

# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2008.842, 2008.842), c(-1300000, 1300000))
lines(c(2013.343, 2013.343), c(-1300000, 1300000))

text(1998, 1300000, "Training")
text(2011, 1300000, "Validation")
text(2015, 1300000, "Future")

arrows(1991, 1300000, 2008.842, 1300000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.842, 1300000, 2013.343, 1300000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.343, 1300000, 2017, 1300000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## ------ TWO-LEVEL MODEL (regression model with linear trend and seasonality & trailing MA of residuals) ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
training.lin.trend.seas.res <- training.lin.trend.seas.pred$residuals
training.lin.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.lin.trend.seas.res <- rollmean(training.lin.trend.seas.res, k = 2, align = "right")
ma.trail.lin.trend.seas.res
# Create residuals forecast for validation period.
ma.trail.lin.trend.seas.res.pred <- forecast(ma.trail.lin.trend.seas.res, h = nValid, level = 0)
ma.trail.lin.trend.seas.res.pred
# Regression residuals in validation period.
training.lin.trend.seas.res.valid <- nValid - training.lin.trend.seas.pred$mean
training.lin.trend.seas.res.valid
# To develop real forecast for validation period, 
# combine regression forecast and trailing MA forecast for residuals.
valid.forecast.2level.linTS.ma <- training.lin.trend.seas.pred$mean + ma.trail.lin.trend.seas.res.pred$mean
valid.forecast.2level.linTS.ma

# Plot the predictions for trailing MA.
plot(train.ts, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", ylim = c(1300000, 2950000), bty = "l",
     xlim = c(1991, 2017), main = "Linear Trend and Seasonality & Trailing MA") 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(trailing.ma_2, col = "brown", lwd = 2)
lines(valid.forecast.2level.linTS.ma, col = "brown", lwd = 2, lty = 2)
lines(nValid)
legend(1991,2950000, legend = c("riderships", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")



# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2008.867, 2008.867), c(0, 2950000))
lines(c(2013.247, 2013.247), c(0, 2950000))

text(1998, 2950000, "Training")
text(2015, 2950000, "Validation")
text(2017, 2950000, "Future")

arrows(1991, 2950000, 2008.867, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.867, 2950000, 2013.247, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.247, 2950000, 2017, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



## ------ TWO-LEVEL MODEL (regression model with quadratic trend and seasonality & trailing MA of residuals) ------

# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
training.quad.trend.seas.res <- training.quad.trend.seas.pred$residuals
training.quad.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.quad.trend.seas.res <- rollmean(training.quad.trend.seas.res, k = 2, align = "right")
ma.trail.quad.trend.seas.res
# Create residuals forecast for validation period.
ma.trail.quad.trend.seas.res.pred <- forecast(ma.trail.quad.trend.seas.res, h = nValid, level = 0)
ma.trail.quad.trend.seas.res.pred
# Regression residuals in validation period.
training.quad.trend.seas.res.valid <- nValid - training.quad.trend.seas.pred$mean
training.quad.trend.seas.res.valid
# To develop real forecast for validation period, 
# combine regression forecast and trailing MA forecast for residuals.
valid.forecast.2level.quadTS.ma <- training.quad.trend.seas.pred$mean + ma.trail.quad.trend.seas.res.pred$mean
valid.forecast.2level.quadTS.ma

# Plot the predictions for trailing MA.
plot(train.ts, 
     xlab = "Time Period", ylab = "Ridership (No.of Number_of_Passengers)", ylim = c(1300000, 2950000), bty = "l",
     xlim = c(1991, 2017), main = "Quadratic Trend and Seasonality & Trailing MA") 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(trailing.ma_2, col = "brown", lwd = 2)
lines(valid.forecast.2level.quadTS.ma, col = "brown", lwd = 2, lty = 2)
lines(nValid)
legend(1991,2950000, legend = c("riderships", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.

lines(c(2008.881, 2008.881), c(0, 2950000))
lines(c(2013.344, 2013.344), c(0, 2950000))

text(1998, 2950000, "Training")
text(2011, 2950000, "Validation")
text(2015, 2950000, "Future")

arrows(1991, 2950000, 2008.881, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.881, 2950000, 2013.344, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.344, 2950000, 2017, 2950000, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, nValid), 3)
# Accuracy of two-level Model
# regression model with linear trend & seasonality and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.linTS.ma, nValid), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, nValid), 3)
# Accuracy of two-level Model
# regression model with quadratic trend & seasonality and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.quadTS.ma, nValid), 3)
# Accuracy of Naive forecast model (training data)



# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(training.lin.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(nValid - training.lin.trend.seas.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Validation Residuals")



## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & AUTOREGRESSIVE MODEL OF RESIDUALS ------

# Using Arima() function to fit AR(1) model for training residuals of regression model with linear trend
# The Arima model of order = c(1,0,0) gives an AR(1) model
lin.trend.seas.res.ar1 <- Arima(training.lin.trend.seas$residuals, order = c(1,0,0))
summary(lin.trend.seas.res.ar1)
z.stat <- (0.860 - 1)/0.036
p.val <- pnorm(z.stat)
p.val

# The Arima model of order = c(2,0,0) gives an AR(2) model
lin.trend.seas.res.ar2 <- Arima(training.lin.trend.seas$residuals, order = c(2,0,0))
summary(lin.trend.seas.res.ar2)
z.stat <- (0.6684 - 1)/0.0666
p.val <- pnorm(z.stat)
p.val

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(train.ts, training.lin.trend$fitted, 
                       training.lin.trend.seas$residuals, lin.trend.seas.res.ar1$fitted, 
                       lin.trend.seas.res.ar1$residuals, lin.trend.seas.res.ar2$fitted, 
                       lin.trend.seas.res.ar2$residuals)
names(train.df) <- c("train.data", "Regression.linearTS", "Regression.Residuals",
                     "AR(1).Model", "AR(1).Model.Residuals",
                     "AR(2).Model", "AR(2).Model.Residuals")
train.df


# Use forecast() function to make prediction of residuals in validation set.
lin.trend.seas.res.ar1.pred <- forecast(lin.trend.seas.res.ar1, h = nValid, level = 0)
lin.trend.seas.res.ar1.pred

lin.trend.seas.res.ar2.pred <- forecast(lin.trend.seas.res.ar2, h = nValid, level = 0)
lin.trend.seas.res.ar2.pred

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.2level.linTS.ar1.pred <- training.lin.trend.seas.pred$mean + lin.trend.seas.res.ar1.pred$mean
valid.2level.linTS.ar2.pred <- training.lin.trend.seas.pred$mean + lin.trend.seas.res.ar2.pred$mean

valid.df <- data.frame(valid.ts, training.lin.trend.seas.pred$mean,
                       valid.2level.linTS.ar2.pred)
names(valid.df) <- c("ridership.Valid", "Reg.LinTS.Forecast", "Combined.Forecast.AR(2)")
valid.df




## ------- QUADRATIC TREND AND SEASONALITY & AUTOREGRESSIVE MODEL OF RESIDUALS ------

# The Arima model of order = c(1,0,0) gives an AR(1) model
quad.trend.seas.res.ar1 <- Arima(training.quad.trend.seas$residuals, order = c(1,0,0))
summary(quad.trend.seas.res.ar1)
z.stat <- (0.7187 - 1)/0.0474
p.val <- pnorm(z.stat)
p.val

# The Arima model of order = c(2,0,0) gives an AR(2) model
quad.trend.seas.res.ar2 <- Arima(training.quad.trend.seas$residuals, order = c(2,0,0))
summary(quad.trend.seas.res.ar2)
z.stat <- (0.5947 - 1)/0.0675
p.val <- pnorm(z.stat)
p.val

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
quad_train.df <- data.frame(train.ts, training.quad.trend$fitted, 
                            training.quad.trend.seas$residuals, quad.trend.seas.res.ar1$fitted, 
                            quad.trend.seas.res.ar1$residuals)
names(quad_train.df) <- c("train.data", "Regression.quadTS", "Regression.Residuals",
                          "AR(1).Model", "AR(1).Model.Residuals")
quad_train.df


# Use forecast() function to make prediction of residuals in validation set.
quad.trend.seas.res.ar1.pred <- forecast(quad.trend.seas.res.ar1, h = nValid, level = 0)
quad.trend.seas.res.ar1.pred

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
quad_valid.two.level.pred <- training.quad.trend.seas.pred$mean + quad.trend.seas.res.ar1.pred$mean

quad_valid.df <- data.frame(valid.ts, training.quad.trend.seas.pred$mean, 
                            quad_valid.two.level.pred)
names(quad_valid.df) <- c("ridership.Valid", "Reg.QuadTS.Forecast", "Combined.Forecast.(AR1)")
quad_valid.df



##--------- NAIVE FORECAST--------
# Use naive() to make naive forecast (training.naive.pred) 
# for validation data.
training.naive.pred <- naive(train.ts, h = nValid)

# Plot predictions for naive forecast.
plot(training.naive.pred$mean, 
     xlab = "Time Period", ylab = "riderships (in 0000s)", ylim = c(1350000, 2900000), bty = "l",
     xlim = c(1991, 2017), main = "Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(training.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.



lines(c(2008.924, 2008.924), c(0, 2900000))
lines(c(2013.424, 2013.424), c(0, 2900000))

text(1998, 2900000, "Training")
text(2011, 2900000, "Validation")
text(2015, 2900000, "Future")

arrows(1991, 2900000, 2008.924, 2900000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.924, 2900000, 2013.424, 2900000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.424, 2900000, 2017, 2900000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



##--------- SEASONAL NAIVE FORECAST--------
# Use snaive() to make naive forecast (training.snaive.pred) 
# for validation data.
training.snaive.pred <- snaive(train.ts, h = nValid)

# Plot predictions for seasonal naive forecast.
plot(training.snaive.pred$mean, 
     xlab = "Time Period", ylab = "riderships (in 0000s)", ylim = c(1350000, 2900000), bty = "l",
     xlim = c(1991, 2017), main = "Seasonal Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)) )
lines(training.snaive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.



lines(c(2008.887, 2008.887), c(0, 2900000))
lines(c(2013.400, 2013.400), c(0, 2900000))

text(1998, 2900000, "Training")
text(2011, 2900000, "Validation")
text(2015, 2900000, "Future")

arrows(1991, 2900000, 2008.887, 2900000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2008.887, 2900000, 2013.400, 2900000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.400, 2900000, 2017, 2900000, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, valid.ts), 3)
# Accuracy of two-level regression model with linear trend & seasonality and 
# trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.linTS.ma, valid.ts), 3)
# Accuracy of regression model with linear trend (training data) and 
# AR(2) model for residuals (training data)
round(accuracy(valid.2level.linTS.ar2.pred, valid.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, valid.ts), 3)
# Accuracy of regression model with quadratic trend (training data) and 
# AR(1) model for residuals (training data)
round(accuracy(quad_valid.two.level.pred, valid.ts), 3)
# Accuracy of Seasonal Naive forecast model (training data)
round(accuracy(training.snaive.pred$mean, valid.ts), 3)



Acf(training.lin.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(lin.trend.seas.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")
Acf(lin.trend.seas.res.ar2$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")

Acf(training.quad.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(quad.trend.seas.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")



#### ENTIRE DATASET ####

## ---- LINEAR TREND AND SEASONALITY -------
# developing a regression model with linear trend ans seasonality (training data)
ridership.lin.trend.seas <- tslm(ridership.ts ~ trend + season)
summary(ridership.lin.trend.seas)
# forecasting in the validation period
ridership.lin.trend.seas.pred <- forecast(ridership.lin.trend.seas, 
                                          h = 12, level = 0)
ridership.lin.trend.seas.pred

# Use plot() function to create plot with linear trend and seasonality 
plot(ridership.ts, 
     xlab = "Time Period", ylab = "riderships", 
     ylim = c(1350000, 2900000), xlim = c(1991, 2025), 
     main = "Linear Trend and Seasonality Regression Model - Amtrek Ridership")
lines(ridership.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(ridership.lin.trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)


## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & TRAILING MA OF RESIDUALS ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
ridership.lin.trend.seas.res <- ridership.lin.trend.seas.pred$residuals
ridership.lin.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.lin.trend.seas.res <- rollmean(ridership.lin.trend.seas.res, k = 2, align = "right")
ma.trail.lin.trend.seas.res
# Create residuals forecast for future periods
ma.trail.lin.trend.seas.res.pred <- forecast(ma.trail.lin.trend.seas.res, h = 12, level = 0)
ma.trail.lin.trend.seas.res.pred 
# combine regression forecast and trailing MA forecast for residuals.
forecast.2level.linTS.ma <- ridership.lin.trend.seas.pred$mean + ma.trail.lin.trend.seas.res.pred$mean
forecast.2level.linTS.ma


# Use plot() function to create plot 
plot(ridership.ts, 
     xlab = "Time Period", ylab = "riderships", 
     ylim = c(1350000, 2900000), xlim = c(1991, 2025), 
     main = "Linear Trend and Seasonality Regression Model + Trailing MA (width = 2)")
lines(ridership.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(forecast.2level.linTS.ma, col = "blue", lwd = 2, lty = 2)


## ---- TWO-LEVEL MODEL (Linear T&S and AR(2) of Residuals) -------
# The Arima model of order = c(2,0,0) gives an AR(2) model
ridership.lin.trend.seas.res.ar2 <- Arima(ridership.lin.trend.seas.pred$residuals, order = c(2,0,0))
summary(ridership.lin.trend.seas.res.ar2)
z.stat <- (0.6422 - 1)/0.0592
p.val <- pnorm(z.stat)
p.val
# Use forecast() function to make prediction of residuals
ridership.lin.trend.seas.res.ar2.pred <- forecast(ridership.lin.trend.seas.res.ar2, h = 12, level = 0)
ridership.lin.trend.seas.res.ar2.pred
# two level model results
ridership.two.level.linTS.ar2.pred <- ridership.lin.trend.seas.pred$mean + ridership.lin.trend.seas.res.ar2.pred$mean
ridership.two.level.linTS.ar2.pred


# Use plot() function to create plot 
plot(ridership.ts, 
     xlab = "Time Period", ylab = "riderships", 
     ylim = c(1350000, 2900000), xlim = c(1991, 2025), 
     main = "Linear Trend and Seasonality Regression Model + Autoregressive (2)")
lines(ridership.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(ridership.two.level.linTS.ar2.pred, col = "blue", lwd = 2, lty = 2)



# Accuracy of regression model with linear trend and seasonality(entire data)
round(accuracy(ridership.lin.trend.seas.pred$fitted, ridership.ts), 3)
# Accuracy of regression model with linear trend and seasonality (entire data) and 
# trailing MA for residuals (entire data)
round(accuracy(ridership.lin.trend.seas.pred$fitted + 
                       ma.trail.lin.trend.seas.res.pred$fitted, ridership.ts), 3)
# Accuracy of regression model with linear trend and seasonality (entire data) and 
# AR(2) model for residuals (entire data)
round(accuracy(ridership.lin.trend.seas.pred$fitted + 
                       ridership.lin.trend.seas.res.ar2.pred$fitted, ridership.ts), 3)
# Accuracy of seasonal naive forecast (baseline model)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)



## TEST PREDICTABILITY

# Use Arima() function to fit AR(1) model for ridership
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
ridership.data.ar1<- Arima(ridership.ts, order = c(1,0,0))
summary(ridership.data.ar1)

# The ARIMA model of order = c(2,0,0) gives an AR(2) model.
ridership.data.ar2<- Arima(ridership.ts, order = c(2,0,0))
summary(ridership.data.ar2)

# Create first difference of ClosePrice data using diff() function.
diff.ridership.data <- diff(ridership.ts, lag = 1)
diff.ridership.data

Acf(diff.ridership.data, lag.max = 12, 
    main = "Autocorrelation for First Differencing - Amtrek Ridership")



## 
# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
pass.ts <- ts(ridership.data$Number_of_Passengers, 
              start = c(1991,1), end = c(2013, 5), freq = 12)

pass.ts
head(pass.ts)
tail(pass.ts)

## 
# Plot the time series data. 
plot(pass.ts, 
     xlab = "Time Period", ylab = "Amtrek Ridership(in 1350000)", ylim = c(1350000, 2900000), bty = "l",
     xaxt = "n", xlim = c(1991, 2017), main = "Amtrek Ridership", lwd = 3, col="blue") 
axis(1, at = seq(1991, 2017, 1), labels = format(seq(1991, 2017, 1)))



# The plot includes original data, trend, seasonal, and reminder (level and noise component).
pass.stl <- stl(pass.ts, s.window = "periodic")
autoplot(pass.stl, main = "Amtrek Ridership Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(pass.ts, lag.max = 12, main = "Autocorrelation for Amtrek Ridership")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#ACF for Training and validation periods 
Acf(train.ts, lag.max = 12, main = "Autocorrelation for Amtrek ridership Training Data Set")
Acf(valid.ts, lag.max = 12, main = "Autocorrelation for Amtrek ridership Validation Data Set")

pass.ar1<- Arima(pass.ts, order = c(1,0,0))
summary(pass.ar1)


z.statistic <- (0.8553  -1)/0.0322

#P-Value for z.statistic
p.value <-pnorm(z.statistic)
p.value 

# Using the first referencing (lag1) of the historical data and Acf() function

diff.pass <- diff(pass.ts, lag = 1)

#Acf() function with maximum of 8 lags

Acf(diff.pass, lag.max = 8, 
    main = "Autocorrelation Amtrek Ridership for First Differencing (lag1)")


# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# nTrain and nValid, respectively.
nValid <- round(length(pass.ts)*0.8, 0)
nTrain <- length(pass.ts) - nValid
train.ts <- window(pass.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(pass.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

head(train.ts)
tail(train.ts)

head(valid.ts)
tail(valid.ts)
valid.ts
