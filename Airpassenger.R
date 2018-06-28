#Time Series Using ARIMA Model
#Get the AirPassenger Data
data(AirPassengers)
View(AirPassengers)

#Class of data
class(AirPassengers)

#Data View
start(AirPassengers)
end(AirPassengers)

frequency(AirPassengers)
summary(AirPassengers)


#Plot the graph
plot(AirPassengers)

#fit a line to get the mean of the time series
abline(reg=lm(AirPassengers~time(AirPassengers)))

#To get the cycle across years
cycle(AirPassengers)

#Aggregate the cycles and display the year on year trend
plot(aggregate(AirPassengers, FUN = mean))

#boxplot of seasonal effect based on month
boxplot(AirPassengers ~ cycle(AirPassengers))

#trend of the data per year
plot(AirPassengers)

#to get the stationary variance for the cycle
plot(diff(log(AirPassengers)))

#ARIMA MODEL - Auto regressive Integration Moving Average - to see the past value to predict the future based on different interval
install.packages("tseries")
library(tseries)

adf.test(diff(log(AirPassengers)), alternative = c("stationery", "explosive"))


#AR I MA
#P  d  q -> get the value from graph
#auto correlation function
acf(AirPassengers)

# get the value of q - Moving Average -> take the value of the line before the previous line which is inverted - 0.1
acf(diff((log(AirPassengers)))) 
# get the value of p - Auto regfressive -> Partial Autocorelation function
pacf(diff((log(AirPassengers)))) 

#Fitting the ARIMA Model to the data
fit <- arima(log(AirPassengers), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

#Predicting the value for the next 10 years with the log value
pred <- predict(fit, n.ahead = 10*12)

#Converting to normal value
pred1 <- 2.718^pred$pred

#Plotting the predicted value in the graph
ts.plot(AirPassengers, 2.718^pred$pred, log = "y", lty = c(1,3))

#testing the Model
datawide <- ts(AirPassengers, frequency = 12, start = c(1949,1), end = c(1959,12))

fit <- arima(log(datawide), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
pred1 <- 2.718^pred$pred

data1 <- head(pred1, 12)

#Getting the predicted and the original value
predicted_1960 <- round(data1, digits = 0)
original_1960 <- tail(AirPassengers,12)

#Plotting the predicted value
ts.plot(AirPassengers, 2.718^pred$pred, log = "y", lty = c(1,3))
