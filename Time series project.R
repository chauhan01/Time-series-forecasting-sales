#-----------------------------------------Time Series Forecasting---------------------------------------#



#Analytical Problem:  To build an Auto Regressive Moving Average (ARIMA) model which predicts 36 data points ahead for the dependent variable: Sales 

#----------------------------------Preparing the environment--------------------------------------------#

list.of.packages <- c("forecast", "ggplot2","MASS","caTools","sqldf","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast)
library(tseries)
library(stats)



#-----------------------------Setting the working directory-------------------------------------------#

Path<-"C:/Users/Ravi/Desktop/shubham data/R/Time series"
setwd(Path)
getwd()

data<-read.csv("1sales.csv",header = TRUE)
TSdata=data#To create a backup of original data


#---------------------------------Exploring the data-------------------------------------------------------#
head(TSdata)
tail(TSdata)
dim(TSdata)#We have 144 time series points (at a date level) and 2 vars(Date, Sales)
str(TSdata)
summary(TSdata)
colSums(is.na(TSdata))
names(TSdata)[c(1:2)]=c("Date","Sales")


#---------------------Transformation of the date data into time series------------------------------------#

TSdata=ts(TSdata[,2],start=c(2003,1),frequency=12)
start(TSdata)
end(TSdata)
frequency(TSdata)
str(TSdata)


#ploting the sales
library(pastecs)
options(scipen = 100)
par(mfrow = c(1,1))
plot(TSdata, ylim=c(1,110000), ylab = "sales", xlab = "Year", main = "sales between 2003 and 2017", col = "grey")
abline(reg = lm(TSdata~time(TSdata)))
plot(aggregate(TSdata,FUN=mean))



#--------------->Differencing the data to remove trend and drift

plot(log10(TSdata), ylab = "log(sales)", xlab = "Year", main = "log sales between 2003 and 2017", col = "grey")

#ploting difference

plot(diff(TSdata, differences = 1), ylab = "diff(sales)", xlab = "Year", main = "diff of sales between 2003 and 2017", col = "grey")


#ploting log+difference

plot(diff(log10(TSdata), differences = 2), ylab = "diffl(log(sales))", xlab = "Year", main = "diff + log of sales between 2003 and 2017", col = "grey")


LDTSdata = diff(log10(TSdata), differences = 2) 



#----------------->Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)

require(forecast)
adf.test(LDTSdata,alternative="stationary")

#Since, the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary 


par(mfrow = c(1,2))
acf(diff(log10(TSdata), main = "ACF plot"))

pacf(diff(log10(TSdata)), main = "PACF plot")


#Running the ARIMA model
ARIMAfit <- arima((log10(TSdata)), c(0,2,0))
summary(ARIMAfit)

#Running the ARIMA model-R, gives the best model fit 
require(forecast)
ARIMAFit1=auto.arima(log10(TSdata),approximation=TRUE,trace=TRUE)


summary(ARIMAFit1)


ARIMAFit1$residuals

#predicting future values

pred <- predict(ARIMAFit1, n.ahead = 36)

#ploting the observed data and forcasted data


#ts.plot(TSdata,10^(pred$pred), log = "y")

par(mfrow = c(1,1))
plot(TSdata,type = "l",xlim = c(2003,2020), ylim = c(1, 110000) ,xlab="Year",ylab="Sales")
lines(10^(pred$pred),col="red")


#plotting the +-2 standard error to range of expected error
plot(TSdata,type="l",xlim=c(2004,2020),ylim=c(1,110000),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue")
lines(10^(pred$pred-2*pred$se),col="black")
## then forecast the result
pred = predict(ARIMAFit1, n.ahead = 36)
write.csv(pred,"predict.csv")

## taking exponential since we had used log earlier.
normal_result=10^pred$pred
write.csv(normal_result,"finalpredict.csv", row.names = FALSE)
plot(normal_result)
