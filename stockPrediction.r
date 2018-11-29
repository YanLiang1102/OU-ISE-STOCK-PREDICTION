#predict the stock price with ARIMA:

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

getSymbols('AMZN', from='2013-01-01', to='2018-11-20')
getSymbols('GOOGL', from='2013-01-01', to='2018-11-20')
getSymbols('AAPL', from='2013-01-01', to='2018-11-20')
getSymbols('FB', from='2013-01-01', to='2018-11-20')

#without any update and this is the raw data update
stock_prices=AMZN[,4]
plot(stock_prices,title="amazon stock")

stock=log(stock_prices)
#doing lag1 differencing model to make the series to be stationary.
stock=diff(log(stock_prices),lag=3)
stock=stock[!is.na(stock)]
plot(stock,type='l',main='log amazon with lag1')

#choose 0.8 as the training and 0.2 as the testing
breakpoint = floor(nrow(stock)*0.8)

#using the pacf to determine the p value for ar model and using the acf to determine the q value for ma model
par(mfrow = c(1,1))

acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
#based on the acf graph I will chose the q=4
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)  
#based pacf graph I will choose the p=12

# Initialzing an xts object for Actual log returns
stock[breakpoint+1,]
#base on the return for the test dataset 2017-09-20 -0.01385732
Actual_series = xts(0,as.Date("2017-09-19","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())
b<-breakpoint 
stock_train = stock[1:b, ]
fit = arima(stock_train, order = c(12, 3, 4),include.mean=FALSE)
summary(fit)

fit = arima(stock_train, order = c(65, 0, 0),include.mean=FALSE)
summary(fit)

for (b in breakpoint:(nrow(stock)-1)) {
  print(b)
  stock_train = stock[1:b, ]
  stock_test = stock[(b+1):nrow(stock), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit = arima(stock_train, order = c(12,3, 4),include.mean=FALSE)
  #summary(fit)
  
  # plotting a acf plot of the residuals
  #acf(fit$residuals,main="Residuals plot")
  
  # Forecasting the log returns
  #arima.forecast = forecast.Arima(fit, h = 1,level=99)
  arima.forecast = forecast(fit, h =1,level=99)
  #summary(arima.forecast)
  
  # plotting the forecast
  par(mfrow=c(1,1))
  #plot(arima.forecast, main = "ARIMA Forecast")
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  # Creating a series of actual returns for the forecasted period
  Actual_return = stock[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  #print(stock_prices[(b+1),])
  #print(stock_prices[(b+2),]) 
}

# Adjust the length of the Actual return series, since when construct the xts it starts with a dummy value 0
Actual_series = Actual_series[-4]

# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series,index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))
plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')


# Create a table for the accuracy of the forecast
#comparsion = merge(Actual_series,forecasted_series)
#comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
#comparsion$Accuracy=(comparsion$Actual_series>1&&comparsion$Forecasted>1)||(comparsion$Actual_series1&&comparsion$Forecasted>1)
#Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
#print(Accuracy_percentage)

#51.85 might be not a good result.
sqrt(mean((Actual_series-forecasted_series)**2))
#the RMSE is 0.02025829 error to predict the log diff.

#LSTM for the stock prediction
devtools::install_github("rstudio/keras")
library(keras)
install_keras()

#using random walk and monte carlo simulation
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

getSymbols("AMZN",from="2013-01-01",to="2018-11-20")
AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')

AMZN%>%Ad()%>%chartSeries()
AMZN%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018')

#see the relationship of amazon with other big tech companies:
library(PerformanceAnalytics)
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)

#get the mean of logreturn as the return and variance as the deviation to say draw the retunn vs risk graph for these four
#stocks to see which one we should buy.
meandata<-cbind(mean(data$AMZN.Close,na.rm=TRUE),mean(data$GOOGL.Close,na.rm=TRUE),mean(data$AAPL.Close,na.rm=TRUE),mean(data$FB.Close,na.rm=TRUE))
vardata<-cbind(var(data$AMZN.Close,na.rm=TRUE),var(data$GOOGL.Close,na.rm=TRUE),var(data$AAPL.Close,na.rm=TRUE),var(data$FB.Close,na.rm=TRUE))

#using random walk to simulate the change notice that set.seed() is not set, so each time the simulation is different.
mu<-mean(diff(log(stock_prices)),na.rm=TRUE)
sig<-sd(diff(log(stock_prices)),na.rm=TRUE)

price<-rep(NA,nrow(stock_prices))

price[1]=1512.29 #this is the price of 2015/01/01
for(i in 2:nrow(stock_prices)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:nrow(stock_prices))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)

random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Amazon (AMZN) price simulation for 4 years")+theme_bw()

N<-500
mc_matrix<-matrix(nrow=nrow(stock),ncol=N)
mc_matrix[1,1]<-as.numeric(stock_prices[nrow(stock_prices)])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(stock_prices[nrow(stock_prices)])
  for(i in 2:nrow(stock)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
name<-str_c("Sim ",seq(1,N))
name<-c("Day",name)

final_mat<-cbind(1:nrow(stock),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

final_mat%>%gather("Simulation","Price",2:501)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="(AMZN): 500 Monte Carlo Simulations for 4 Years")+theme_bw()



















