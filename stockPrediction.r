####################################
############
###########start of yan's code.
####################################
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

getSymbols('AMZN', from='2013-01-01', to='2018-11-20')
getSymbols('GOOGL', from='2013-01-01', to='2018-11-20')
getSymbols('AAPL', from='2013-01-01', to='2018-11-20')
getSymbols('FB', from='2013-01-01', to='2018-11-20')
getSymbols('T',from='2013-01-01',to='2018-11-20')
getSymbols('TMUS',from='2013-01-01',to='2018-11-20')
getSymbols('S',from='2013-01-01',to='2018-11-20')
getSymbols('VZ',from='2013-01-01',to='2018-11-20')
getSymbols('AAL',from='2013-01-01',to='2018-11-20')
getSymbols('DAL',from='2013-01-01',to='2018-11-20')
getSymbols('LUV',from='2013-01-01',to='2018-11-20')
getSymbols('ALK',from='2013-01-01',to='2018-11-20')



#without any update and this is the raw data update
stock_prices=AMZN[,4]
stock_prices_goo=GOOGL[,4]
stock_prices_aap=AAPL[,4]
stock_prices_fb=FB[,4]
plot(stock_prices,title="amazon stock")
att=T[,4]
tmobile=TMUS[,4]
sprint=S[,4]
verizon=VZ[,4]
american=AAL[,4]
delta=DAL[,4]
southwest<-LUV[,4]
alaska<-ALK[,4]


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
AMZN%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018',name="amazon 2018")

T%>%Ad()%>%chartSeries()
T%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018',name="att 2018")

AAL%>%Ad()%>%chartSeries()
AAL%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018',name="american airlines 2018")

#see the relationship of amazon with other big tech companies:
library(PerformanceAnalytics)
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)

#for att data
data1<-cbind(diff(log(Cl(att))),diff(log(Cl(sprint))),diff(log(Cl(tmobile))),diff(log(Cl(verizon))))
chart.Correlation(data1)

#for american airline
data2<-cbind(diff(log(Cl(american))),diff(log(Cl(southwest))),diff(log(Cl(alaska))),diff(log(Cl(delta))))
chart.Correlation(data2)

#get the mean of logreturn as the return and variance as the deviation to say draw the retunn vs risk graph for these four
#stocks to see which one we should buy.
meandata<-cbind(mean(data$AMZN.Close,na.rm=TRUE),mean(data$GOOGL.Close,na.rm=TRUE),mean(data$AAPL.Close,na.rm=TRUE),mean(data$FB.Close,na.rm=TRUE))
vardata<-cbind(var(data$AMZN.Close,na.rm=TRUE),var(data$GOOGL.Close,na.rm=TRUE),var(data$AAPL.Close,na.rm=TRUE),var(data$FB.Close,na.rm=TRUE))

#using random walk to simulate the change notice that set.seed() is not set, so each time the simulation is different.
mu<-mean(diff(log(stock_prices[1:1200])),na.rm=TRUE)
sig<-sd(diff(log(stock_prices[1:1200])),na.rm=TRUE)

# remainingrow<-nrow(stock_prices)-1200
# price<-rep(NA,remainingrow)
# 
# price[1]=980.85 # this is the stock price on date 2017-10-05
# for(i in 2:remainingrow){
#   price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
# }
# 
# random_data<-cbind(price,1:remainingrow)
# colnames(random_data)<-c("Price","Day")
# random_data<-as.data.frame(random_data)
# 
# random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Amazon (AMZN) price simulation from 2017-10-05 to 2018-11-20")+theme_bw()
#doing monto_calo for Amazon stock
N<-50
mc_matrix<-matrix(nrow=remainingrow,ncol=N)
mc_matrix[1,1]<-as.numeric(stock_prices[1200])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(stock_prices[1200])
  for(i in 2:remainingrow){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
name<-str_c(seq(1,N))
name<-c("Day",name)

final_mat<-cbind(1:remainingrow,mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

final_avg<-rep(0,remainingrow)
for(row in 1:remainingrow)
{
  for(col in colnames(final_mat))
  {
    if(col=="Day")
    {
      next
    }
    #print(col)
    #this is the way how to access the tibble object kind of weird.
    y<-final_mat[toString(col)][[1]]
    #print(row)
    final_avg[row]<-final_avg[row]+y[row]
  }
}
final_avg<-final_avg/N
par(new=TRUE)
final_mat%>%gather("Simulation","Price",2:N+1)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="Amazon (AMZN) price simulation from 2017-10-05 to 2018-11-20")+theme_bw()
par(new=TRUE)
plot(stock_prices[1201:nrow(stock_prices),],col="green")
par(new=TRUE)
plot(final_avg,col="red")

#doing monte carlo for att stock
mu<-mean(diff(log(att[1:1200])),na.rm=TRUE)
sig<-sd(diff(log(att[1:1200])),na.rm=TRUE)
N<-50
mc_matrix<-matrix(nrow=remainingrow,ncol=N)
mc_matrix[1,1]<-as.numeric(att[1200])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(att[1200])
  for(i in 2:remainingrow){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
name<-str_c(seq(1,N))
name<-c("Day",name)

final_mat<-cbind(1:remainingrow,mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

final_avg<-rep(0,remainingrow)
for(row in 1:remainingrow)
{
  for(col in colnames(final_mat))
  {
    if(col=="Day")
    {
      next
    }
    #print(col)
    #this is the way how to access the tibble object kind of weird.
    y<-final_mat[toString(col)][[1]]
    #print(row)
    final_avg[row]<-final_avg[row]+y[row]
  }
}
final_avg<-final_avg/N
par(new=TRUE)
final_mat%>%gather("Simulation","Price",2:N+1)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="att price simulation from 2017-10-05 to 2018-11-20")+theme_bw()
par(new=TRUE)
plot(att[1201:nrow(att),],col="green")
par(new=TRUE)
plot(final_avg,col="red")


#doing monte carlo for airline stock
mu<-mean(diff(log(airline[1:1200])),na.rm=TRUE)
sig<-sd(diff(log(airline[1:1200])),na.rm=TRUE)
N<-50
mc_matrix<-matrix(nrow=remainingrow,ncol=N)
mc_matrix[1,1]<-as.numeric(airline[1200])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(airline[1200])
  for(i in 2:remainingrow){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
name<-str_c(seq(1,N))
name<-c("Day",name)

final_mat<-cbind(1:remainingrow,mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

final_avg<-rep(0,remainingrow)
for(row in 1:remainingrow)
{
  for(col in colnames(final_mat))
  {
    if(col=="Day")
    {
      next
    }
    #print(col)
    #this is the way how to access the tibble object kind of weird.
    y<-final_mat[toString(col)][[1]]
    #print(row)
    final_avg[row]<-final_avg[row]+y[row]
  }
}
final_avg<-final_avg/N
par(new=TRUE)
final_mat%>%gather("Simulation","Price",2:N+1)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="att price simulation from 2017-10-05 to 2018-11-20")+theme_bw()
par(new=TRUE)
plot(att[1201:nrow(att),],col="green")
par(new=TRUE)
plot(final_avg,col="red")




#building the model of knn to determine if the stock will go up and donw
library(class)
library(dplyr)
library(lubridate)
set.seed(100)

#from the previous analysis we can see that Amazon and Google and Facebook are highly correlated, so will using 
#that price to predict too.
fm<-as.data.frame(stock_prices)
fm$GOOGL.Close<-stock_prices_goo
fm$AAPL.Close<-stock_prices_aap

#airline data
fm.airline<-as.data.frame(american)
fm.airline$southwest<-southwest
fm.airline$alaska<-alaska
fm.airline$delta<-delta
fm.airline$Increase<-rep(TRUE,nrow(stock_prices))

#for att data
fm.att<-as.data.frame(att)
fm.att$verizon<-verizon


#don't need this for the feature engineering.
fm$FB.Close<-stock_prices_fb
getSymbols('AMZN', from='2012-11-11', to='2013-01-01')
getSymbols("T",from='2012-11-11', to='2013-01-01')
getSymbols("AAL",from='2012-11-11', to='2013-01-01')
getSymbols("DAL",from='2012-11-11', to='2013-01-01')
getSymbols("LUV",from='2012-11-11', to='2013-01-01')
getSymbols("ALK",from='2012-11-11', to='2013-01-01')
getSymbols("VZ",from='2012-11-11', to='2013-01-01')
#find the start price is 2012-12-31 is 250.87 this is needed to determine if the first record is increasing or
#decreasing
#amazon start
start<-250.87
#att start
start.att<-33.71
start.american<-13.50

#construct the increase label for amazon data
for(i in 1:nrow(stock_prices))
{
  fm$Increase[i]<-(fm$AMZN.Close[i]-start)>=0
  start=fm$AMZN.Close[i]
}
#construct the increase label for airline data
for(i in 1:nrow(stock_prices))
{
  fm.airline$Increase[i]<-(fm.airline$AAL.Close[i]-start.american)>=0
  start.american=fm.airline$AAL.Close[i]
}
#construct the increase label for att data
for(i in 1:nrow(stock_prices))
{
  fm.att$Increase[i]<-(fm.att$T.Close[i]-start.att)>=0
  start.att=fm.airline$AAL.Close[i]
}
#for Amazon
fm.train<-fm[1:1200,]
fm.test<-fm[1201,nrow(stock_prices)]
#for american airline
fm.airline.train<-fm.airline[1:1200,]
fm.airline.test<-fm.airline[1201,nrow(stock_prices)]
#for att data
fm.att.train<-fm.att[1:1200,]
fm.att.test<-fm.att[1201,nrow(stock_prices)]


#construct the predictors for amazon
predictors <- cbind(lag(fm$AAPL.Close, default = 76.02), lag(fm$GOOGL.Close, default = 354.0440), lag(fm$AMZN.Close, default = 250.87))
prediction <- knn(predictors[1:1200, ], predictors[1200:nrow(stock_prices), ], fm$Increase[1:1200], k = 9)
table(prediction, fm$Increase[1200:nrow(stock_prices)])
mean(prediction == fm$Increase[1200:nrow(stock_prices)])
#this is the output for k=10 and beat  random so based on how much money you have a litle bit over 50% actually it is good
0.584507
accuracy <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction <- knn(predictors[1:1200, ], predictors[1200:nrow(stock_prices), ],
                    fm$Increase[1:1200], k = x)
  accuracy[x] <- mean(prediction == fm$Increase[1200:nrow(stock_prices)])
}

plot(k, accuracy, type = 'b')
#from here we can see the best accuracy can be achieved when k=9 and the accuracy is calculated above we saw the accuracy is 58.45%

predictors.airline<-cbind(lag(fm.airline$southwest, default = 10.24), lag(fm.airline$delta, default = 11.87), lag(fm.airline$AAL.Close, default = 13.50))
prediction.airline<-knn(predictors.airline[1:1200, ], predictors.airline[1200:nrow(stock_prices), ], fm.airline$Increase[1:1200], k = 3)
table(prediction.airline, fm.airline$Increase[1200:nrow(stock_prices)])
mean(prediction.airline == fm.airline$Increase[1200:nrow(stock_prices)])
0.5880
accuracy.airline <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction <- knn(predictors.airline[1:1200, ], predictors.airline[1200:nrow(stock_prices), ],
                    fm.airline$Increase[1:1200], k = x)
  accuracy.airline[x] <- mean(prediction == fm.airline$Increase[1200:nrow(stock_prices)])
}
plot(k, accuracy.airline, type = 'b',main="knn k value for airline")

#now constaruc the knn for att data
predictors.att<-cbind(lag(fm.att$verizon, default = 43.27), lag(fm.att$T.Close, default = 33.7))
prediction.att<-knn(predictors.att[1:1200, ], predictors.att[1200:nrow(stock_prices), ], fm.att$Increase[1:1200], k = 12)
table(prediction.att, fm.att$Increase[1200:nrow(stock_prices)])
mean(prediction.att == fm.att$Increase[1200:nrow(stock_prices)])
accuracy.att <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction.att <- knn(predictors.att[1:1200, ], predictors.att[1200:nrow(stock_prices), ],
                    fm.att$Increase[1:1200], k = x)
  accuracy.att[x] <- mean(prediction.att == fm.att$Increase[1200:nrow(stock_prices)])
}
plot(k, accuracy.att, type = 'b',main="knn k value for att")


















