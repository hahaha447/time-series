library(dplyr)
library(forecast)
library(xts)
library(EMD)
library(ggplot2)
library(jsonlite)
library(tseries)
setwd("C:/Users/Arche/graduate/R working directory/5505/timeseries")


## Time series analysis

# Abstract: In this note, I mainly used the ARIMA model and EMD decomposition to predict 
# the stock price of American Electric Power. And I used crawler technology to download 
# the stock price that I want to analyze from the web.

Get_price <- function(symbol="MSFT",f="TIME_SERIES_DAILY"){
  url <- paste("https://www.alphavantage.co/query?function=",f,"&symbol=",symbol,"&outputsize=full&apikey=",myapikey,sep = "")
  data <- fromJSON(url)
  if(f=="TIME_SERIES_DAILY"){
    temp_data <- data$`Time Series (Daily)`
  } else if(f=="TIME_SERIES_WEEKLY"){
    temp_data <- data$`Weekly Time Series`
  } else if(f=="TIME_SERIES_MONTHLY"){
    temp_data <- data$`Monthly Time Series`
  }
  day <- names(temp_data)
  close_price <- array()
  for (i in 1:length(day)) {
    d <- day[i]
    close_price[i] <- as.numeric(temp_data[[d]]$'4. close')
  }
  as.numeric() 
  mydata <- data.frame(symbol,day,close_price)
  return(mydata)
}



AEP <- Get_price("AEP","TIME_SERIES_WEEKLY")[1:528,]
AEP$day <- as.Date(AEP$day,format ="%Y-%m-%d")
Timeseries <- xts(x = AEP$close_price, order.by = AEP$day)
head(Timeseries)




mod_arima <- arima(Timeseries,order = c(8,1,5))
perdict_arima <- forecast(mod_arima,h=12)
autoplot(perdict_arima)



emd_predict <- function(X=AEP){
  a <- emd(X$close_price)
  emd_price <- as.data.frame(a$imf)
  emd_price$residue <- a$residue
  n_imfs <- length(names(emd_price))
  emd_price$day <- as.Date(X$day,format ="%Y-%m-%d")
  p <- list()
  fft_plot <- list()
  predict_price <- rep(0,12)
  period <- array()
  for (i in 1:n_imfs) {
    temp_series <- xts(x = emd_price[i], order.by = X$day)
    mod_emd_arima <- auto.arima(temp_series)
    predict_emd_arima <- forecast(mod_emd_arima,h=12)
    p[[i]] <- autoplot(predict_emd_arima)
    predict_price <- predict_price+predict_emd_arima$mean
    
    ######
    temp_series <- as.numeric(unlist(emd_price[i]))
    N=length(temp_series)
    dt=1
    f  = (0:(N-1))/N * (1/dt);
    freq<-fft(temp_series,inverse = T)
    period[[i]] <- N/(which.max(abs(freq)[1:N/2])-1)
    temp_data <- data.frame(x=f[1:N/2],y=abs(freq)[1:N/2])
    fft_plot[[i]] <- ggplot(data = temp_data,aes(x=x,y=y))+
      geom_line()
    ######
  }
  return(list(p,predict_price,period,fft_plot))
}



result <- emd_predict(AEP)
result[[1]]

predict_price <- result[[2]]
predict_price_xts <- xts(predict_price,
                         order.by =as.Date(c("2019-2-15","2019-2-22","2019-3-1","2019-3-8",
                                             "2019-3-15","2019-3-22","2019-3-29","2019-4-5",
                                             "2019-4-12","2019-4-19","2019-4-26","2019-5-3"),
                                           format ="%Y-%m-%d"))

df_history <- fortify(Timeseries)
df_predict <- fortify(predict_price_xts)
ggplot()+
  geom_line(aes(x=Index,y=Timeseries),data=df_history, col="gray2")+
  geom_line(aes(x=Index,y=predict_price_xts),data=df_predict,col="red")
df_predict


result[[4]]
result[[3]]



