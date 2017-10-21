#----------------------------------------------------------------------------
  getwd()
sa= read.csv("sales.csv")
sa
 
  
acf(sa)
pacf (sa)
?acf
?arima
  #---- Below command are to figure out best ARIMA model
  
arima(sa,order= c(1,1,1))
  
arima(sa,order= c(2,1,2))
  
arima(sa,order= c(1,1,0))
  
arima(sa,order= c(0,1,1))
  
  
 # --- For checking Seasonal ARIMA
  
  arima(sa,seasonal = list(order = c(1,1,1), period = NA))
  
  arima(sa,seasonal = list(order = c(2,1,2), period = NA))
  
  arima(sa,seasonal = list(order = c(2,2,2), period = NA))
  
  arima(sa,seasonal = list(order = c(2,2,2), period = NA))
  
  arima(sa,seasonal = list(order = c(1,2,1), period = NA))
  
  arima(sa,seasonal = list(order = c(2,0,2), period = NA))
  
  arima(sa,seasonal = list(order = c(1,0,1), period = NA))
  
  
  #---Based on value of AIC Seasonal ARIMA (1,2,1) is better than all other models hence Final ARIMA model is
  
  arima(sa, order = c(1, 1, 1), seasonal = list(order = c(1, 2, 1), period = 4))
  
 # ----The value of period can be select based on seasonality behavior present in data. In our case best results are coming for period -4. Command For The  Box-Jenkins model building- Model Validation
  
  
fit<-arima(sa, order = c(1, 1, 1), seasonal = list(order = c(1, 2, 1), period = 4))
tsdiag(fit)
  
#  -- Command for Model Validation - Box Test
Box.test(fit$residuals,lag=5)
  
  
#  -- Model Forecasting command
  
predict(fit,n.ahead=8)
  
  
  
  
  
  
  
  
  
  
  
  
  
  