# Ch2 Linear Time Series Analysis: seasonal effect and long memory
# Last Updated 2022.10.26 by S. F. Huang

#===========================
# (1) Seasonal differencing
#     (1-B)(1-B^4)yt = at
#===========================
  T     = 100
  burn  = 100
  sigma = 1
  at    = rnorm(T+burn,0,sigma)
  y0     = matrix(0, nrow=T, ncol=1)
  y0[1]  = at[1]
  y0[2]  = y0[1] + at[2]
  y0[3]  = y0[2] + at[3]
  y0[4]  = y0[3] + at[4]
  y0[5]  = y0[4] + y0[1] + at[5]
  for (t in 6:(T+burn)){
       y0[t] = y0[t-1] + y0[t-4] - y0[t-5] +  at[t]
  }
  y = NULL
  y = y0[(burn+1):(burn+T)]
  
  
  par(mfrow = c(3,1))
  plot(1:T,y, type='l',main='',xlab='')
  acf(y,main='')
  pacf(y,main='')

  adfTest(y, type="ct")
  
## (1-B)yt
  par(mfrow = c(3,1))
  plot(diff(y), type='l',main='',xlab='')
  acf(diff(y),main='')
  pacf(diff(y),main='')

## (1-B^4)yt
  par(mfrow = c(3,1))
  plot(diff(y, lag = 4), type='l',main='',xlab='')
  acf(diff(y, lag = 4),main='')
  pacf(diff(y, lag = 4),main='')

  adfTest(diff(y, lag = 4), type="c")
  
  m0 = arima(y, order = c(0,0,0), seasonal = list(order = c(0,1,0), period = 4))
  m0
  tsdiag(m0)
  
## (1-B)(1-B^4)yt
  m1 = arima(y, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 4))
  m1
  
  par(mfrow = c(3,1))
  plot(m1$residuals, type='l',main='',xlab='')
  acf(m1$residuals,main='')
  pacf(m1$residuals,main='')

  tsdiag(m1)


#==============================================
# (2) Real data: Example 2.3 Johnson & Johnson
#==============================================
  
  da = as.matrix(read.table("q-JNJ1960-80.txt", header=T),ncol=1)
  
  d1 = log(da)
  T  = length(d1)

  par(mfrow = c(3,1))
  plot(1:T, d1, type='l',ylab='log(earning)')
  abline(a = mean(d1), b = 0, col = 2)
  acf(d1, main='')
  pacf(d1, main='')

  Box.test(d1, lag=log(T), type='Ljung')

  
## Interaction between lag-1 and lag-s serial dependence 
  wt = d1
  for (t in 2:5){
    wt[t] = d1[t]-d1[t-1]
  }
  for (t in 6:T){
    wt[t] = d1[t]-d1[t-1]-d1[t-4]+d1[t-5]
  }
  
  par(mfrow = c(3,1))
  plot(1:T, wt, type='l',main='',xlab='')
  abline(a = mean(wt), b = 0, col = 2)
  acf(wt, main='')
  pacf(wt, main='')
  
  
## Seasonal ARMA model
  
  m1 = arima(d1, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 4))
  m1
  
  par(mfrow = c(3,1))
  plot(1:T, m1$residual, type='l',main='',xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(m1$residual, main='')
  pacf(m1$residual, main='')
  
  tsdiag(m1, gof = 36)
  
  
## Multiplicative seasonal ARMA model

  m2 = arima(d1, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 4),include.mean = F)
  m2

  par(mfrow = c(3,1))
  plot(1:T, m2$residual, type='l',main='',xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(m2$residual, main='')
  pacf(m2$residual, main='')


  tsdiag(m2, gof = 36)

  
  #=============================
  # (3) Real data: Example 2.4
  #=============================
  
  setwd("C:/Users/user/Dropbox/開課/財務方面/2022 財務時間序列/lectures") # set working directory
  
  da = read.table("m-deciles08.txt", header=T)
  d1 = da[,2]
  T  = length(d1)
  
  par(mfrow = c(3,1))
  plot(1:T, d1, type='l',main='',xlab='')
  abline(a = mean(d1), b = 0, col = 2)
  acf(d1,main='')
  pacf(d1,main='')
  
  Box.test(d1, lag=log(T), type='Ljung')
  
  ## Deterministic seasonal model: adjust the January effect
  
  jan = rep(c(1, rep(0,11)), T/12)
  
  m1 = lm(d1~jan)
  summary(m1)
  
  par(mfrow = c(3,1))
  plot(m1$residuals, type='l',main='',xlab='')
  acf(m1$residuals,main='')
  pacf(m1$residuals,main='')
  
  Box.test(m1$residuals, lag=log(T), type='Ljung')
  
  
  ## Multiplicative seasonal ARMA model
  
  m2 = arima(d1, order = c(1,0,0), seasonal = list(order = c(1,0,1), period = 12))
  m2
  tsdiag(m2, gof = 36)
  
  m2 = arima(d1, order = c(1,0,0), seasonal = list(order = c(1,0,1), period = 12),include.mean = F)
  m2
  tsdiag(m2, gof = 36)
  
#==================================================================
# (4) Real example: linear regression model with time series errors
#==================================================================
  r1 = read.table("w-gs1yr.txt", header = T)[,4]
  r3 = read.table("w-gs3yr.txt", header = T)[,4]
  T  = length(r1)


  par(mfrow = c(1,2))  
  plot(1:T, r1, type = 'l', ylab = '')
  par(new = T)
  plot(1:T, r3, type = 'l', col = 2, ylab = '')
  plot(r1, r3, type = 'p')


  par(mfrow = c(2,1))
  acf(r1)
  acf(r3)
#  plot(r1, r3, type = 'p')
#  plot(diff(r1), diff(r3), type = 'p')

## 1. Simple linear regression model
##    r_3t = r_1t + e_t

  m1 = lm(r3~r1)
  summary(m1)

  par(mfrow = c(3,1))
  plot(m1$residuals, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(m1$residuals)
  pacf(m1$residuals)

  install.packages("fUnitRoots")
  library(fUnitRoots)
  adfTest(m1$residuals)  

## 2. Simple linear regression model of the change series
##    (1-B)r_3t = (1-B)r_1t + e_t

  c1 = diff(r1)
  c3 = diff(r3)
  m2 = lm(c3~-1+c1)
  summary(m2)

  par(mfrow = c(3,1))
  plot(m2$residuals, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(m2$residuals)
  pacf(m2$residuals)


## 3. Linear regression model with time series errors
##    c_3t = beta c_1t + e_t,  e_t = (1-theta1 B) a_t

  m3 = arima(x = c3, order = c(0,0,1), xreg = c1, include.mean = F)
  m3
 
  rsq = (sum(c3^2)-sum(m3$residuals^2))/sum(c3^2)
  rsq

  par(mfrow = c(3,1))
  plot(m3$residuals, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(m3$residuals)
  pacf(m3$residuals)

  tsdiag(m3)
  Box.test(m3$residuals, lag=log(T), type='Ljung')

