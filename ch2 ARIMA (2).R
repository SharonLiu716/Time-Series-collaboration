# Ch2 Linear Time Series Analysis: ARIMA
# Last Updated 2022.10.29 by S. F. Huang

#=========================
# (1) Random walk
#     y_t = y_{t-1} + a_t
#=========================
  T     = 1000
  sigma = 1
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = at[1]
  for (t in 2:T){
       y[t] = y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(y)
  pacf(y)
  
  
## Multiple realizations
  
  T     = 1000
  sigma = 1
  par(mfrow = c(1,1))
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = at[1]
  for (t in 2:T){
    y[t] = y[t-1] + at[t] 
  }
  plot(1:T, y, type='l',ylim=c(-4*sqrt(T)*sigma,4*sqrt(T)*sigma))
  abline(0, 0, col=2, lwd = 3)
  points(1:T,3*sqrt(1:T)*sigma, type = 'l', col=2, lty = 2, lwd = 3)
  points(1:T,-3*sqrt(1:T)*sigma, type = 'l', col=2, lty = 2, lwd = 3)
  
  for (k in 2:30){
    at    = rnorm(T,0,sigma)
    y     = matrix(0, nrow=T, ncol=1)
    y[1]  = at[1]
    for (t in 2:T){
      y[t] = y[t-1] + at[t] 
    }
  
    par(new=T)
    plot(1:T, y, type='l', ylim=c(-4*sqrt(T)*sigma,4*sqrt(T)*sigma))
    abline(0, 0, col=2, lwd = 3)
    points(1:T,3*sqrt(1:T)*sigma, type = 'l', col=2, lty = 2, lwd = 3)
    points(1:T,-3*sqrt(1:T)*sigma, type = 'l', col=2, lty = 2, lwd = 3)
  }
  

#==============================
# (2) Random walk with drift
#     y_t = mu + y_{t-1} + a_t
#==============================
  T     = 1000
  mu    = 0.5
  sigma = 1
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = mu + at[1]
  for (t in 2:T){
       y[t] = mu + y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = 0, b = mu, col = 2)
  acf(y)
  pacf(y)

## Multiple realizations
  
  T     = 1000
  mu    = 0.5
  sigma = 1
  par(mfrow = c(1,1))
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = at[1]
  for (t in 2:T){
    y[t] = mu + y[t-1] + at[t] 
  }
  plot(1:T, y, type='l',ylim=c(-sqrt(T)*sigma,mu*T+4*sqrt(T)*sigma))
  
  
  for (k in 2:30){
    at    = rnorm(T,0,sigma)
    y     = matrix(0, nrow=T, ncol=1)
    y[1]  = at[1]
    for (t in 2:T){
      y[t] = mu + y[t-1] + at[t] 
    }
    
    par(new=T)
    plot(1:T, y, type='l',ylim=c(-sqrt(T)*sigma,mu*T+4*sqrt(T)*sigma))
    abline(0, mu, col=2, lwd = 3)
    points(1:T,mu*seq(1,T)+3*sqrt(1:T)*sigma, type = 'l', col=2, lty = 2, lwd = 3)
    points(1:T,mu*seq(1,T)-3*sqrt(1:T)*sigma, type = 'l', col=2, lty = 2, lwd = 3)
  }
  
  
## 2.1 Fitting a time-trend model (wrong way)
  m1 = lm(y~c(1:T)) 
   
  par(mfrow = c(3,1))
  plot(1:T, m1$res, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(m1$res)  # still has significant autocorrelations
  pacf(m1$res)


## 2.2 Differencing (correct way)
  par(mfrow = c(3,1))
  plot(1:(T-1), diff(y), type='l')
  abline(a = 0, b = 0, col = 2)
  acf(diff(y))
  pacf(diff(y))


#========================================
# (3) Trend stationary time series model
#     y_t = beta0 + beta1*t + r_t
#     r_t is a stationary time series.
#========================================
  T     = 1000
  beta0 = 1
  beta1 = 2
  sigma = 10
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  rt    = matrix(0, nrow=T, ncol=1)
#  rt    = at  # rt is a WN series
  y[1]  = beta0 + beta1 + at[1]
  for (t in 2:T){
       rt[t] = 0.5*rt[t-1] + at[t] # E(r_t) = 0
       y[t] = beta0 + beta1*t + rt[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)

  
## Multiple realizations
  T     = 100
  beta0 = 1
  beta1 = 2
  sigma = 10
  
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  rt    = matrix(0, nrow=T, ncol=1)
  y[1]  = beta0 + beta1 + at[1]
  for (t in 2:T){
    rt[t] = 0.5*rt[t-1] + at[t] # E(r_t) = 0
    y[t] = beta0 + beta1*t + rt[t] 
  }
  par(mfrow = c(1,1))
  plot(1:T, y, type='l',ylim=c(-3*sigma,beta1*T+4*sigma))
  
  
  for (k in 2:30){
    at    = rnorm(T,0,sigma)
    y     = matrix(0, nrow=T, ncol=1)
    rt    = matrix(0, nrow=T, ncol=1)
    y[1]  = beta0 + beta1 + at[1]
    for (t in 2:T){
      rt[t] = 0.5*rt[t-1] + at[t] # E(r_t) = 0
      y[t] = beta0 + beta1*t + rt[t] 
    }
    
    par(new=T)
    plot(1:T, y, type='l',ylim=c(-3*sigma,beta1*T+4*sigma))
    abline(beta0, beta1, col=2, lwd = 3)
    points(1:T,beta1*seq(1,T)+3*sigma, type = 'l', col=2, lty = 2, lwd = 3)
    points(1:T,beta1*seq(1,T)-3*sigma, type = 'l', col=2, lty = 2, lwd = 3)
  }
  
  
## 3.1 Differencing (wrong way and induces over differencing)
  par(mfrow = c(3,1))
  plot(diff(y), type='l')
  abline(a = 0, b = 0, col = 2)
  acf(diff(y))
  pacf(diff(y)) # comparing this PACF with the previous one

  
  y = arima.sim(list(order = c(1,0,0), ar = 0.8), T)
  par(mfrow = c(2,3))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)
  plot(1:(T-1), diff(y), type='l')
  abline(a = 0, b = 0, col = 2)
  acf(diff(y))  
  pacf(diff(y))  # the over differencing is not severe

  
  y = arima.sim(list(order = c(0,0,1), ma = 0.7), T)
  par(mfrow = c(2,3))
  plot(1:T, y, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(y)
  pacf(y)
  plot(1:(T-1), diff(y), type='l')
  abline(a = 0, b = 0, col = 2)
  acf(diff(y))
  pacf(diff(y))  # significant over differencing

  
## 3.2 Fitting a time-trend model (correct way)
  m3 = arima(y, order = c(1,0,0), xreg = 1:T, include.mean = T)
  m3

  par(mfrow = c(3,1))
  plot(m3$residuals, type='l',main='',xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(m3$residuals,main='')
  pacf(m3$residuals,main='')
  
  tsdiag(m3)
  
  
#===============================================
# (4) Unit root test: Case 1
#     H_0: y_t = y_{t-1} + a_t
#     H_1: y_t = rho * y_{t-1} + a_t, |rho| < 1
#===============================================

  library(fUnitRoots)

## 4.1 Generate data from a random walk process
  T     = 500
  rho   = 1
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = at[1]                        
  for (t in 2:T){
       y[t] = rho * y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = mean(y), b = 0, col = 2)
  acf(y)
  pacf(y)

  adfTest(y, type="nc")

## 4.2 Generate data from an AR(1) process with zero intercept
  T     = 5000
  rho   = 0.995
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = at[1]                        
  for (t in 2:T){
       y[t] = rho * y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(y)
  pacf(y)

  adfTest(y, type="nc")


#=======================================================
# (5) Unit root test: Case 2
#     H_0: y_t = alpha + y_{t-1} + a_t
#     H_1: y_t = alpha + rho * y_{t-1} + a_t, |rho| < 1
#=======================================================

 # library(fUnitRoots)

## 5.1 Generate data from a random walk process
  T     = 500
  rho   = 1
  alpha = 1
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = alpha + at[1]                        
  for (t in 2:T){
       y[t] = alpha + rho * y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = mean(y), b = 0, col = 2)
  acf(y)
  pacf(y)

  adfTest(y, type="c")

## 5.2 Generate data from an AR(1) process with intercept
  T     = 5000
  alpha = 1
  rho   = 0.99
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = alpha + at[1]                        
  for (t in 2:T){
       y[t] = alpha + rho * y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = alpha/(1-rho), b = 0, col = 2)
  acf(y)
  pacf(y)

  adfTest(y, type="nc") # Wrong input
  
  adfTest(y, type="c")  # Correct input


#=================================================================
# (6) Unit root test: Case 3
#     H_0: y_t = alpha + delta*t + y_{t-1} + a_t
#     H_1: y_t = alpha + delta*t + rho * y_{t-1} + a_t, |rho| < 1
#=================================================================

  #library(fUnitRoots)

## 6.1 Generate data from a random walk process with drift and time trend
  T     = 500
  alpha = 1
  delta = 0.2
  rho   = 1
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = alpha + delta + at[1]                        
  for (t in 2:T){
       y[t] = alpha + delta*t + rho * y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)

  adfTest(y, type="ct")

## 6.2 Generate data from a time trend stationary process
  T     = 500
  alpha = 1
  delta = 0.2
  rho   = 0.9
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = alpha + delta + at[1]                        
  for (t in 2:T){
       y[t] = alpha + delta*t + rho * y[t-1] + at[t] 
  }
  
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)

  adfTest(y, type="c")  # Wrong input
  
  adfTest(y, type="ct") # Correct input


#=========================
# (7) Real data example 1
#=========================
  setwd("C:/Users/user/Dropbox/開課/財務方面/2022 財務時間序列/lectures") # set working directory
  #library(fUnitRoots)

  da   = read.table("q-gdp4708.txt", header=T)
  lgdp = log(da[,4])
  T    = length(lgdp)
  m1   = ar(diff(lgdp), method = 'mle')
  
  par(mfrow = c(2,3))
  plot(1:T, lgdp, type='l', main="lgdp")
  acf(lgdp, main="lgdp")
  pacf(lgdp, main="lgdp")
  
  plot(1:(T-1), diff(lgdp), type='l', main="diff(lgdp)")
  abline(a = mean(diff(lgdp)), b = 0, col = 2)
  acf(diff(lgdp), main="diff(lgdp)")
  pacf(diff(lgdp), main="diff(lgdp)")

  adfTest(lgdp, lags = m1$order, type = "ct")
 
  
## Model fitting
  
  m1 = arima(lgdp[1:(T-10)], order = c(5,1,2))
  m1  
  
  par(mfrow = c(3,1))
  plot(m1$residuals, type='l',main='',xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(m1$residuals,main='')
  pacf(m1$residuals,main='')
  
  tsdiag(m1)
  
  
## Multi-step Prediction
  pred = predict(m1, 10)
  ll   = min(c(lgdp[(T-20):T], pred$pred-2*pred$se))
  uu   = max(c(lgdp[(T-20):T], pred$pred+2*pred$se)) 
  
  par(mfrow = c(1,1))
  plot((T-20):T, lgdp[(T-20):T], type = 'l', ylim = c(ll,uu))
  lines((T-9):T,pred$pred+2*pred$se, lty = 2, ylim = c(ll,uu), col=2) 
  lines((T-9):T,pred$pred-2*pred$se, lty = 2, ylim = c(ll,uu), col=2)
  par(new=T)
  plot((T-9):T, pred$pred, type = 'p', col=3, ylim = c(ll,uu), xlim=c((T-20),T),
       xlab='',ylab='')
  par(new=T)
  abline(v=T-10, col=4, lty=3)
  
  
#=========================
# (8) Real data example 2
#=========================
  #setwd("C:/Users/user/Dropbox/開課/財務方面/2022 財務時間序列/lectures") # set working directory
  #library(fUnitRoots)

  da   = read.table("d-sp55008.txt", header=T)
  lsp5 = log(da[,7])
  T    = length(lsp5)
  m1   = ar(diff(lsp5), method = 'mle')
  
  par(mfrow = c(2,3))
  plot(1:T, lsp5, type='l', main="lsp5")
  acf(lsp5, main="lsp5")
  pacf(lsp5, main="diff(lsp5)")
  
  plot(1:(T-1), diff(lsp5), type='l', main="diff(lsp5)")
  abline(a = mean(diff(lsp5)), b = 0, col = 2)
  acf(diff(lsp5), main="lsp5")
  pacf(diff(lsp5), main="diff(lsp5)")

  adfTest(lsp5, lags = m1$order, type = "ct")
  adfTest(lsp5, lags = 15, type = "ct")

  
## Model fitting
  
  m1 = arima(lsp5[1:(T-10)], order = c(5,1,0))
  m1  
  
  par(mfrow = c(3,1))
  plot(m1$residuals, type='l',main='',xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(m1$residuals,main='')
  pacf(m1$residuals,main='')
  
  tsdiag(m1)
  
  
## Multi-step Prediction
  pred = predict(m1, 10)
  ll   = min(c(lsp5[(T-20):T], pred$pred-2*pred$se))
  uu   = max(c(lsp5[(T-20):T], pred$pred+2*pred$se)) 
  
  par(mfrow = c(1,1))
  plot((T-20):T, lsp5[(T-20):T], type = 'l', ylim = c(ll,uu))
  lines((T-9):T,pred$pred+2*pred$se, lty = 2, ylim = c(ll,uu), col=2) 
  lines((T-9):T,pred$pred-2*pred$se, lty = 2, ylim = c(ll,uu), col=2)
  par(new=T)
  plot((T-9):T, pred$pred, type = 'p', col=3, ylim = c(ll,uu), xlim=c((T-20),T),
       xlab='',ylab='')
  par(new=T)
  abline(v=T-10, col=4, lty=3)
  
  

  
  
