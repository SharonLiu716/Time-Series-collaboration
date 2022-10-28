# Ch2 Linear Time Series Analysis: ARMA 
# Last Updated 2022.10.14 by S. F. Huang

#===============
# (1) Load data
#=============== 
  setwd("C:/Users/user/Dropbox/開課/財務方面/2022 財務時間序列/lectures") # set working directory

  d_ibm_ln = scan("d-ibmln.txt")
  T        = length(d_ibm_ln)

## Plots ----------------
  par(mfrow = c(3,1))
  plot(1:T, d_ibm_ln, type='l', xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(d_ibm_ln, lag.max = log(T))
  pacf(d_ibm_ln)

## Ljung-Box test: Q(log(T)) -------------------
  Box.test(d_ibm_ln, lag=log(T), type='Ljung')

#================================================
# (2) Generate an IID process
#     y_t = mu + a_t, a_t iid N(0,sigma)
#================================================
  T     = 1000
  mu    = 0
  sigma = 1
  at    = rnorm(T,0,sigma)
  y1    = matrix(0, nrow=T, ncol=1)
  y1[1] = mu + at[1]
  for (t in 2:T){
       y1[t] = mu + at[t] 
  }
  
  ## Plots ----------------
  par(mfrow = c(3,1))
  plot(1:T, y1, type='l', xlab='')
  abline(a = 0, b = 0, col = 2)
  acf(y1, lag.max = log(T))
  pacf(y1)
  
  ## Ljung-Box test: Q(log(T)) -------------------
  Box.test(d_ibm_ln, lag=log(T), type='Ljung')



#================================================
# (2) Generate an AR(1) process
#     (1-phi B)y_t = mu + a_t, a_t iid N(0,sigma)
#================================================
  T     = 150
  mu    = 0.2
  phi   = 0.5
  sigma = 1
  at    = rnorm(T,0,sigma)
  y1    = matrix(0, nrow=T, ncol=1)
  y1[1] = mu + at[1]
  for (t in 2:T){
       y1[t] = mu + phi*y1[t-1] + at[t] 
  }
  
  phi2  = 1
  y2    = matrix(0, nrow=T, ncol=1)
  y2[1] = mu + at[1]
  for (t in 2:T){
       y2[t] = mu + phi2*y2[t-1] + at[t] 
  }


## 1. Time plot ----------------

  par(mfrow = c(2,1))
  plot(1:T, y1, type='l',xlab='')
  abline(a = mean(y1), b = 0, col = 2)
  plot(1:T, y2, type='l',xlab='')
  abline(a = mean(y2), b = 0, col = 2)

  par(mfrow = c(2,2))
  acf(y1)
  acf(y2)
  pacf(y1)
  pacf(y2)

## 2. Ljung-Box test: Q(log(T)) -------------------
  Box.test(y1, lag=log(T), type='Ljung')



#=================================
# (3) Fit an AR model for the data 
#=================================

## 1. Find the AR order based on AIC ----------

  m1 = ar(y1[,1], method="mle")
  m1$aic
  m1$order

## 2. Estimation -------------

#  m2 = arima(y1[,1], order=c(m1$order,0,0))
  m2 = arima(y1[1:100,1], order=c(1,0,0))
  m2
  head(m2)  

  r_hat_1 = NULL
  r_hat_1 = y1[1]
  for (t in 2:(T-50)){
    r_hat_1[t] = (1-m2$coef[1])*m2$coef[2] + m2$coef[2]*y1[t-1]
  }
  for (t in (T-49):T){
    r_hat_1[t] = (1-m2$coef[1])*m2$coef[2] + m2$coef[2]*y1[t-1]
  }
# one-step ahead predition  
  par(mfrow=c(1,2))
  plot(1:T, y1, type='l',xlab='',lwd=2, main='1000*return')
  lines(1:(T-50), r_hat_1[1:(T-50)], lwd=2, col=4)
  lines((T-50):T, r_hat_1[(T-50):T], lwd=2, col=2)
  abline(v=100, lty=2, col=3,lwd=2)

# Convert to price    
  Pt=NULL
  Pt_hat = NULL
  Pt[1]=100
  Pt_hat[1] = Pt[1]
  for (t in 2:T){
    Pt[t] = Pt[t-1]*(1+y1[t]/1000)
    Pt_hat[t] = Pt[t-1]*(1+r_hat_1[t]/1000)
  }
  plot(1:T, Pt, type='p',xlab='',lwd=2, main = 'Price')
  lines(1:(T-50), Pt_hat[1:(T-50)], lwd=2, col=4)
  lines((T-50):T, Pt_hat[(T-50):T], lwd=2, lty = 2, col=2)
  abline(v=100, lty=2, col=3,lwd=2)
  
  
## 3. Note that "intercept" denotes the mean of the series.
##    Therefore, the constant term is obtained below --------------

  phi0_hat = (1-m2$coef[1])*m2$coef[2]
  phi0_hat

## 4. Residual standard error --------------

  sqrt(m2$sigma2)

## 5. Model checking: plots and test of the residuals ---------------

  Box.test(m2$residuals, lag=log(T), type='Ljung')

  par(mfrow = c(3,1))
  plot(1:length(m2$residuals), m2$residuals, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(m2$residuals)
  pacf(m2$residuals)


#=================
# (4) AR(2) model
#=================

## 1. Generate an AR(2) series --------------

  T     = 1000
  mu    = 3
  phi1  = 1.2
  phi2  = -0.7
  sigma = 2
  at    = rnorm(T,0,sigma)
  y     = matrix(0, nrow=T, ncol=1)
  y[1]  = mu + at[1]
  y[2]  = mu + phi1*y[1] + at[2]
  for (t in 3:T){
       y[t] = mu + phi1*y[t-1] + phi2*y[t-2] + at[t] 
  }
 
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  abline(a = mu/(1-phi1-phi2), b = 0, col = 2)
  acf(y)
  pacf(y)

  Box.test(y, lag=log(T), type='Ljung')

## 2. Model fitting and model diagnostic --------------

  m1 = ar(y[,1], method="mle")
  m1$aic
  m1$order
  m2 = arima(y[,1], order=c(m1$order,0,0))
  m2

  phi0_hat = (1-sum(m2$coef[1:m1$order]))*m2$coef[m1$order+1]
  phi0_hat

  sqrt(m2$sigma2)

  tsdiag(m2, gof=floor(log(T))+1) 


## 3. Roots of the characteristic function -------------

  p1 = c(1,-m2$coef[1:m1$order])
  p1

  roots = polyroot(p1) 
  roots          

## 6. Check whether the roots are ourside the unit circle -----------

  modulus = Mod(roots)
  modulus

## 7. Compute the average length of business cycles -------------

  k = 2*pi/acos(Re(roots[1])/modulus[1])
  k

##=====================================================
## (5) Fit an AR(3) model for the US GNP data
##=====================================================

  gnp  = scan("dgnp82.txt")
  T    = length(gnp)
  gnp1 = ts(gnp, frequency = 4, start = c(1947,2)) # start from the 2nd quarter of 1947
  
  par(mfrow = c(3,1))
  plot(gnp1)
  abline(a = mean(gnp1), b = 0, col = 2)
  acf(gnp)
  pacf(gnp)

  m1 = ar(gnp, method="mle")
  m1$aic
  m1$order

  m2 = arima(gnp, order=c(m1$order,0,0))
  m2

  phi0_hat = (1-sum(m2$coef[1:m1$order]))*m2$coef[m1$order+1]
  phi0_hat

  sqrt(m2$sigma2)

  tsdiag(m2, gof=floor(log(T))+1) 

  p1    = c(1,-m2$coef[1:m1$order])
  roots = polyroot(p1) 
  roots          
  modulus = Mod(roots)
  modulus

  k = 2*pi/acos(Re(roots[1])/modulus[1])
  k

##=================================================================
## (6) Fit an AR(3) model with the AR(2) coefficient fixed to zero
##=================================================================
  
  vw  = read.table("m-ibm3dx2608.txt", header=T)[3]
  T   = dim(vw)[1]
  m1 = ar(vw[,1], method="mle")
  m1$order

  par(mfrow=c(3,1))
  plot(vw[,1],type='l')
  abline(a = mean(vw[,1]), b = 0, col = 2)
  acf(vw[,1])
  pacf(vw[,1])

## 1. Fit an AR(3) model --------------------

  m3  = arima(vw, order=c(3,0,0))
  m3

  phi0_hat = (1-sum(m3$coef[1:3]))*m3$coef[4]
  phi0_hat

  sqrt(m3$sigma2)

  tsdiag(m3) 

  
## 2. Fit an AR(3) model with phi_2 = 0 --------------------

  m4  = arima(vw, order = c(3,0,0), fixed = c(NA,0,NA,NA))
  m4

  phi0_hat = (1-sum(m4$coef[1:3]))*m4$coef[4]
  phi0_hat

  sqrt(m4$sigma2)

  tsdiag(m4) 


##================
## (7) Prediction
##================

  pred = predict(m4, 10)
  ll   = min(c(vw[947:996,1],pred$pred-2*pred$se))
  uu   = max(c(vw[947:996,1],pred$pred+2*pred$se)) 
 
  par(mfrow=c(1,1))
  plot(c(vw[947:996,1], pred$pred), type = 'l', ylim = c(ll,uu))
  lines(51:60,pred$pred+2*pred$se, lty = 2, ylim = c(ll,uu), col=2) 
  lines(51:60,pred$pred-2*pred$se, lty = 2, ylim = c(ll,uu), col=2)
  par(new=T)
  plot(51:60, pred$pred, type = 'p', col=2, ylim = c(ll,uu), xlim=c(1,60),
       xlab='',ylab='')
  

##====================================
## (8) MA(1) model: 
##     y_t = mu + a_t - theta1*a_{t-1}
##=====================================

## 1. Generate an MA(1) series --------------

  T      = 1000
  mu     = 3
  theta1 = 0.8
  sigma  = 2
  at     = rnorm(T,0,sigma)
  y      = matrix(0, nrow=T, ncol=1)
  y[1]   = mu + at[1]
  for (t in 2:T){
       y[t] = mu + at[t] - theta1*at[t-1] 
  }
 
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)

  Box.test(y, lag=log(T), type='Ljung')

## 2. Model fitting and model diagnostic --------------

  m1 = ar(y[,1], method="mle")
  m1$aic
  m1$order

  m2 = arima(y[,1], order=c(0,0,1))
  m2

  sqrt(m2$sigma2)

  -2*m2$loglik + 2*(length(m2$coef)+1)  # check the value of AIC

  tsdiag(m2, gof=floor(log(T))+1) 


##=====================================================
## (9) MA(2) model: 
##     y_t = mu + a_t - theta1*a_{t-1} - theta2*a_{t-2}
##=====================================================

## 1. Generate an MA(2) series --------------

  T      = 1000
  mu     = 3
  theta1 = 0.8
  theta2 = -0.5
  sigma  = 2
  at     = rnorm(T,0,sigma)
  y      = matrix(0, nrow=T, ncol=1)
  y[1]   = mu + at[1]
  y[2]   = mu + at[2] - theta1*at[1]
  for (t in 3:T){
       y[t] = mu + at[t] - theta1*at[t-1] - theta2*at[t-2]
  }
 
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)

  Box.test(y, lag=log(T), type='Ljung')

## 2. Model fitting and model diagnostic --------------

  m1 = arima(y[,1], order=c(0,0,1))
  m1
  sqrt(m1$sigma2)
  tsdiag(m1, gof=floor(log(T))+1) 

  m2 = arima(y[,1], order=c(0,0,2))
  m2
  sqrt(m2$sigma2)
  tsdiag(m2, gof=floor(log(T))+1)

  m3 = arima(y[,1], order=c(0,0,3))
  m3
  sqrt(m3$sigma2)
  tsdiag(m3, gof=floor(log(T))+1)


##==========================================
## (10) ARMA(1,1) model: 
##     (1-phi_1 B)y_t = mu + (1-theta1 B)a_t
##==========================================

## 1. Generate an ARMA(1,1) series --------------

  T      = 1000
  mu     = 3
  phi1   = -0.5
  theta1 = 0.8
  sigma  = 2
  at     = rnorm(T,0,sigma)
  y      = matrix(0, nrow=T, ncol=1)
  y[1]   = mu + at[1]
  for (t in 2:T){
       y[t] = mu + phi1*y[t-1] + at[t] - theta1*at[t-1] 
  }
 
  par(mfrow = c(3,1))
  plot(1:T, y, type='l')
  acf(y)
  pacf(y)

  Box.test(y, lag=log(T), type='Ljung')

## 2. Model fitting and model diagnostic --------------

  m1 = arima(y[,1], order=c(6,0,0))
  m1
  sqrt(m1$sigma2)
  tsdiag(m1, gof=floor(log(T))+1) 

  m2 = arima(y[,1], order=c(0,0,4))
  m2
  sqrt(m2$sigma2)
  tsdiag(m2, gof=floor(log(T))+1)

  m3 = arima(y[,1], order=c(1,0,1))
  m3
  sqrt(m3$sigma2)
  tsdiag(m3, gof=floor(log(T))+1)

#===============================================================
# (11) Plot ACF and PACF of AR(1) and AR(2) process
#===============================================================
# AR(1)
  T     = 1000
  mu    = 0.2
  phi   = -0.5
  sigma = 1
  at    = rnorm(T,0,sigma)
  y1    = matrix(0, nrow=T, ncol=1)
  y1[1] = mu + at[1]
  for (t in 2:T){
       y1[t] = mu + phi*y1[t-1] + at[t] 
  }

# AR(2)
  T     = 1000
  mu    = 3
  phi1  = 0.5
  phi2  = -0.3
  sigma = 2
  at    = rnorm(T,0,sigma)
  y2     = matrix(0, nrow=T, ncol=1)
  y2[1]  = mu + at[1]
  y2[2]  = mu + phi1*y[1] + at[2]
  for (t in 3:T){
       y2[t] = mu + phi1*y2[t-1] + phi2*y2[t-2] + at[t] 
  }

# plot 
  par(mfrow = c(2,2))
  acf(y1, main='AR(1)') 
  pacf(y1, main='AR(1)')
  acf(y2, main='AR(2)')
  pacf(y2, main='AR(2)')




