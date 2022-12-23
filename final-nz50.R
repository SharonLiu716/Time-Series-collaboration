rm(list=ls(all=TRUE)) 
#setwd("C:/Users/User/Documents/Time-Series-collaboration")
library(zoo)
library(xts)
library(TTR)
library(quantmod)# Finance data
library(ggplot2)
library(magrittr)
library(broom)
library(qpcR)   
library(fpp2)
library(fUnitRoots)
library(stats)
library(fGarch)
detect <- function(data,p,q){
  temp <- c()
  ord <- c()
  for (i in 0:p) {
    for (j in 0:q) {
      if (i == 0 & j==0) next
      temp <- c(temp,arima(data,order = c(i,0,j))$aic)
      ord <- rbind(ord, c(i,0,j))
    }
  }
  list(likelihood = sort(temp),order = ord[order(temp), ])
}
res <- function(data,obj){
  
  ACF<-acf(data, lag.max = 30,plot = FALSE)
  plot(ACF, main = paste("ACF of",obj))
  PACF<-pacf(data, lag.max = 30,plot = FALSE)
  plot(PACF, main = paste("PACF of",obj))
  par(mfrow=c(1,1))
  plot(as.xts(data),main= paste("Time Plot of",obj))
  
}
#美紐匯率
getSymbols("USDNZD=X", from = '2012-10-12', to = '2022-10-12')
USDNZD<-as.xts(data.frame(`USDNZD=X` = `USDNZD=X`[, paste0("USDNZD=X.Adjusted")]))
USDNZD<-na.omit(USDNZD)
names(USDNZD) = c("USDNZD")
index(USDNZD) = as.Date(index(USDNZD))
res(USDNZD,'original USDNZD')


logrt.USDNZD<-apply( log(USDNZD) , 2 , diff )*100
res(logrt.USDNZD,'log return of USDNZD')

Box.test(logrt.USDNZD, lag=10, type='Ljung')
Box.test(logrt.USDNZD^2, lag=10, type='Ljung')

#adf test , p-value<0.05->don't diff
adfTest(logrt.USDNZD,type = "c")

#fit Garch
mod.UN<-garchFit(logrt.USDNZD~ garch(1,0), data = logrt.USDNZD, trace = FALSE)
summary(mod.UN)
plot(mod.UN)
par(mfrow=c(1,2))
mod.UN<-garchFit(logrt.USDNZD~ garch(1,1), data = logrt.USDNZD, trace = FALSE)
summary(mod.UN)
plot(mod.UN)
mod.UN<-garchFit(logrt.USDNZD~ garch(1,1), data = logrt.USDNZD, trace = FALSE,cond.dist = 'sstd')
summary(mod.UN)
plot(mod.UN)

#紐西蘭50
getSymbols("^NZ50", from ='2012-12-12', to = '2022-12-12')
NZ50 <- as.xts(data.frame(NZ50 = NZ50[, "NZ50.Adjusted"]))
NZ50 <- na.omit(NZ50)
names(NZ50) = c("NZ50")
index(NZ50) = as.Date(index(NZ50))
res(NZ50,'original NZ50')

logrt.NZ50 <- apply( log(NZ50) , 2 , diff )*100
res(logrt.NZ50,'log return of NZ50')

Box.test(logrt.NZ50, lag=10, type='Ljung')
Box.test(logrt.NZ50^2, lag=10, type='Ljung')


#adf test , p-value<0.05->don't diff
adfTest(logrt.NZ50,type = "ct")

# Check aic and it's order given max.p & max.q
detect(logrt.NZ50, p = 9, q = 3)

fit.NZ50 <- arima(logrt.NZ50, order = c(4,0,2))
fit.NZ50
Residual<-fit.NZ50$residuals
res(Residual,'Residual of ARIMA(4,0,2)for log NZ50')
Box.test(Residual, lag = 12, type='Ljung')
res(Residual^2,'Residual^2 of ARIMA(4,0,2)for log NZ50')
Box.test(Residual^2, lag = 12, type='Ljung')
#garch(1,0) residual box test result:p-value = 0.01085-->garch(1,1)
mod.NZ50 = garchFit(logrt.NZ50~arma(4,2) + garch(1,0), data = logrt.NZ50, trace = FALSE)
summary(mod.NZ50)
plot(mod.NZ50)
mod.NZ50 = garchFit(logrt.NZ50~ arma(4,2) + garch(1,1), data = logrt.NZ50, trace = FALSE)
summary(mod.NZ50)
plot(mod.NZ50)
mod.NZ50 = garchFit(logrt.NZ50~arma(4,2) + garch(1,1), data = logrt.NZ50, trace = FALSE,cond.dist = 'sstd')
summary(mod.NZ50)
plot(mod.NZ50)

res.NZ50 = residuals(mod.NZ50, standardize = T)
Box.test(res.NZ50, lag = 12, type='Ljung')
Box.test(res.NZ50^2, lag = 12, type='Ljung')

ks.test(res.NZ50,"pt",df)
ad2_stat <- function(x, y) {
  
  # Sample sizes
  n <- length(x)
  m <- length(y)
  
  # Pooled sample and pooled ecdf
  z <- c(x, y)
  z <- z[-which.max(z)] # Exclude the largest point
  H <- rank(z) / (n + m)
  
  # Statistic computation via ecdf()
  (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2 / ((1 - H) * H))
  
}
ad0 <- ad2_stat(x = res.NZ50, y =t(length(res.NZ50), df))
pval0 <- 1 - goftest::pAD(q = ad0)
c("statistic" = ad0, "p-value"= pval0)

fit.NZ50 <- arima(logrt.NZ50, order = c(4,0,2),
                  fixed = c(NA,NA,0,NA,NA,NA,NA),transform.pars= FALSE)
fit.NZ50


tsdiag(fit.NZ50)
Box.test(fit.NZ50$residuals, lag=10, type='Ljung')
arch.test(fit.NZ50)
Resids=fit.NZ50$residuals
plot(Resids)
acf(Resids^2)
pacf(Resids^2)


m3 = arima(logrt.NZ50, order = c(4,0,2), xreg = 1:length(logrt.NZ50), include.mean = T)
m3
