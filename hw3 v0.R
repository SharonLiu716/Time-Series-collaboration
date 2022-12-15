setwd("C:/Users/User/Documents/Time-Series-collaboration") # set working directory
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(fUnitRoots)
#plot packages
library(ggplot2)
library(magrittr)
library(broom)
#install.packages("fpp2")                            # Install qpcR package
library(qpcR)   
library(fpp2)
#start<-paste0(as.numeric(format(Sys.Date(), "%Y"))-11,format(Sys.Date(), "-%m-%d"))

#===============================================================
#Get stocks price
#===============================================================
getSymbols("^N225", from = '2012-10-12', to = '2022-10-12')
N225<-as.xts(data.frame(N225 = N225[, "N225.Adjusted"]))
N225<-na.omit(N225)
names(N225) = c("N225")
index(N225) = as.Date(index(N225))

res <- function(data){
  
  acf(data, lag.max = 30)
  #  title(paste("ACF","of",name), line = 0)
  
  pacf(data, lag.max = 30)
  #  title(paste("PACF","of",name), line = 0.3)
  
}

# Check aic and it's order given max.p & max.q
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

plot(N225,main='N225')
adfTest(N225,type = "nc")
res(N225)

plot(diff(N225),main='N225')
adfTest(N225,type = "nc")
adfTest(diff(N225),type = "c")
adfTest(diff(Price_data[[1]]),type = "ct")
res(diff(N225)[-1])# p=3, q=2

# determine the order
auto.arima(diff(N225),max.p = 4, max.q = 2)
arima(diff(N225), order = c(4,0,0))
detect(diff(N225), p = 4, q = 2)

fit.N225 <- arima(diff(N225), order = c(3,0,0))
fit.N225
fit.N225 <- arima(diff(N225), order = c(3,0,0),fixed = c(0,NA,0,NA),transform.pars='FALSE')
fit.N225
fit.N225$residuals[-1] %>% res()
tsdiag(fit.N225)

#===============================================================
#Get stocks price
#===============================================================
getSymbols("^GDAXI", from = '2012-10-12', to = '2022-10-12')
GDAXI<-as.xts(data.frame(GDAXI = GDAXI[, "GDAXI.Adjusted"]))
GDAXI<-na.omit(GDAXI)
names(GDAXI) = c("GDAXI")
index(GDAXI) = as.Date(index(GDAXI))

res <- function(data){
  
  acf(data, lag.max = 30)
  #  title(paste("ACF","of",name), line = 0)
  
  pacf(data, lag.max = 30)
  #  title(paste("PACF","of",name), line = 0.3)
  
}

# Check aic and it's order given max.p & max.q
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

plot(GDAXI,main='GDAXI')
adfTest(GDAXI,type = "nc")
res(GDAXI)

plot(diff(GDAXI),main='GDAXI')
adfTest(diff(GDAXI),type = "nc")
Box.test(GDAXI, lag=10, type='Ljung')
res(diff(GDAXI)[-1])# p=3, q=2

# determine the order
auto.arima(diff(N225),max.p = 4, max.q = 2)
arima(diff(VIX), order = c(4,0,0))
detect(diff(VIX), p = 4, q = 2)

fit.N225 <- arima(diff(N225), order = c(3,0,0))
fit.N225
fit.N225 <- arima(diff(N225), order = c(3,0,0),fixed = c(0,NA,0,NA))
fit.GSPC
fit.GSPC$residuals[-1] %>% res()
tsdiag(fit.GSPC)
#==========================================
#Get original data
#==========================================
ori.N225<-N225 
oritp.N225 =tidy(as.xts(ori.N225)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(ori.N225),"Stock Price for 10 years"),subtitle = "Stock Price",caption = " Time Plot") +
  xlab("Date") + ylab("Stock Price") 
oritp.N225
ggsave(paste(colnames(ori.N225),"Stock Price time plot.png"), plot = oritp.N225)
#==========================================
#ADF test
#==========================================
adfTest(ori.VIX, type="c")
adfTest(ori.N225, type="ct")
#==========================================
#Get daily log return and time plots
#==========================================
log.VIX<-log(VIX)
logtp.VIX=tidy(as.xts(log.VIX)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#2E9FDF") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(log.VIX),"Log Price for 10 years"),subtitle = "Log Price",caption = " Time Plot") +
  xlab("Date") + ylab("Log Price") 
logtp.VIX
ggsave(paste(colnames(log.VIX),"daily log price time plot.png"), plot = logtp.VIX)


log.N225<-log(N225)
logtp.N225=tidy(as.xts(log.N225)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(log.N225),"Log Price for 10 years"),subtitle = "Log Price",caption = " Time Plot") +
  xlab("Date") + ylab("Log Price") 
logtp.N225
ggsave(paste(colnames(log.N225),"daily log price time plot.png"), plot = logtp.N225)

#==========================================
#plot acf & pacf 
#==========================================
res<-function(data,name){
  jpeg(paste("ACF & PACF","of",name,".jpg"))
  par(mfrow = c(2,1))
  p1<-acf(data, lag.max = log(length(data)), plot = FALSE)
  plot(p1, main = paste("ACF","of",name))
  p2<-pacf(data, plot = FALSE)
  plot(p2, main = paste("PACF","of",name))
  dev.off() 
  Box.test(data, lag=10, type='Ljung')
}
#-------------------------------------------
#plot acf & pacf for stocks price
#-------------------------------------------
res(log.VIX,"VIX log price")
res(ori.VIX,"N225 stock price")
#-------------------------------------------
#plot acf & pacf for stocks price
#-------------------------------------------
res(log.N225,"N225 log price")
res(ori.N225,"N225 stock price")

#==========================================
#ADF test
#==========================================
adfTest(ori.N225, type="c")
adfTest(ori.VIX, type="c")
adfTest(log.N225, type="c")
adfTest(log.VIX, type="c")
#==========================================
#plot ACF PACF after diff
#==========================================
plot(1:(length(log.VIX)-1), diff(as.numeric(logrt.VIX)), type='l', main="log price of VIX, d=1")
abline(a = mean(diff(as.numeric(log.VIX))), b = 0, col = 2)
acf(diff(as.numeric(log.VIX)), main="log price of VIX, d=1")
pacf(diff(as.numeric(log.VIX)), main="log price of VIX, d=1")

plot(1:(length(log.N225)-1), diff(as.numeric(logrt.N225)), type='l', main="log price of N225, d=1")
abline(a = mean(diff(as.numeric(log.N225))), b = 0, col = 2)
acf(diff(as.numeric(log.N225)), main="log price of N225, d=1")
pacf(diff(as.numeric(log.N225)), main="log price of N225, d=1")
#==========================================
#construct AR model
##-Find the AR order based on AIC(mle)
##-use ACF & PACF to find AR order
#==========================================




adfTest(logrt.N225, type="c")
adfTest(logrt.VIX, type="c")
adfTest(rt.HSI, type="ct")
#=========================
# (7) Real data example 1
#=========================


adfTest(diff(as.numeric(logrt.VIX)), lags = m1$order, type = "nc")


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