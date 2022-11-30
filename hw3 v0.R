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

#VIX
getSymbols("^VIX", from = '2012-11-28', to = '2022-11-28')
VIX<-as.xts(data.frame(VIX = VIX[, "VIX.Adjusted"]))
VIX<-na.omit(VIX)
names(VIX) = c("VIX")
index(VIX) = as.Date(index(VIX))
#HSI
getSymbols("^N225", from = '2012-11-28', to = '2022-11-28')
N225<-as.xts(data.frame(N225 = N225[, "N225.Adjusted"]))
N225<-na.omit(N225)
names(N225) = c("N225")
index(N225) = as.Date(index(N225))
#==========================================
#Get original data
#==========================================
ori.VIX<-VIX 
oritp.VIX=tidy(as.xts(ori.VIX)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#2E9FDF") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(ori.VIX),"Stock Price for 10 years"),subtitle = "Stock Price",caption = " Time Plot") +
  xlab("Date") + ylab("Stock Price in Percentage") 
oritp.VIX
ggsave(paste(colnames(ori.VIX),"Stock Price time plot.png"), plot = oritp.VIX)

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