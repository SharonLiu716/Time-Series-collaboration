setwd("C:/Github/Data-Analysis-Practice/Time Series/Lecture/hw2") # set working directory
library(zoo)
library(xts)
library(TTR)
library(quantmod)
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
#SP500
getSymbols("^GSPC", from ='2012-10-12', to = '2022-10-12')
SP500<-as.xts(data.frame(GSPC = GSPC[, "GSPC.Adjusted"]))
names(SP500) = c("SP500")
index(SP500) = as.Date(index(SP500))
#TWII
getSymbols("^TWII", from = '2012-10-12', to = '2022-10-12')
TWII<-as.xts(data.frame(TWII = TWII[, "TWII.Adjusted"]))
TWII<-na.omit(TWII)
names(TWII) = c("TWII")
index(TWII) = as.Date(index(TWII))
#FTSE
getSymbols("^FTSE", from = '2012-10-12', to = '2022-10-12')
FTSE<-as.xts(data.frame(FTSE = FTSE[, "FTSE.Adjusted"]))
FTSE<-na.omit(FTSE)
names(FTSE) = c("FTSE")
index(FTSE) = as.Date(index(FTSE))
#==========================================
#Get daily log return and time plots
#==========================================
#SP500
logrt.SP500<-apply( log(SP500) , 2 , diff )*100
#log again
logrttp.SP500=tidy(as.xts(logrt.SP500)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.SP500),"Log Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
logrttp.SP500
ggsave(paste(colnames(logrt.SP500),"daily log return time plot.png"), plot = logrttp.SP500)

logrt.TWII<-apply( log(TWII) , 2 , diff )*100
logrttp.TWII=tidy(as.xts(logrt.TWII)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#2E9FDF") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.TWII),"Log Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
logrttp.TWII
ggsave(paste(colnames(logrt.TWII),"daily log return time plot.png"), plot = logrttp.TWII)

logrt.FTSE<-apply( log(FTSE) , 2 , diff )*100
logrttp.FTSE=tidy(as.xts(logrt.FTSE)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#FC4E07") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.FTSE),"Log Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
logrttp.FTSE
ggsave(paste(colnames(logrt.FTSE),"daily log return time plot.png"), plot = logrttp.FTSE)


#==========================================
#Get daily return and time plots
#==========================================
#SP500
rt.SP500<-(exp(apply( log(SP500) , 2 , diff ))-1)*100
rttp.SP500=tidy(as.xts(rt.SP500)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(rt.SP500),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
rttp.SP500
ggsave(paste(colnames(rt.SP500),"daily return time plot.png"), plot = rttp.SP500)
#TWII
rt.TWII<-(exp(apply( log(TWII) , 2 , diff ))-1)*100
rttp.TWII=tidy(as.xts(rt.TWII)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#2E9FDF") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(rt.TWII),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
rttp.TWII
ggsave(paste(colnames(rt.TWII),"daily return time plot.png"), plot = rttp.TWII)
#FTSE
rt.FTSE<-(exp(apply( log(FTSE) , 2 , diff ))-1)*100
rttp.FTSE=tidy(as.xts(rt.FTSE)) %>%
      ggplot(aes(x=index,y=value)) + geom_line(color="#FC4E07") +geom_hline(yintercept = 0,color='black')+
      labs(title = paste(colnames(rt.FTSE),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
      xlab("Date") + ylab("Daily Return in Percentage") 
rttp.FTSE
ggsave(paste(colnames(rt.FTSE),"daily return time plot.png"), plot = rttp.FTSE)

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
#ggAcf(logrt.SP500,lag.max =log(length(logrt.SP500))) + labs(title = "ACF PLot of SP500")
#-------------------------------------------
#plot acf & pacf for stocks log return
#-------------------------------------------
res(logrt.SP500,"SP500 log return")
res(logrt.TWII,"TWII log return")
res(logrt.FTSE,"FTSE log return")
#-------------------------------------------
#plot acf & pacf for stocks simple return
#-------------------------------------------
res(rt.SP500,"SP500 simple return")
res(rt.TWII,"TWII simple return")
res(rt.FTSE,"FTSE simple return")
#==========================================
#construct AR model
##-Find the AR order based on AIC(mle)
##-use ACF & PACF to find AR order
#==========================================

#------------------------------------------
##MLE
#------------------------------------------
modle.MLE<-function(data){
  
  m1 = ar(data, method="mle")
  m1$aic
  m1$order
  
  m2 = arima(data, order=c(m1$order,0,0))
  m2
  
  phi0_hat = (1-sum(m2$coef[1:m1$order]))*m2$coef[m1$order+1]
  phi0_hat
  
  sqrt(m2$sigma2)
  
  bt<-Box.test(m2$residuals, lag=10, type='Ljung')
  
  par(mfrow = c(3,1))
  plot(1:length(m2$residuals), m2$residuals, type='l')
  abline(a = 0, b = 0, col = 2)
  acf(m2$residuals)
  pacf(m2$residuals)
  
  
  tsdiag(m2, gof=floor(log(T))+1) 
  
  p1    = c(1,-m2$coef[1:m1$order])
  roots = polyroot(p1) 
  roots          
  modulus = Mod(roots)
  modulus
  
  k = 2*pi/acos(Re(roots[1])/modulus[1])
  k
  res<-list()
  objs<-c("AIC","mle.order","m1","m2","Residual Box Test","phi0_hat","sigma2","roots","modulus","business cycle")
  res.num<-list(m1$aic,m1$order,m1,m2,bt,phi0_hat,sqrt(m2$sigma2),roots ,modulus,k )
  for (i in 1:length(res.num)){res[objs[i]] <- res.num[i]}
  return(res)
}


logres.SP500<-modle.MLE(as.numeric(logrt.SP500))
logres.TWII<-modle.MLE(as.numeric(logrt.TWII))
logres.FTSE<-modle.MLE(as.numeric(logrt.FTSE))

res.SP500<-modle.MLE(as.numeric(rt.SP500))
res.TWII<-modle.MLE(as.numeric(rt.TWII))
res.FTSE<-modle.MLE(as.numeric(rt.FTSE))

