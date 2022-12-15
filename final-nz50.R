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
#紐西蘭50
getSymbols("^NZ50", from ='2012-12-12', to = '2022-12-12')
NZ50 <- as.xts(data.frame(NZ50 = NZ50[, "NZ50.Adjusted"]))
NZ50 <- na.omit(NZ50)
names(NZ50) = c("NZ50")
index(NZ50) = as.Date(index(NZ50))
logrt.NZ50 <- apply( log(NZ50) , 2 , diff )*100
plot(NZ50,main=names(NZ50))
acf(NZ50, lag.max = 30)
pacf(NZ50, lag.max = 30)
#plot(logrt.NZ50,main=names(logrt.NZ50))
tp.NZ50=tidy(as.xts(logrt.NZ50)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.NZ50),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
tp.NZ50
acf(logrt.NZ50, lag.max = 30)
pacf(logrt.NZ50, lag.max = 30)
Box.test(logrt.NZ50, lag=10, type='Ljung')

getSymbols("USDNZD=X", from = '2012-10-12', to = '2022-10-12')
USDNZD<-as.xts(data.frame(`USDNZD=X` = `USDNZD=X`[, paste0("USDNZD=X.Adjusted")]))
USDNZD<-na.omit(USDNZD)
names(USDNZD) = c("USDNZD")
index(USDNZD) = as.Date(index(USDNZD))
logrt.USDNZD<-apply( log(USDNZD) , 2 , diff )*100
plot(USDNZD,main=names(USDNZD))
acf(USDNZD, lag.max = 30)
pacf(USDNZD, lag.max = 30)
#plot(logrt.USDNZD,main=names(logrt.USDNZD))
tp.USDNZD=tidy(as.xts(logrt.USDNZD)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.USDNZD),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
tp.USDNZD
acf(logrt.USDNZD, lag.max = 30)
pacf(logrt.USDNZD, lag.max = 30)
Box.test(logrt.USDNZD, lag=10, type='Ljung')


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
res <- function(data){
  
  acf(data, lag.max = 30)
  #  title(paste("ACF","of",name), line = 0)
  
  pacf(data, lag.max = 30)
  #  title(paste("PACF","of",name), line = 0.3)
  
}
detect(logrt.NZ50, p = 9, q = 2)
adfTest(logrt.NZ50,type = "ct")
fit.NZ50 <- arima(logrt.NZ50, order = c(4,0,2))
fit.NZ50
fit.NZ50 <- arima(logrt.NZ50, order = c(4,0,2),
                  fixed = c(NA,NA,0,NA,NA,NA,NA),transform.pars= FALSE)
fit.NZ50

fit.NZ50$residuals %>% res()
tsdiag(fit.NZ50)
Box.test(fit.NZ50$residuals, lag=10, type='Ljung')
arch.test(fit.NZ50)
Resids=fit.NZ50$residuals
plot(Resids)
acf(Resids^2)
pacf(Resids^2)


m3 = arima(logrt.NZ50, order = c(4,0,2), xreg = 1:length(logrt.NZ50), include.mean = T)
m3
