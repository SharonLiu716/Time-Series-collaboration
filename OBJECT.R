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
#美國nasdaq
getSymbols("^IXIC", from ='2012-12-12', to = '2022-12-12')
IXIC <- as.xts(data.frame(IXIC = IXIC[, "IXIC.Adjusted"]))
IXIC <- na.omit(IXIC)
names(IXIC) = c("IXIC")
index(IXIC) = as.Date(index(IXIC))
logrt.IXIC <- apply( log(IXIC) , 2 , diff )*100
plot(IXIC,main=names(IXIC))
acf(IXIC, lag.max = 30)
pacf(IXIC, lag.max = 30)
#plot(logrt.IXIC,main=names(logrt.IXIC))
tp.IXIC=tidy(as.xts(logrt.IXIC)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.IXIC),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
tp.IXIC
acf(logrt.IXIC, lag.max = 30)
pacf(logrt.IXIC, lag.max = 30)
Box.test(logrt.IXIC, lag=10, type='Ljung')

#日本N225
getSymbols("^N225", from ='2012-12-12', to = '2022-12-12')
N225 <- as.xts(data.frame(N225 = N225[, "N225.Adjusted"]))
N225 <- na.omit(N225)
names(N225) = c("N225")
index(N225) = as.Date(index(N225))
logrt.N225 <- apply( log(N225) , 2 , diff )*100
plot(N225,main=names(N225))
acf(N225, lag.max = 30)
pacf(N225, lag.max = 30)
#plot(logrt.N225,main=names(logrt.N225))
tp.N225=tidy(as.xts(logrt.N225)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.N225),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
tp.N225
acf(logrt.N225, lag.max = 30)
pacf(logrt.N225, lag.max = 30)
Box.test(logrt.N225, lag=10, type='Ljung')

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

#巴西Bovespa指數
getSymbols("^BVSP", from ='2012-12-12', to = '2022-12-12')
BVSP <- as.xts(data.frame(BVSP = BVSP[, "BVSP.Adjusted"]))
BVSP <- na.omit(BVSP)
names(BVSP) = c("BVSP")
index(BVSP) = as.Date(index(BVSP))
logrt.BVSP <- apply( log(BVSP) , 2 , diff )*100
plot(BVSP,main=names(BVSP))
acf(BVSP, lag.max = 30)
pacf(BVSP, lag.max = 30)
#plot(logrt.BVSP,main=names(logrt.BVSP))
tp.BVSP=tidy(as.xts(logrt.BVSP)) %>%
  ggplot(aes(x=index,y=value)) + geom_line(color="#E7B800") +geom_hline(yintercept = 0,color='black')+
  labs(title = paste(colnames(logrt.BVSP),"Daily Return for 10 years"),subtitle = "Daily Return",caption = " Time Plot") +
  xlab("Date") + ylab("Daily Return in Percentage") 
tp.BVSP
acf(logrt.BVSP, lag.max = 30)
pacf(logrt.BVSP, lag.max = 30)
Box.test(logrt.BVSP, lag=10, type='Ljung')
