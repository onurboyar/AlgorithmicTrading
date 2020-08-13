
library(tidyverse)
library(zoo)
library(quantmod)
require(quantstrat) 
Sys.setenv(TZ="Europe/Istanbul") 

# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 



# Define instruments
currency("USD")
stock("BIST",currency="USD",multiplier=1)

# Get data
setwd("/Users/boyaronur/Desktop/EC581")
Data<-read.csv(file = "XU100.csv",sep = ";")
Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
names(Data)<-c("Open","High","Low","Close","Volume")


.from='2006-01-03'
.to='2016-09-30'

BIST<-xts(coredata(Data),
          as.POSIXct(time(Data)))#Must be POSIXct
BIST<-BIST[paste0(.from,"/",.to)]
head(Cl(BIST))

# HP Filter Trend Function #

HP.Filter <- function(x,lambda=1600){
  x <- na.omit(x) 
  eye <- diag(length(x))
  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),as.numeric(x))
  result <- xts(result,as.POSIXct(time(x)))
  #return(result)
  #trend hesab? i?in yeni kod eklenir
  Returns <- diff(log(abs(result)))
  trends <- Returns
  trends[1] <- 0
  trends[2] <- 1
  # first two rows are 0 and 1.
  for(i in 3:length(Returns)){
    trends[i] <-  ifelse(as.numeric(sign(Returns[i])) ==
                           as.numeric(sign(Returns[i-1])), as.numeric(trends[i-1])
                         + as.numeric(sign(Returns[i])),  sign(Returns[i]))
  }
  trends
  return(trends)
}



strategy.st = 'hpfilter_direction'
portfolio.st = 'hpfilter_direction'
account.st = 'hpfilter_direction' 

initDate <- "2003-12-07"

initPortf(portfolio.st, symbols='BIST', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq = 1000000)

### quantstrat
initOrders(portfolio.st, initDate=initDate)

### define strategy
strategy(strategy.st, store=TRUE)
summary(get.strategy(strategy.st)) 



### indicators
.lambda = 1600
.th_1=3
.th_2=-3

add.indicator(strategy.st,
              name = "HP.Filter", #function name
              arguments = list(
                x=quote(Cl(mktdata)),
                  lambda=.lambda
              ),
              label = "trend_up"
)

summary(get.strategy(strategy.st)) 

#Signals

add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             threshold = .th_1,
             relationship="eq",
             column = "trend_up"
           ),
           label='long'
)

add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             threshold = .th_2,
             relationship="eq",
             column = "trend_up"
           ),
           label='short'
)


summary(get.strategy(strategy.st))

### rules
.orderqty = 1
.threshold = 0.0005
.txnfees = 0 # round-trip fee

My_OS_Fnc<-function(timestamp,orderqty,
                    portfolio,symbol,ruletype,...){
  ClosePrice<-as.numeric(Cl(mktdata[timestamp,]))
  orderqty<-round(100000/ClosePrice)
  return(orderqty)
}

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='market',
                        TxnFees=.txnfees,
                        orderqty=+.orderqty,
                        osFUN='My_OS_Fnc',
                        orderset='ocolong'
         ),
         type='enter',
         label='EnterLONG'
)


add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short',
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE,
                        orderset='ocolong',
                        osFUN = 'My_OS_Fnc'
         ),
         type='exit',
         label='Exit2SHORT'
)
###############################################################################
# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Updates
updatePortf(portfolio.st, Symbols='BIST')
updateAcct(account.st)
updateEndEq(account.st)

# Analyze indicators, signals, orders, txns
View(mktdata)
View(getOrderBook(portfolio.st)[[portfolio.st]]$BIST)
View(t(tradeStats(portfolio.st, 'BIST')))
View(perTradeStats(portfolio.st))

# MFE and MAE charts
chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')

# Analyze portfolio object
myPort <- getPortfolio(portfolio.st)
names(myPort)

names(myPort$symbols)
names(myPort$symbols$BIST)
head(myPort$symbols$BIST$txn)
head(myPort$symbols$BIST$posPL.USD)
head(myPort$symbols$BIST$posPL)

names(myPort$summary)

library(lattice)
plot(xyplot(myPort$summary,xlab="",type="h",col=4))

###############################################################################
# Perf chart and equity
chart.Posn(portfolio.st, "BIST")

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))
###############################################################################
# save the strategy in an .RData object for later retrieval
save.strategy(strategy.st)


##################################
# OPTIMIZATION
##################################


### Distributions for paramset analysis
.nsamples=10 
.up_d = (1:10)
.down_d = (-1:-10)
.lambdaHP = (seq(10,200,by=10))^2


### SMA paramset
add.distribution(strategy.st,
                 paramset.label = 'HP',
                 component.type = 'indicator',
                 component.label = 'trend_up',
                 variable = list(lambda = .lambdaHP),
                 label = 'HPFAST'
)
add.distribution(strategy.st,
                 paramset.label = 'HP',
                 component.type = 'signal',
                 component.label = 'long',
                 variable = list(lambda = .up_d),
                 label = 'up'
)
add.distribution(strategy.st,
                 paramset.label = 'HP',
                 component.type = 'signal',
                 component.label = 'short',
                 variable = list(threshold = .down_d), 
                 label = 'down'
)
add.distribution.constraint(strategy.st, 
                            paramset.label = 'HP',
                            distribution.label.1 = 'up',
                            distribution.label.2 = 'down',
                            operator = '>', 
                            label = 'myfunc'
)
###
summary(get.strategy(strategy.st))
save.strategy(strategy.st)


############################

require(doParallel)
registerDoParallel(cores=8)


results <- apply.paramset(strategy.st,
                          paramset.label='HP',
                          portfolio.st=portfolio.st,
                          account.st=account.st,
                          nsamples=.nsamples,
                          verbose=TRUE)
stats <- results$tradeStats
View(t(stats))
plot(stats$Profit.Factor,
     stats$Net.Trading.PL,
     xlab='Profit Factor',
     ylab='Net.Trading.PL',
     main='Luxor')
barplot(stats$Profit.To.Max.Draw,
        names.arg=paste(stats$nFAST,stats$nSLOW,sep="/"),
        las=2,cex.names=0.75)








