
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

# HP Filter Function #
#HP.Filter <- function(x,lambda=1600){
 # eye <- diag(length(x))
  #result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
  #return(result)
#}
HP.Filter <- function(x,lambda=1600){
  x <- na.omit(x) 
  eye <- diag(length(x))
  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),as.numeric(x))
  result <- xts(result,as.POSIXct(time(x)))
  return(result)
}

#HP.Filter(Data)
#hpf <- HP.Filter(Cl(Data))
#HP.Filter(Cl(Data[2006-01-03/2016-09-30]))
#head(hpf)

.from='2006-01-03'
.to='2016-09-30'

BIST<-xts(coredata(Data),
          as.POSIXct(time(Data)))#Must be POSIXct
BIST<-BIST[paste0(.from,"/",.to)]
head(Cl(BIST))

#hp_bist <- HP.Filter(Cl(BIST))
#str(hp_bist)
#head(hp_bist)
#head(Cl(BIST))


strategy.st = 'project'
portfolio.st = 'project'
account.st = 'project' 

initDate <- "2003-12-07"

initPortf(portfolio.st, symbols='BIST', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq = 1000000)

### quantstrat
initOrders(portfolio.st, initDate=initDate)

### define strategy
strategy(strategy.st, store=TRUE)
summary(get.strategy(strategy.st)) 


#Function
#Omitting Na values
## HP.Filter <- function(x,lambda=1600){
##  x<- na.omit(x)
##  eye <- diag(length(x))
##  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
##  result <- xts(result,as.POSIXct(time(x)))
##  return(result)
## }

#hpf.bist <- HP.Filter(Cl(BIST))

### indicators
.fast = 1600
.slow = 14400

# add.indicator(strategy.st,
#               name = "HP.Filter", #function name
#               arguments = list(
#                 x=quote(HP.Filter(Cl(mktdata),
#                   lambda=.fast))
#               ),
#               label = "HPnFast"
# )
add.indicator(strategy.st,
              name = "HP.Filter", #function name
              arguments = list(
                x=quote(Cl(mktdata)),
                lambda=.fast
              ),
              label = "HPnFast"
)

add.indicator(strategy.st,
              name = "HP.Filter", #function name
              arguments = list(
                x=quote(Cl(mktdata)),
                  lambda=.slow
              ),
              label = "HPnSlow"
)





summary(get.strategy(strategy.st)) 

add.signal(strategy.st,
           name = 'sigCrossover',
           arguments = list(columns=c("HPnFast", "HPnSlow"),
             relationship = "gte"
           ),
           label = 'long'
)

add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("HPnFast", "HPnSlow"),
                            relationship = "lt"),
           label = "short")


summary(get.strategy(strategy.st))

### rules
.orderqty = 1
My_OS_Fnc<-function(timestamp,orderqty,
                    portfolio,symbol,ruletype,...){
  ClosePrice<-as.numeric(Cl(mktdata[timestamp,]))
  orderqty<-round(100000/ClosePrice)
  return(orderqty)
}

add.rule(strategy.st, name='ruleSignal', 
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='market',
                        prefer='Open',
                        
                        TxnFees=0,
                        #orderqty=+.orderqty,
                        osFUN=My_OS_Fnc,
                        orderset='ocolong'
         ),
         type='enter',
         label='EnterLONG'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(
                        
                        sigcol='short', sigval=TRUE,
                        replace=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        TxnFees=0,
                        orderqty='all',
                        orderset='ocolong'
                        
                        
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
.nsamples=20
#.FastSMA = (seq(10,1200,by=50))^2
#.SlowSMA = (seq(1000,2000,by=50))^2

.FastSMA = (seq(10,120,by=10))^2
.SlowSMA = (seq(100,200,by=10))^2


### SMA paramset
add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'HPnFast',
                 variable = list(lambda = .FastSMA),
                 label = 'nFAST'
)
add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'HPnSlow',
                 variable = list(lambda = .SlowSMA),
                 label = 'nSLOW'
)
add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)
###
summary(get.strategy(strategy.st))
save.strategy(strategy.st)


############################

require(doParallel)
registerDoParallel(cores=8)


results <- apply.paramset(strategy.st,
                          paramset.label='SMA',
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








