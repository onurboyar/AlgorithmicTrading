##### Lowes - Crossover Strategy Applied Only to BIST Data
# Author: Onur Boyar

library(zoo)
library(quantmod)
require(quantstrat) 

rm(list = ls())
suppressWarnings(rm(list = ls(envir = .blotter), envir = .blotter))
suppressWarnings(rm(list = ls(envir = .strategy), envir = .strategy))
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 



# Define instruments
currency("USD")
currency("USD")
stock("BIST",currency="USD",multiplier=1)

# Get data
setwd("/Users/boyaronur/Desktop/EC581")
Data<-read.csv(file = "XU100.csv",sep = ";")
Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
names(Data)<-c("Open","High","Low","Close","Volume")

BIST<-xts(coredata(Data),
          as.POSIXct(time(Data)))
BIST<-BIST[paste0(.from,"/",.to)]
head(Cl(BIST))


my.lowess <- function(data, f = 0.1){
  x <- lowess(index(data), Cl(data), f = f)
  t <- index(data)
  z <- x[[2]]
  qxts <- xts(z, order.by=t)
  qxts
}

My_OS_Fnc<-function(timestamp,orderqty,
                    
                    portfolio,symbol,ruletype,...){
  ClosePrice<-as.numeric(Cl(mktdata[timestamp,]))
  orderqty<-round(100000/ClosePrice)
  return(orderqty)
}

.from='2006-01-03'
.to='2016-09-30'


strategy.st = 'lowess'
portfolio.st = 'lowess'
account.st = 'lowess' 

initDate <- "2003-12-07"

initPortf(portfolio.st, symbols='BIST', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq = 1000000)

### quantstrat
initOrders(portfolio.st, initDate=initDate)

### define strategy
strategy(strategy.st, store=TRUE)
summary(get.strategy(strategy.st)) 

### indicators
f_fast = 0.25
f_slow = 0.5
add.indicator(strategy.st,
              name = "my.lowess", #function name
              arguments = list(
                data = quote(mktdata),
                f=f_fast),
              label = "LWSnFast"
)

add.indicator(strategy.st,
              name = "my.lowess", #function name
              arguments = list(
                data = quote(mktdata),
                f=f_slow),
              label = "LWSnSlow"
)


summary(get.strategy(strategy.st)) 

# signals
add.signal(strategy.st,
           name = 'sigCrossover',
           arguments = list(columns=c("X1.LWSnFast", "X1.LWSnSlow"),
                            relationship = "gte"
           ),
           label = 'long'
)

add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("X1.LWSnFast", "X1.LWSnSlow"),
                            relationship = "lt"),
           label = "short")



summary(get.strategy(strategy.st))


add.rule(strategy.st, #
         name='ruleSignal', 
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' , 
                        ordertype='market', 
                        prefer='Open', 
                        replace=FALSE,
                        osFUN = 'My_OS_Fnc'
         ),
         type='enter', 
         label='EnterLONG'
         
         
)


add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, 
                        orderside='long' , 
                        ordertype='market', 
                        orderqty='all', 
                        replace=TRUE,
                        osFUN = 'My_OS_Fnc'
         ),
         type='exit', # cikis
         label='Exit2SHORT' # rule'un ismi
)

# # Apply strategy
applyStrategy(strategy.st, portfolio.st)
# 
# # Updates
updatePortf(portfolio.st, Symbols='BIST')
updateAcct(account.st)
updateEndEq(account.st)
# 
# # Analyze indicators, signals, orders, txns
# View(mktdata)
View(getOrderBook(portfolio.st)[[portfolio.st]]$'BIST')
View(t(tradeStats(portfolio.st, 'BIST')))
View(perTradeStats(portfolio.st))
# 
# # MFE and MAE charts
par(mfrow = c(1,1))
chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')

 # 
# # Analyze portfolio object
myPort <- getPortfolio(portfolio.st)
names(myPort)

names(myPort$symbols)
names(myPort$symbols$BIST)
head(myPort$symbols$BIST$txn)
head(myPort$symbols$BIST$posPL.USD)
head(myPort$symbols$BIST$posPL)

names(myPort$summary)
# 
# library(lattice)
plot(xyplot(myPort$summary,xlab="",type="h",col=4))
# 
# ###############################################################################
# # Perf chart and equity
chart.Posn(portfolio.st, "BIST")
# 
Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))
# ###############################################################################
# # save the strategy in an .RData object for later retrieval
# save.strategy(strategy.st)


##################################
# OPTIMIZATION
##################################


### Distributions for paramset analysis
.nsamples=10
.f_fast = c(0.01, 0.03, 0.06, 0.09, 0.15, 0.25, 0.38, 0.5, 0.7, 0.85, 1)
.f_slow = c(0.01, 0.03, 0.06, 0.09, 0.15, 0.25, 0.38, 0.5, 0.7, 0.85, 1)



add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'LWSnFast',
                 variable = list(f = .f_fast),
                 label = 'F_Fast')

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'LWSnSlow',
                 variable = list(f = .f_slow),
                 label = 'F_Slow')

add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'F_Fast',
                            distribution.label.2 = 'F_Slow',
                            operator = '<',
                            label = 'SMA'
)

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

stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)








