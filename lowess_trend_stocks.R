##### Lowes - Trend Direction Strategy applied for different stocks
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
stocks=c("XU100","TTKOM","KRDMD","TUPRS","GARAN","FROTO","MGROS")
stock(stocks,currency="USD",multiplier=1)
count=1
setwd("/Users/boyaronur/Desktop/EC581/BIST")

while(count<=length(stocks)){
  a=stocks
  n=paste(stocks[count],".csv",sep="")
  Data<-read.csv(file = n,sep = ";")
  Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
  names(Data)<-c("Open","High","Low","Close","Volume")
  
  assign((a[count]),na.omit(xts(coredata(Data),
                                as.POSIXct(time(Data))))[paste0(.from,"/",.to)])#Must be POSIXct
  
  count=count+1
}



my.lowess <- function(data = mktdata, f = 0.1){
  x <- lowess(index(data), Cl(data), f = 0.3)
  t <- index(data)
  z <- x[[2]]
  qxts <- xts(z, order.by=t)
  qxts
  Returns <- diff(log(qxts))
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

initPortf(portfolio.st, symbols=stocks, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq = 1000000)

### quantstrat
initOrders(portfolio.st, initDate=initDate)

### define strategy
strategy(strategy.st, store=TRUE)
summary(get.strategy(strategy.st)) 



f = 0.38
.th_1=10
.th_2=-4

add.indicator(strategy.st, 
              name = "my.lowess", 
              arguments = list(
                x = quote(mktdata),f), 
              label = "trend_up")


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

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='market',
                        osFUN='My_OS_Fnc',
                        orderset='ocolong',
                        prefer = 'Open'
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
                        replace=TRUE,
                        orderset='ocolong',
                        osFUN = 'My_OS_Fnc'
         ),
         type='exit',
         label='Exit2SHORT'
)



# # Apply strategy
applyStrategy(strategy.st, portfolio.st)
# 
# # Updates
 updatePortf(portfolio.st, Symbols=stocks)
 updateAcct(account.st)
 updateEndEq(account.st)
# 
getEndEq(account.st, Sys.Date()) - x
# # Analyze indicators, signals, orders, txns
View(mktdata)
View(getOrderBook(portfolio.st)[[portfolio.st]]$BIST)
 View(t(tradeStats(portfolio.st, stocks)))
# View(perTradeStats(portfolio.st))
# 
# # MFE and MAE charts
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

library(lattice)
plot(xyplot(myPort$summary,xlab="",type="h",col=4))
# 
# ###############################################################################
# # Perf chart and equity
chart.Posn(portfolio.st, "BIST")

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))
###############################################################################
# save the strategy in an .RData object for later retrieval
save.strategy(strategy.st)


##################################
# OPTIMIZATION
##################################

.nsamples=10 
.up_d = (1:10)
.down_d = (-1:-10) 
.f_val = c(0.01, 0.03, 0.06, 0.09, 0.15, 0.25, 0.38, 0.5, 0.7, 0.85, 1)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'trend_up',
                 variable = list(f = .f_val),
                 label = 'F_Fast')

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'signal', 
                 component.label = 'long',
                 variable = list(threshold = .up_d), 
                 label = 'up' 
)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'signal',
                 component.label = 'short',
                 variable = list(threshold = .down_d), 
                 label = 'down'
)
add.distribution.constraint(strategy.st, 
                            paramset.label = 'SMA',
                            distribution.label.1 = 'up',
                            distribution.label.2 = 'down',
                            operator = '>', 
                            label = 'myfunc'
)

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








