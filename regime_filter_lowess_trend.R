#### Code For Regime Filtering

#### HOW TO RUN THIS CODE ######

# In this code, results are obtained for each stock INDIVIDUALLY!
# First, read MGROS.csv, in example
# Then, apply paramset WITHOUT RUNNING FOLLOWING CODES INSIDE CURLY BRACKETS
#{my.lowess_2 function, 
# the lines 78-89, myfunc function and the signal 'sig4'., ruleSignals for long.sig4}
# obtain mktdata. (this is no different than applying lowess_trend strategy)
# to stock data.
# use mktdata to obtain regime filter signal.
# TO OBTAIN REGIME FILTER;
# RUN my.lowess_2 function
# run the code in line number 78 to 89.
# run myfunc
# add signal sig4
# add.rule for long.sing4, do not run add.rule functions labeled
# EnterLONG and Exit2SHORT this time.
# apply paramset and get results.
# Repeat the same process for each stock.

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
stock("BIST",currency="USD",multiplier=1)

# Get data
setwd("/Users/boyaronur/Desktop/EC581/BIST")
Data<-read.csv(file = "XU100.csv",sep = ";")
Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
names(Data)<-c("Open","High","Low","Close","Volume")

my.lowess_2 <- function(data, f = 0.1){
  x <- lowess(index(data), Cl(data), f = f)
  t <- index(data)
  z <- x[[2]]
  qxts <- xts(z, order.by=t)
  qxts
}


my.lowess <- function(data, f = 0.1){
  x <- lowess(index(BIST), Cl(BIST), f = 0.3)
  t <- index(BIST)
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
                           as.numeric(sign(Returns[i-1])), 
                         as.numeric(trends[i-1])
                         + as.numeric(sign(Returns[i])),  
                         sign(Returns[i]))
  }
  trends
  }
###### CODE CHUNK 78-89 STARTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
LWS <- my.lowess_2(Cl(mktdata), 0.38)
mktdata$X1.LWSnFast = LWS
sig1 <- ifelse(mktdata[,'Close'] > mktdata[,'X1.LWSnFast'], 1, -1)
mktdata[,'long'] <- ifelse(is.na(mktdata[,'long']), 0, mktdata[,'long'])
mktdata[,'short'] <- ifelse(is.na(mktdata[,'short']), 0, mktdata[,'short'])
sig2 <- (mktdata[,'long'] == 1) * 1
sig3 <- (mktdata[,'short'] == 1) * 1
sig3 <- ifelse(sig3 == 1, -1, 0)
sig4 <- sig2 + sig3
sig4 <- ifelse(sig4 > 1, 1, sig4)
sig4 <- ifelse(sig4 < 0, -1, sig4)
sig4
######## CODE 89 ENDS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

myfunc <- function(data){
  sig4
}


My_OS_Fnc<-function(timestamp,orderqty,
                    
                    portfolio,symbol,ruletype,...){
  ClosePrice<-as.numeric(Cl(mktdata[timestamp,]))
  orderqty<-round(100000/ClosePrice)
  return(orderqty)
}

.from='2006-01-03'
.to='2016-09-30'

BIST<-xts(coredata(Data),
          as.POSIXct(time(Data)))
BIST<-BIST[paste0(.from,"/",.to)]
head(Cl(BIST))

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
#### DO NOT ADD THIS SIGNAL IN THE FIRST STEP
### ADD IT AFTER OBTAINING MKTDATA FROM TREND DIRECTION STRATEGY
add.signal(strategy.st,name="myfunc",
           arguments = list(quote(mktdata)),
           label="sig4"
)


summary(get.strategy(strategy.st))

##### ADD FIRST TWO RULES IN THE REGIME FILTERING STEP ONLY!!!!!
#### DO NOT ADD THEM IN TREND DIRECTION STEP (FIRST STEP)
add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="long.sig4",
                          sigval=TRUE,
                          #orderqty=100,
                          ordertype='market',
                          orderside='long',
                          threshold=NULL,
                          osFUN = 'My_OS_Fnc'),
         type='enter',
         label='enter',
         storefun=FALSE
)

add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "long.sig4",
                          sigval = TRUE,
                          orderqty = 'all',
                          ordertype = 'market',
                          orderside = NULL,
                          threshold = NULL,
                          osFUN = 'My_OS_Fnc'),
         label = 'exitMid',
         type = 'exit')
##### SECOND RULE ENDS HERE !!!!!!!!!!


add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long' ,
                        ordertype='market',
                        TxnFees=.txnfees,
                        #orderqty=+.orderqty,
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
                        TxnFees=.txnfees,
                        replace=TRUE,
                        orderset='ocolong',
                        osFUN = 'My_OS_Fnc'
         ),
         type='exit',
         label='Exit2SHORT'
)

#




##########################################################

### TYPE CTRL SHIFT C TO UNCOMMENT

# # Apply strategy
applyStrategy(strategy.st, portfolio.st)
# 
# # Updates
updatePortf(portfolio.st, Symbols='BIST')
updateAcct(account.st)
updateEndEq(account.st)
#initial <- getEndEq(account.st, Sys.Date())
#TTKOM_Regime <- getEndEq(account.st, Sys.Date()) # 1 trade
#TUPRS_Regime <- getEndEq(account.st, Sys.Date()) # 1 trade
#KRDMD_Regime <- getEndEq(account.st, Sys.Date()) # 1trade
#GARAN_Regime <- getEndEq(account.st, Sys.Date()) # 1 al sat, 1 al
#FROTO_Regime <- getEndEq(account.st, Sys.Date()) # 1 al sat
#MGROS_Regime <- getEndEq(account.st, Sys.Date()) # 1 al sat
#BIST_Regime <- getEndEq(account.st, Sys.Date()) # 1 al sat

# 
# # Analyze indicators, signals, orders, txns
# View(mktdata)
View(getOrderBook(portfolio.st)[[portfolio.st]]$BIST)
View(t(tradeStats(portfolio.st, 'BIST')))
View(perTradeStats(portfolio.st))
# 
# # MFE and MAE charts
chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')
# 
# # Analyze portfolio object
myPort <- getPortfolio(portfolio.st)
# names(myPort)
# 
# names(myPort$symbols)
# names(myPort$symbols$BIST)
# head(myPort$symbols$BIST$txn)
head(myPort$symbols$BIST$posPL.USD)
# head(myPort$symbols$BIST$posPL)
# 
# names(myPort$summary)
# 
# library(lattice)
# plot(xyplot(myPort$summary,xlab="",type="h",col=4))
# 
# ###############################################################################
# # Perf chart and equity
# chart.Posn(portfolio.st, "BIST")
# 
# Eq<-getAccount(account.st)$summary[,"End.Eq"]
# plot(as.zoo(Eq))
# ###############################################################################
# # save the strategy in an .RData object for later retrieval
# save.strategy(strategy.st)











