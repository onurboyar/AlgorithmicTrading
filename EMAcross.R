################################
###   BASE STRATEGY
################################




#kendi directorynizi set edin !!!
# setwd("~/Desktop/Ayhan Y??ksel Proje")


#Load libraries
library(quantstrat)
library(Quandl)

ls()
ls(all=T) #.blotter and .strategy environments added
class(.blotter)

# Define instruments
currency("USD")


# Get data
.from='2006-01-03'
.to='2016-09-30'


#csv dosyalar??n?? alip xts'e ceviriyoruz.
stocks=c("TTKOM","KRDMD","TUPRS","GARAN","FROTO","XU100","MGROS")
stock(stocks,currency="USD",multiplier=1)
count=1

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

# Data<-read.csv(file = "GARAN.csv",sep = ";")
# Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
# names(Data)<-c("Open","High","Low","Close","Volume")
# GARAN<-xts(coredata(Data),
#           as.POSIXct(time(Data)))#Must be POSIXct
# GARAN<-na.omit(GARAN)
# GARAN<-GARAN[paste0(.from,"/",.to)]

# Define strategy component names
strategy.st = 'ReturnRun_MeanReversion'
portfolio.st = 'myPortfolio'
account.st = 'AkInvestment'

# If you removed all objects from the global environment,
# then you may need to recreate .blotter and .strategy environments
#.blotter<-new.env()
#.strategy<-new.env()

# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 


# Initialize portfolio&account in .blotter, 
# and orderbook&strategy in .strategy environments
initDate<-as.character(as.Date(.from)-1) # One day before data starts
initEq<-1000000

initPortf(portfolio.st, 
          symbols=stocks, 
          initDate=initDate, 
          currency='USD')
initAcct(account.st, 
         portfolios=portfolio.st, 
         initDate=initDate, 
         currency='USD',
         initEq=initEq)
initOrders(portfolio.st, 
           initDate=initDate)
strategy(strategy.st, 
         store=TRUE)

# Add position limit
addPosLimit(portfolio.st, stocks, timestamp=initDate, maxpos=10, minpos=0)


My_OS_Fnc<-function(timestamp,orderqty,
                    
                    portfolio,symbol,ruletype,...){
  ClosePrice<-as.numeric(Cl(mktdata[timestamp,]))
  orderqty<-round(100000/ClosePrice)
  return(orderqty)
}




.fast = 1
.slow = 120

add.indicator(strategy.st, 
              name = "EMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="EMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)









# Add signals
add.signal(strategy.st, 
           name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)


# Add rules (i.e. when to send orders)
add.rule(strategy.st, 
         name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market', 
                        prefer='Open', 
                      
                        osFUN="My_OS_Fnc",
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        prefer='Open', 
                        orderqty='all',
                        osFUN="My_OS_Fnc",
                        replace=TRUE #Replace any pending open orders
         ),
         type='exit',
         label='ExitLONG'
)



# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Update portfolio & account
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)



# Save end equity for future use
saveRDS(BuyHold,"BH.rds")
saveRDS(Eq,"GC_10_20.rds")

################################
###   OPTIMIZE PARAMETERS
################################


# Define parameter space 
.FastEMA = c(1,5,10,15,20,50,70)
.SlowEMA = c(60,90,120,150,200,250,350)

add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastEMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowEMA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'EMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'EMA'
)





# Use nsamples if you want random samples from the parameter space
results <- apply.paramset(strategy.st, 
                          paramset.label='EMA', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          nsamples = 3,
                          verbose=TRUE)

stats <- results$tradeStats
View(stats)
names(stats)


par(mfrow = c(1,2))
##
stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$nFAST[1:10],stats$nSLOW[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best Ann.Sharpe Ratio ")


stats <- stats[order(stats$Ann.Sharpe, decreasing = F),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$nFAST[1:10],stats$nSLOW[1:10], sep="/"),
        las=2,cex.names=0.75)

title("Worst Ann.Sharpe Ratio")

###
stats <- stats[order(stats$End.Equity, decreasing = T),]

barplot(stats$End.Equity[1:10],
        names.arg=paste(stats$nFAST[1:10],stats$nSLOW[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best End.Equity ")


stats <- stats[order(stats$End.Equity, decreasing = F),]

barplot(stats$End.Equity[1:10],
        names.arg=paste(stats$nFAST[1:10],stats$nSLOW[1:10], sep="/"),
        las=2,cex.names=0.75)

title("Worst End.Equity")

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$nFAST[1:10],stats$nSLOW[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best Profit.To.Max.Draw")


stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = F),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$nFAST[1:10],stats$nSLOW[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Worst Profit.To.Max.Draw")

ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, color = Symbol), size =5)+ggtitle("Ann.Sharpe vs. Profit.To.Max.Draw")




