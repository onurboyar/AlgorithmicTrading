################################
###   BASE STRATEGY
################################

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

stocks=c("XU100","TTKOM","KRDMD","TUPRS","GARAN","FROTO","MGROS")
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
# Data<-read.csv(file = "XU100.csv",sep = ";")
# Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
# names(Data)<-c("Open","High","Low","Close","Volume")
# 
# 
# 
# 
# BIST<-xts(coredata(Data),
#           as.POSIXct(time(Data)))#Must be POSIXct
# BIST<-BIST[paste0(.from,"/",.to)]

My_OS_Fnc<-function(timestamp,orderqty,
                    
                    portfolio,symbol,ruletype,...){
  ClosePrice<-as.numeric(Cl(mktdata[timestamp,]))
  orderqty<-round(100000/ClosePrice)
  return(orderqty)
}
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





# Add indicators
ReturnRun<-function(Prices,n){
  Ret<-diff(log(EMA(Prices,n)))
  Ind<-NA*Ret
  Ind[n+1]<-sign(Ret[n+1])
  
  for (i in (n+2):length(Ind)){
    if (as.numeric(sign(Ret[i]))!=as.numeric(sign(Ret[i-1]))){
      Ind[i]<-sign(Ret[i])
    } else {
      Ind[i]<-sign(Ret[i])*(abs(as.numeric(Ind[i-1]))+1)
    }
  }
  colnames(Ind)<-"ReturnRun"
  Ind
}

add.indicator(strategy.st, 
              name = "ReturnRun",
              arguments = list(
                Prices = quote(Cl(mktdata)),
                n=150
              ),
              label="Runs"
)








# Add signals
add.signal(strategy.st, 
           name='sigThreshold',
           arguments = list(
             column="Runs",
             relationship="lte",
             threshold=-4,
             cross=T
           ),
           label='short'
)

add.signal(strategy.st, 
           name='sigThreshold',
           arguments = list(
             column="Runs",
             relationship="gte",
             threshold=6,
             cross=T
           ),
           label='long'
)

# Add rules (i.e. when to send orders)
add.rule(strategy.st, 
         name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market', 
                        prefer='Open', 
                        orderqty=1,
                        osFUN=My_OS_Fnc,
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

View(t(tradeStats(portfolio.st,stocks)))
# Analyze performance


# Save results 
Acc<-get("account.AkInvestment",envir = .blotter)
Eq<-Acc$summary$End.Eq
saveRDS(Eq,"MR_2_10.rds")


##########
#OPTIMIZATION

N.Range<- c(2:6)
M.Range<- -c(2:5)
day.range=c(10,30,60,100,150,200,250,350)
add.distribution(strategy.st,
                 paramset.label = 'ParamSet',
                 component.type = 'signal',
                 component.label = 'long',
                 variable = list(threshold = N.Range),
                 label = 'up'
)

add.distribution(strategy.st,
                 paramset.label = 'ParamSet',
                 component.type = 'signal',
                 component.label = 'short',
                 variable = list(threshold = M.Range),
                 label = 'down'
)
add.distribution(strategy.st,
                 paramset.label = 'ParamSet',
                 component.type = 'indicator',
                 component.label = 'Runs',
                 variable = list(n=day.range),
                 label = 'emaDays'
)

# Apply parameter optimization
# library(doParallel)
# detectCores()
# registerDoParallel(cores=3) # Parallel computing

results<-apply.paramset(strategy.st, paramset.label='ParamSet',
                        portfolio.st=portfolio.st, account.st=account.st, nsamples=10)



stats <- results$tradeStats
View(stats)
names(stats)

library(tidyverse)
par(mfrow = c(1,2))
##
stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best Ann.Sharpe Ratio ")


stats <- stats[order(stats$Ann.Sharpe, decreasing = F),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)

title("Worst Ann.Sharpe Ratio")

###
stats <- stats[order(stats$End.Equity, decreasing = T),]

barplot(stats$End.Equity[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best End.Equity ")


stats <- stats[order(stats$End.Equity, decreasing = F),]

barplot(stats$End.Equity[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)

title("Worst End.Equity")

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best Profit.To.Max.Draw")


stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = F),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Worst Profit.To.Max.Draw")


ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, color = Symbol), size =5)+ggtitle("Ann.Sharpe vs. Profit.To.Max.Draw")


stats <- stats[order(stats$Avg.WinLoss.Ratio, decreasing = T),]

barplot(stats$Avg.WinLoss.Ratio[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Avg.WinLoss.Ratio")


stats <- stats[order(stats$Med.WinLoss.Ratio, decreasing = T),]

barplot(stats$Med.WinLoss.Ratio[1:10],
        names.arg=paste(stats$emaDays[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Med.WinLoss.Ratio")






