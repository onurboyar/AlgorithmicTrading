##### Different type of plots are tried in this plot.
# This code file has trial and error plots and data.
# It mainly uses stats object obtained from results$tradeStats
#Author: Onur Boyar


hp0 <- HP.Filter(Cl(BIST), 0)
#hp10 <- HP.Filter(Cl(BIST),10)
hp100 <- HP.Filter(Cl(BIST),100)
hp1600 <- HP.Filter(Cl(BIST), 1600)
hp3600 <- HP.Filter(Cl(BIST), 3600)

comparison = c(Cl(BIST), hp3600)
plot(comparison, 
     plot.type = "single", 
     col = c("brown", "black"))

plot(hp100)
#install.packages('tseries')
library("tseries")    
comb_ts <- cbind(Cl(BIST), hp3600) # please make sure the length of both your timeseries
plot.ts(comb_ts, plot.type = "single")


par(mfrow = c(1,2))

stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)

stats <- stats[order(stats$Ann.Sharpe, decreasing = F),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = F),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)


ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, color = Symbol), size =5) #+ xlim(c(10,30))

stats <- stats[order(stats$Avg.WinLoss.Ratio, decreasing = T),]

barplot(stats$Avg.WinLoss.Ratio[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)

stats <- stats[order(stats$Med.WinLoss.Ratio, decreasing = T),]

barplot(stats$Med.WinLoss.Ratio[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)

# burada en iyi deger hangisi onu konus

chart.Posn(portfolio.st, "BIST")
#  
Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(Eq)

chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')


stats$ratio <- stats$F_Fast / stats$F_Slow
ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = ratio, color = Symbol), size =5) #+ xlim(c(10,30))
ggplot(stats) + geom_point(aes(y = Profit.To.Max.Draw, x = ratio, color = Symbol), size =5) #+ xlim(c(10,30))


sharpe_groups_mean = stats %>% group_by(Symbol) %>% summarise(mean = mean(na.omit(Ann.Sharpe)))
sharpe_groups_median = stats %>% group_by(Symbol) %>% summarise(median = median(na.omit(Ann.Sharpe)))
p.max.dr_mean = stats %>% group_by(Symbol) %>% summarise(mean = mean(na.omit(Profit.To.Max.Draw)))
p.max.dr_median = stats %>% group_by(Symbol) %>% summarise(median = median(na.omit(Profit.To.Max.Draw)))
ending_mean = stats %>% group_by(Symbol) %>% summarise(mean = mean(End.Equity))
average.min_loss = stats %>% group_by(Symbol) %>% summarise(mean = mean(Avg.WinLoss.Ratio))
med.min_loss = stats %>% group_by(Symbol) %>% summarise(mean = mean(Med.WinLoss.Ratio))



p1 <- ggplot(sharpe_groups_mean) + geom_bar(aes(x = Symbol, y = mean, fill = Symbol), stat = 'identity') + 
  theme(legend.position = "none") + ggtitle('Sharpe Ratio Mean Values') + xlab('Stock Type') +
  ylab('Mean Value')
p2 <- ggplot(p.max.dr_mean) + geom_bar(aes(x = Symbol, y = mean, fill = Symbol), stat = 'identity') +
  ggtitle('Profit to Max Drawdown Mean Values')+ xlab('Stock Type') +
  ylab('Mean Value')
p2

library(ggpubr)
ggarrange(p1, p2)

sharpe_groups_max = stats %>% group_by(Symbol) %>% summarise(max = max(na.omit(Ann.Sharpe)))
sharpe_groups_min = stats %>% group_by(Symbol) %>% summarise(min = min(na.omit(Ann.Sharpe)))

sharpe_sd = stats %>% group_by(Symbol) %>% summarise(sd = sd(na.omit(Ann.Sharpe)))
p.mdraw_sd = stats %>% group_by(Symbol) %>% summarise(sd = sd(na.omit(Profit.To.Max.Draw)))

p.mdraw_max = stats %>% group_by(Symbol) %>% summarise(max = max(na.omit(Profit.To.Max.Draw)))
p.mdraw_min = stats %>% group_by(Symbol) %>% summarise(min = min(na.omit(Profit.To.Max.Draw)))




par(mfrow = c(1,2))

stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]
# her grup icin en yuksek 10 Ann.Sharpe Geldi
x <- stats %>%
  group_by(Symbol) %>%
  top_n(10, Ann.Sharpe) 

stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:5],
        names.arg=paste(round(stats$Profit.To.Max.Draw[1:5],2)),
        las=2,cex.names=0.75)

title(main = list("Maximum and Minumum Sharpe Ratios with Diferent F Values", font = 4))

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:5],
        names.arg=paste(round(stats$Ann.Sharpe[1:5], 2)),
        las=2,cex.names=0.75)

### burada 0.1/0.4 hem cok iyi hem cok kotu. bir hisse icin
# iyi sonuc verirken digeri icin cok kotu veriyor. onlar hangileri?

f_vals <- stats[stats$F_Fast == 0.1 & stats$F_Slow == 0.4,]
f_vals[which.min(f_vals$Ann.Sharpe),]$Symbol
f_vals[which.max(f_vals$Ann.Sharpe),]$Symbol

comparison = cbind(Cl(MGROS), Cl(TTKOM), my.lowess(MGROS, 0.1), my.lowess(MGROS, 0.4))
plot(comparison, 
     plot.type = "single", 
     col = c("red", "blue", "black", "brown"))

#title(main = list("Min Sharpe Ratios with Different F Values", font = 4))


# Kardemir icin lowess'li degerler ve gercek degerin gorseli

stats_krd = stats[stats$Symbol == 'KRDMD',]
# 0.1, 0.2
krdmd.fast <- my.lowess(KRDMD, 0.1)
krdmd.slow <- my.lowess(KRDMD, 0.2)
plot(krdmd.fast)
comparison = cbind(Cl(KRDMD), krdmd.fast, krdmd.slow)
plot(comparison, 
         plot.type = "single", 
         col = c("gray","red", "blue"))
## simple_lwess icin en iyi f degerlerinden gelen
# smoothed grafik, ve orjinal deger ust uste!!!



#############################################################
### 

# burada her grup icin en iyi 5 sharpe, 5 P.Max Drawdown, Grup bazli
# Sonra da genel.

x_sharpe <- stats %>%
  group_by(Symbol) %>%
  top_n(5, Ann.Sharpe)

x_p.max <- stats %>%
  group_by(Symbol) %>%
  top_n(5, Profit.To.Max.Draw)

ggplot(x_p.max) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, color = Symbol), size =5) #+ xlim(c(10,30))

barplot(stats[stats$Symbol == 'TTKOM',]$Ann.Sharpe[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=0.75)

sharpe_groups_max = stats %>% group_by(Symbol) %>% summarise(max = max(na.omit(Ann.Sharpe)))
sharpe_groups_min = stats %>% group_by(Symbol) %>% summarise(min = min(na.omit(Ann.Sharpe)))

sharpe_sd = stats %>% group_by(Symbol) %>% summarise(sd = sd(na.omit(Ann.Sharpe)))
p.mdraw_sd = stats %>% group_by(Symbol) %>% summarise(sd = sd(na.omit(Profit.To.Max.Draw)))

p.mdraw_max = stats %>% group_by(Symbol) %>% summarise(max = max(na.omit(Profit.To.Max.Draw)))
p.mdraw_min = stats %>% group_by(Symbol) %>% summarise(min = min(na.omit(Profit.To.Max.Draw)))


