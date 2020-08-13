

#hp0 <- HP.Filter(Cl(BIST), 0)
#hp100 <- HP.Filter(Cl(BIST),100)
hp1600 <- HP.Filter(Cl(BIST), 1600)
#hp6400 <- HP.Filter(Cl(BIST), 6400)
hp14400 <- HP.Filter(Cl(BIST), 14400)
hp25600 <- HP.Filter(Cl(BIST),25600)
hp40000 <- HP.Filter(Cl(BIST),40000)
hp1000000 <- HP.Filter(Cl(BIST),1000000)


par(mfrow = c(1,1))


comparison = cbind(Cl(BIST), hp1600, hp14400, hp25600, hp40000,hp1000000)
plot(comparison, 
     plot.type = "single", 
     col = c("black", "blue", "red","purple","yellow","green"))
title("comparison")


### Visualizations for crossover lowess
# BIST Gorselleri
#stats <- read.csv("stats_bad.csv")
#stats <- read.csv("stats.csv")
stats <- read.csv("stats_direction.csv")

par(mfrow = c(1,2))

stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best Ann. Sharpe Ratios")


stats <- stats[order(stats$Ann.Sharpe, decreasing = F),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)

title("Worst Ann. Sharpe Ratios")

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Best Profit.To.Max.Draw")


stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = F),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Worst Profit.To.Max.Draw")


ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, color = Symbol), size =5)+ggtitle("Ann.Sharpe vs. Profit.To.Max.Draw")


stats <- stats[order(stats$Avg.WinLoss.Ratio, decreasing = T),]

barplot(stats$Avg.WinLoss.Ratio[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Avg.WinLoss.Ratio")


stats <- stats[order(stats$Med.WinLoss.Ratio, decreasing = T),]

barplot(stats$Med.WinLoss.Ratio[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)
title("Med.WinLoss.Ratio")


# burada en iyi deger hangisi onu konus


par(mfrow=c(1,2))
chart.Posn(portfolio.st, "BIST")
title("Pstn with 14400,1,-1")


# 
Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(Eq)
title("Equity with 14400,1,-1")


chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')


stats$fast_slow_n_ratio <- stats$FAST / stats$nSLOW
ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = fast_slow_n_ratio, color = Symbol), size =5) + ggtitle("Ann.Sharpe vs. fast to slow lambda ratio ")
ggplot(stats) + geom_point(aes(y = Profit.To.Max.Draw, x = fast_slow_n_ratio, color = Symbol), size =5) +ggtitle("Profit.To.Max.Draw vs. fast to slow lambda ratio")


######

#### CALCULATE BUY & HOLD RETURNS!!!
## BUY from Closing price of day 1
## Sell from Closing price of last day!!!!
### Onu da ekle!!!!

#### METRICLER

# Ann.Sharpe
# Prof Max Drawdown
# Ending
# Average Min Loss Ratio
# Median Min Loss Ratio

# bu bes metrigin farkli hisselerdeki degerlerinin ortalamalari
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

f_vals <- stats[stats$nFAST == 0.1 & stats$nSLOW == 0.4,]
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

## bu sekilde 5 tanesini verebilirim.
barplot(stats[stats$Symbol == 'TTKOM',]$Ann.Sharpe[1:10],
        names.arg=paste(stats$HPFAST[1:10],stats$up[1:10],stats$down[1:10], sep="/"),
        las=2,cex.names=0.75)

sharpe_groups_max = stats %>% group_by(Symbol) %>% summarise(max = max(na.omit(Ann.Sharpe)))
sharpe_groups_min = stats %>% group_by(Symbol) %>% summarise(min = min(na.omit(Ann.Sharpe)))

sharpe_sd = stats %>% group_by(Symbol) %>% summarise(sd = sd(na.omit(Ann.Sharpe)))
p.mdraw_sd = stats %>% group_by(Symbol) %>% summarise(sd = sd(na.omit(Profit.To.Max.Draw)))

p.mdraw_max = stats %>% group_by(Symbol) %>% summarise(max = max(na.omit(Profit.To.Max.Draw)))
p.mdraw_min = stats %>% group_by(Symbol) %>% summarise(min = min(na.omit(Profit.To.Max.Draw)))


