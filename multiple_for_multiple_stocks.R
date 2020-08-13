# Plots for visualizing multiple stock results
# Author: Onur Boyar


par(mfrow = c(1,1))
stats <- (t(tradeStats(portfolio.st, stocks)))

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))
Eq.optim1 <- Eq
title('Ending Equity')
groups_num_trade <- data.frame(Stocks = colnames(stats), NumTrade = stats[4,])
groups_sharpe <- data.frame(Stocks = colnames(stats),
                     SharpeRatio = c(15.55542, 16.31626, 31.09870, 0, 35.11268, 0, 18.14037))
groups_p.max <- data.frame(Stocks = colnames(stats),
                           ProfitToMaxDraw = stats[27,])
groups_av.win <- data.frame(Stocks = colnames(stats),
                            AvgWinTrade = stats[17,])

p1 <- ggplot(groups_sharpe) + geom_bar(aes(x = Stocks, y = SharpeRatio, fill = Stocks), stat = 'identity') + 
  theme(legend.position = "none") + ggtitle('Sharpe Ratio Values') + xlab('Stock Type') +
  ylab('Sharpe Value')

p1

p2 <- ggplot(groups_p.max) + geom_bar(aes(x = Stocks, y = ProfitToMaxDraw, fill = Stocks), stat = 'identity') +
  ggtitle('Profit to Max Drawdown Values')+ xlab('Stock Type') +
  ylab('Profit to MAx Drawdown Value') +theme(legend.position = "none")

p2

p3 <- ggplot(groups_num_trade) + geom_bar(aes(x = Stocks, y = NumTrade, fill = Stocks), stat = 'identity') +
  ggtitle('Number of Trades')+ xlab('Stock Type') +
  ylab('Trade Number') +theme(legend.position = "none")

p3

p4 <- ggplot(groups_av.win) + geom_bar(aes(x = Stocks, y = AvgWinTrade, fill = Stocks), stat = 'identity') +
  ggtitle('Average Win Trade Values')+ xlab('Stock Type') +
  ylab('Avg Win Trade') +theme(legend.position = "none")

p4

library(ggpubr)
ggarrange(p3, p1, p2, p4)


##########
library(dplyr)
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

ggplot(stats) + geom_point(aes(x = Ann.Sharpe, y = Profit.To.Max.Draw, color = Symbol), size = 5) +
  ggtitle('Sharpe Ratios and Profit to Maximum Drawdown')

options(scipen = 1e7)
ggplot(stats) + geom_point(aes(x = End.Equity, y = Profit.To.Max.Draw, color = Symbol), size = 5) +
  ggtitle('Ending Equity and Profit to Maximum Drawdown')

# En yuksek Profit to Max Drawdown veren ile en yuksek end equity veren
# ayni, 10, -4, 0.38	

# onunla tekrar yapiyorum

stats <- (t(tradeStats(portfolio.st, stocks)))

Eq.optim2<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq.optim2))
title('Ending Equity')
groups_num_trade <- data.frame(Stocks = colnames(stats), NumTrade = stats[4,])
groups_sharpe <- data.frame(Stocks = colnames(stats),
                            SharpeRatio = c(15.55542, 16.31626, 31.09870, 0, 35.11268, 0, 18.14037))
groups_p.max <- data.frame(Stocks = colnames(stats),
                           ProfitToMaxDraw = stats[27,])
groups_av.win <- data.frame(Stocks = colnames(stats),
                            AvgWinTrade = stats[17,])

p1 <- ggplot(groups_sharpe) + geom_bar(aes(x = Stocks, y = SharpeRatio, fill = Stocks), stat = 'identity') + 
  theme(legend.position = "none") + ggtitle('Sharpe Ratio Values') + xlab('Stock Type') +
  ylab('Sharpe Value')

p1

p2 <- ggplot(groups_p.max) + geom_bar(aes(x = Stocks, y = ProfitToMaxDraw, fill = Stocks), stat = 'identity') +
  ggtitle('Profit to Max Drawdown Values')+ xlab('Stock Type') +
  ylab('Profit to MAx Drawdown Value') +theme(legend.position = "none")

p2

p3 <- ggplot(groups_num_trade) + geom_bar(aes(x = Stocks, y = NumTrade, fill = Stocks), stat = 'identity') +
  ggtitle('Number of Trades')+ xlab('Stock Type') +
  ylab('Trade Number') +theme(legend.position = "none")

p3

p4 <- ggplot(groups_av.win) + geom_bar(aes(x = Stocks, y = AvgWinTrade, fill = Stocks), stat = 'identity') +
  ggtitle('Average Win Trade Values')+ xlab('Stock Type') +
  ylab('Avg Win Trade') +theme(legend.position = "none")

p4

library(ggpubr)
ggarrange(p3, p1, p2, p4)

par(mfrow=c(1,1))
plot((Eq.optim1))
plot((Eq.optim2))
abline(h = getEndEq(account.st, Sys.Date()))

OptimizedValues = cbind(Eq.optim1, Eq.optim2)
p1 <- plot(OptimizedValues, 
           plot.type = "single", 
           col = c("red", "blue"),
           lwd = 2)
p1 <- addLegend("topleft", legend.names = c("Optimized Value (0.85, 2, -10)","Values From Further Optimization (0.38, 10, -4"),lwd = 4)
p1


####### REGIME FILTER
EndingEq <- MGROS_Regime + FROTO_Regime + TTKOM_Regime + BIST_Regime + GARAN_Regime + TUPRS_Regime + KRDMD_Regime - 6000000

