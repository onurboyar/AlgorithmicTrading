### Visualizations of Lowess - Trend Following Strategy (Only BIST)
# Author: Onur Boyar

# initial vals = f = 0.1
#.th_1=3
#.th_2=-3

f_0.1 <- my.lowess(BIST, 0.1)

plot(f_0.1)

par(mfrow = c(1,1))

chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))

data.frame <- (t(tradeStats(portfolio.st, 'BIST')))
x <- rownames(data.frame)
sharpe_p.max <- data.frame[x == 'Ann.Sharpe' | x == 'Profit.To.Max.Draw']
sharpe_p.max.df <- data.frame(Type = c('Sharpe Ration', 'Profit to Max Drawdown'), 
                              Values = as.numeric(sharpe_p.max))
library(ggplot2)
ggplot(sharpe_p.max.df) + geom_bar(aes(x = Type, y = Values), 
                                   stat = 'identity') +
  ggtitle('Initial Strategy - Sharpe Ratio & Profit to Max Draw.')

View(t(tradeStats(portfolio.st, 'BIST')))
# avg win trade = 81907.4
# number of trades = 2


######### OPTIMIZATION

# Nsample = 10
#.up_d = (1:10)
#.down_d = (-1:-10) 
#.f_val = c(0.01, 0.03, 0.06, 0.09, 0.15, 0.25, 0.38, 0.5, 0.7, 0.85, 1)

par(mfrow = c(1,2))
stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$up[1:10], stats$down[1:10], sep="/"),
        las=2,cex.names=.80)
title('Sharpe Ratio Order')

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$up[1:10], stats$down[1:10], sep="/"),
        las=2,cex.names=.80)
title('Profit to Max Draw Order')


ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, colour = Ann.Sharpe > 15.5 ), size = 5) +
  scale_colour_manual(values = c("blue", "red")) + 
  ggtitle('Sharpe Ratio - Profit to Max Draw') + theme(legend.position = 'none')


# Selected F 0.85, up 2, down -10
# re-run strategy with those values

par(mfrow = c(1,1))
chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))

Eq.optim = Eq
Eq.initial = Eq



######## INITIAL VALUES
# initial - sharpe = 14.22239
# initial - p max draw = 1.706953
# initial - avg win trade = 81907.4
# initial - number of trades = 2
# end eq= 163814.8

######### OPTIMALL VALUES
# sharpe = 15.55542
# p.max = 1.874996
# number trade = 2
# avg win = 89970.89
# end eq = 179941.8

sharpe_df <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                        Values = c(14.22239, 15.55542))

p.s <- ggplot(sharpe_df) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Sharpe Ratios of Initial and Optimum Strategies')

p.s

p.max_draw.df <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                            Values = c(1.706953, 1.874996))

p.p <- ggplot(p.max_draw.df) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Profit to Max Drawdown Values of Initial and Optimum Strategies')

p.p

avg_win.df <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                         Values = c(81907.4, 89970.89))

p.avg <- ggplot(avg_win.df) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Avg Win Trade Values of Initial and Optimum Strategies')

p.avg

number_trades <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                            Values = c(2, 2))

p.num <- ggplot(number_trades) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Trade Counts of Initial and Optimum Strategies')

p.num

### install.packages('ggpubr')
# ggpubr used to plot multiple ggplot objects in the same view
ggpubr::ggarrange(p.s, p.p, p.avg, p.num)

Eq.optimum = Eq
Eq.initial = Eq
par(mfrow = c(1,2))
plot(as.zoo(Eq.initial))
title('Equity of Initial-Optimum Strategies')
plot(as.zoo(Eq.optim))









