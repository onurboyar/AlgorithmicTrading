### Visualizations of General Lowess and Lowess - Crossover Strategy (Only BIST)
# Author: Onur Boyar

#### Lowess Example
data(cars)
attach(cars)
plot(dist ~ speed, data=cars, main="lowess(cars)")
lines(lowess(dist ~ speed), col=2, lty=2, lwd = 4)
lines(lowess(dist ~ speed, f=.2), col=3, lwd = 4) # smaller bandwith
legend(5, 120, c(paste("f=", c("2/3", ".2"))), lty=1, col=2:3)

##### Lowess Example
BIST_Closing <- Cl(XU100)
BIST.F_0.01 <- my.lowess(XU100, f = 0.01)
BIST.F_0.1 <- my.lowess(XU100, f = 0.1)
BIST.F_0.4 <- my.lowess(XU100, f = 0.4)
BIST.F_0.7 <- my.lowess(XU100, f = 0.7)
BIST.F_1 <- my.lowess(XU100, f = 1)

par(mfrow = c(2,3))

plot(BIST_Closing)
plot(BIST.F_0.01)
plot(BIST.F_0.1)
plot(BIST.F_0.4)
plot(BIST.F_0.7)
plot(BIST.F_1)

# initial strategy without optimization
# f_fast = 0.01, f_slow = 0.4

BIST.F_0.01 <- my.lowess(XU100, f = 0.01)
BIST.F_0.4 <- my.lowess(XU100, f = 0.4)

par(mfrow = c(1,1))

comparison = cbind(Cl(XU100), BIST.F_0.01, BIST.F_0.4)
p1 <- plot(comparison, 
     plot.type = "single", 
     col = c("green","black", "blue"),
     lwd = 2)
p1 <- addLegend("topleft", legend.names = c("Green - BIST Closing","Black - F_0.01", "Blue - F_0.4"),lwd = 4)
p1


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

# avg win trade = 5698.963
# number of trades = 11
## initial strategy plots are done for initial crossover lowess strategy
# now optimization results will be visualized

# Nsample = 10
par(mfrow = c(1,2))
stats <- stats[order(stats$Ann.Sharpe, decreasing = T),]

barplot(stats$Ann.Sharpe[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=1.00)
title('Sharpe Ratio Order')

stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]

barplot(stats$Profit.To.Max.Draw[1:10],
        names.arg=paste(stats$F_Fast[1:10],stats$F_Slow[1:10], sep="/"),
        las=2,cex.names=1.00)
title('Profit to Max Draw Order')


ggplot(stats) + geom_point(aes(y = Ann.Sharpe, x = Profit.To.Max.Draw, colour = Profit.To.Max.Draw > 1 ), size = 5) +
  scale_colour_manual(values = c("blue", "red")) + 
  ggtitle('Sharpe Ratio - Profit to Max Draw') + theme(legend.position = 'none')

# Selected F's 0.25, 0.5
stats <- stats[order(stats$Profit.To.Max.Draw, decreasing = T),]
stats$F_Fast[1]
stats$F_Slow[1]

lowess_cross_f_optim.1 <- my.lowess(BIST, f = stats$F_Fast[1])
lowess_cross_f_optim.2 <- my.lowess(BIST, f = stats$F_Slow[1])

par(mfrow = c(1,1))

Strategy = cbind(lowess_cross_f_optim.1, lowess_cross_f_optim.2)
p1 <- plot(Strategy, 
     plot.type = "single", 
     col = c("red", "blue"),
     lwd = 2)
p1 <- addLegend("topleft", legend.names = c("Red - 0.25","Blue - 0.50"),lwd = 4)
p1

Strategy = cbind(Cl(BIST),lowess_cross_f_optim.1, lowess_cross_f_optim.2)
p2 <- plot(Strategy, 
     plot.type = "single", 
     col = c("purple","red", "blue"),
     lwd = 2, auto.legend = TRUE)
p2 <- addLegend("topleft", legend.names = c("Purple - BIST","Red - 0.25","Blue - 0.50"),lwd = 4)
p2

# re-run strategy with those values 
chart.ME(portfolio.st, 'BIST', scale='percent', type='MAE')
chart.ME(portfolio.st, 'BIST', scale='percent', type='MFE')

Eq<-getAccount(account.st)$summary[,"End.Eq"]
plot(as.zoo(Eq))


###### INITIAL VALUES
# initial - sharpe = 5.252274
# initial - p max draw = .8713229
# initial - avg win trade = 5698.963
# initial - number of trades = 11

###### OPTIMAL VALUES
# optim - sharpe = 22.67891
# optim - p max draw = 1.289801
# optim - avg win trade = 15401.93
# optim - number of trades = 3

sharpe_df <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                        Values = c(5.252274, 22.67891))

p.s <- ggplot(sharpe_df) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Sharpe Ratios of Initial and Optimum Strategies')

p.s

p.max_draw.df <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                            Values = c(.8713229, 1.289801))

p.p <- ggplot(p.max_draw.df) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Profit to Max Drawdown Values of Initial and Optimum Strategies')

p.p

avg_win.df <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                         Values = c(5698.963, 15401.93))

p.avg <- ggplot(avg_win.df) + geom_bar(aes(x = Type, y = Values), stat = 'identity') +
  ggtitle('Avg Win Trade Values of Initial and Optimum Strategies')

p.avg

number_trades <- data.frame(Type = c('Initial Strategy', 'Optimum Strategy'),
                            Values = c(3, 11))

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
plot(as.zoo(Eq.optimum))










