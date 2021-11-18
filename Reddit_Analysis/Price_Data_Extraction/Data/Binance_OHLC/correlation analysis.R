library(stargazer)
library(ggpubr)
pnl_m <- read.csv('pnl_naji102_min.csv')
pnl_hr <- read.csv('pnl_naji102_hr.csv')
eth1m <- read.csv("ETHUSDT-1m-binance-imputed.csv")
eth1hr <- read.csv("ETHUSDT-1h-binance-imputed.csv")

cor.test(pnl_m$X0, eth1m$close)
cor.test(pnl_hr$X, eth1hr$close)

ggscatter(data=data.frame(y = pnl_hr$X0, x = eth1hr$close), x = "y", y = "x",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PnL", ylab = "Close price") + ggtitle("Hourly close price against proft 'n' loss trajectory")

