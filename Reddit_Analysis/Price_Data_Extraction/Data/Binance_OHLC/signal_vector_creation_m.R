library(imputeTS)
library(dplyr)
library(tidyr)
library(lubridate)

post_times <- read.csv(file="naji102_posts_nxtmin.csv")
post_times <- data.frame(post_times=unique(as.POSIXct(post_times$X0)))

ETH_1m <- as.data.frame(read.csv("ETHUSDT-1m-binance.csv"))
ETH_1m <- ETH_1m %>%
  mutate(timestamp = as.POSIXlt(timestamp, format = "%Y-%m-%d %H:%M:%S")) %>%
  complete(timestamp = seq(min(timestamp), max(timestamp), by="min"))
  
length(ETH_1m$timestamp)

#ETH_1h_clean <- data.frame(close=ETH_1h$close[-2020],
                           #timestamp=seq(ETH_1h$timestamp[1], last(ETH_1h$timestamp), by='hour'))

post_times$signal <- rep(1, times = length(post_times$post_times)) 
post_times <- post_times %>% 
  complete(post_times = seq(min(ETH_1m$timestamp), max(ETH_1m$timestamp), by="min")) 

length(post_times$post_times)

signal <- with(post_times, signal)
signal[is.na(signal)] <- 0

ETH_1m$signal <- signal

close_with_na <- with(ETH_1m, close)
ETH_1m$close <- na_kalman(close_with_na)

ggplot_na_imputations(close_with_na, ETH_1m$close)


write.csv(ETH_1m, file="ETHUSDT-1m-binance-imputed.csv")

get_pnl <- function(prices_data_with_signal, start=1000){   
  #extract closing prices and signal
  signal = prices_data_with_signal$signal
  close = prices_data_with_signal$close
  
  start = start
  pnl = c(start)
  i = 1
  for (index in (i+1):length(close)){
    if(signal[i] == 1){
      pnl <- append(pnl, last(pnl)*close[i+1]/close[i])
    }else{
      pnl <- append(pnl, last(pnl))
      }
    i <- i + 1
  }
  return(pnl)
}

pnl_m <- get_pnl(ETH_1m)

df_pnl <- data.frame(pnl=pnl_m, timestamp=post_times$post_times)
ggplot(df_pnl, aes(x=timestamp, y=pnl)) + geom_line()
