library(tseries)
library(imputeTS)
library(dplyr)
library(tidyr)
library(lubridate)

post_times <- read.csv(file="naji102_posts_nxthr.csv")
post_times <- data.frame(post_times=unique(as.POSIXct(post_times$X0)))

ETH_1h <- as.data.frame(read.csv("ETHUSDT-1h-binance.csv"))
ETH_1h <- ETH_1h %>%
  mutate(timestamp = as.POSIXlt(timestamp, format = "%Y-%m-%d %H:%M:%S"))
length(ETH_1h$timestamp)

ETH_1h_clean <- data.frame(close=ETH_1h$close[-2020],
                           timestamp=seq(ETH_1h$timestamp[1], last(ETH_1h$timestamp), by='hour'))

post_times$signal <- rep(1, times = length(post_times$post_times)) 
post_times <- post_times %>% 
  complete(post_times = seq(min(ETH_1h$timestamp), max(ETH_1h$timestamp), by="hour")) 

length(post_times$post_times)

signal <- with(post_times, signal)
signal[is.na(signal)] <- 0

ETH_1h_clean$signal <- signal
  
close_with_na <- with(ETH_1h, close)
ETH_1h$close <- na_kalman(close_with_na)

ggplot_na_imputations(close_with_na, ETH_1h$close)


write.csv(ETH_1h_clean, file="ETHUSDT-1h-binance-imputed.csv")
