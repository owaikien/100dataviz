suppressWarnings({

library("crypto2")
library("dplyr")
library("lubridate")
library("ggplot2")
library("quantmod")

# retrieve coins
coins <- crypto_list(only_active=TRUE)
btc <- coins[0:1,]


# $BTC --------------------------------------------------------------------
# Get price for btc
btc_hist <- crypto_history(btc, start_date = "20100713", end_date= "20230626", interval="monthly", finalWait=FALSE)

# Somehow I could only scrap from Aprik 2013 onwards
# Let's just move on
# Compute % returns
btc_hist$lag_close <- lag(btc_hist$close)
btc_hist <- btc_hist[2:nrow(btc_hist),]
btc_hist$pct_return <- ((btc_hist$close - btc_hist$lag_close) / btc_hist$lag_close) * 100
btc_hist$timestamp <- as.Date(btc_hist$timestamp, format = "%m/%d/%Y")

# Create break points and labels for axis ticks
brks <- btc_hist$timestamp[seq(1, length(btc_hist$timestamp), 12)]
lbls <- year(btc_hist$timestamp[seq(1, length(btc_hist$timestamp), 12)])

# Create area plot
ggplot(btc_hist, aes(timestamp, pct_return)) +
  geom_area() +
  scale_x_date(breaks=brks, labels=lbls) +
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title="$BTC monthly % return",
       subtitle="(2013-2023)",
       y = "% Returns",
       caption = "Source: CoinMarketCap")


# SPX500 ------------------------------------------------------------------
# Let's create another one for S&P500
Sys.setenv(tz="UTC")
sp500 <- getSymbols('^GSPC', auto.assign=FALSE, from="2013-05-01", periodicity = 'monthly')
sp500$lag_close <- lag(sp500$GSPC.Close)
sp500 <- sp500[2:nrow(sp500), ]
sp500$pct_return <- ((sp500$GSPC.Close - sp500$lag_close) / sp500$lag_close) * 100
sp500 <- as.data.frame(sp500)
sp500$date <- row.names(sp500)
sp500$date <- as.Date(sp500$date, format = "%Y-%m-%d")
sp500 <- sp500[1:(nrow(sp500)-1),]

# Create break points and labels for axis ticks
brks <- sp500$date[seq(1, length(sp500$date), 12)]
lbls <- lubridate::year(sp500$date[seq(1, length(sp500$date), 12)])

# Create plot
ggplot(sp500, aes(date, pct_return)) +
  geom_area() +
  scale_x_date(breaks=brks, labels=lbls) +
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title="$SPX500 monthly % return",
       subtitle="(2013-2023)",
       y = "% Returns",
       caption = "Source: YahooFinance")

})