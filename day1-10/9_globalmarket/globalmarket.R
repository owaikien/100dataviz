library(ggplot2)
library(ggrepel)
library(tidyverse)

# creating data frame of assets with their respective market cap
# Data source
# Stocks - https://www.sifma.org/resources/research/research-quarterly-equities/#:~:text=The%20U.S.%20equity%20markets%20are,the%20next%20largest%20market%2C%20EU.
# Bond - https://advisor.visualcapitalist.com/the-largest-bond-markets-in-the-world/#:~:text=In%202022%2C%20the%20global%20bond%20market%20totaled%20%24133%20trillion.
# Commodities (Metals) - https://8marketcap.com/metals/
# Forex - https://www.forex.academy/what-is-the-forex-market-cap/
# Crypto - TradingView
market <- data.frame(Assets = c("Stock", "Bond", "Commodities (Metals)", "Forex", "Crypto"),
                     Market_Cap = c(108.60, 133.00, 15.13, 6.60, 1.14))

# get the percentage owned by each asset
market$percentage <- market$Market_Cap / sum(market$Market_Cap) * 100
market$percentage <- round(market$percentage, 2)


# Get the positions
market <- market %>%
  mutate(csum = rev(cumsum(rev(Market_Cap))), 
         pos = Market_Cap/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Market_Cap/2, pos))

# Create pie chart
ggplot(market, aes(x = "", y = Market_Cap, fill=fct_inorder(Assets))) + 
  geom_col(width=1, color=1) + 
  coord_polar(theta = "y") + 
  geom_label_repel(data = market,
                   aes(y = pos, label = paste0(Market_Cap, " trillion ($)")),
                   size=3, nudge_x=0.5, show.legend=FALSE) +
  guides(fill = guide_legend(title="Assets")) +
  theme_void() +
  theme(legend.position = "bottom") + 
  ggtitle("Risk Assets - Proportion of Market Capitalization")
