library(ggplot2)
library(ggrepel)
library(hrbrthemes)
library(viridis)

# load dframe
df <- read.csv("events.csv", header=TRUE, sep=",")

# Calculate radius of circles
radius <- sqrt(df$Cost..billions..USD./pi) * 1000

# Create the bubble chart using ggplot
ggplot(df, aes(x = Year, y = Duration..days., size = Cost..billions..USD., 
               label = Events, fill=Category)) +
  geom_point(alpha=0.7, shape=21, color='black') +
  geom_text_repel(
    box.padding = 0.25, 
    point.padding = 0.3, 
    segment.color = 'black', 
    segment.size = 0.3,
    size=2.4,
    max.overlaps= Inf
  ) +
  scale_size(range=c(.1, 24), name='Adjusted Cost in billions USD ($)') +
  xlab("Year") +
  ylab("Duration (days)") +
  ggtitle("The Economic Cost of Historical Events") +
  ylim(0, 20000) +
  xlim(1910, 2025) +
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        legend.text=element_text(size=10))
