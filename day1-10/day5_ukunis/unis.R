suppressWarnings({
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(scales)

setwd("~/Documents/100daysviz/day5_ukunis")
data <- read.csv("unis.csv", sep=",")
df <- data %>%
  separate(`University..2022.2023..2023.2024`, into = c("University", "2022/2023",
                                                        "2023/2024", sep=", "))
df <- df[,1:3]

# Change to numeric and get percentage change
df$`2022/2023` <- as.numeric(df$`2022/2023`)
df$`2023/2024` <- as.numeric(df$`2023/2024`)

df <- df %>%
  mutate(change = (`2023/2024` - `2022/2023`) / `2022/2023` * 100)

# Pivot data 
df_long <- df %>%
  pivot_longer(cols = `2022/2023`:`2023/2024`, names_to = 'Year', values_to = 'Fee')

# Check dtype
str(df_long)
df_long$Fee = as.numeric(df_long$Fee)

# Plot slope graph
ggplot(df_long, aes(x = Year, y = Fee, group = University)) +
  geom_line(aes(color = University), size = 1) +
  geom_point(aes(color = University), size = 3) +
  geom_text_repel(data = df, aes(x = "2022/2023", y = `2022/2023`, label = University), hjust=0.35) +
  geom_text_repel(data = df, aes(x = "2023/2024", y = `2023/2024`, label = University), hjust=-0.4) +
  geom_label_repel(data = df, aes(x = 1.5, y = (`2022/2023` + `2023/2024`)/2, label = paste0("+", paste0(round(change, 1), "%"))) ,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.05, "lines"),
                   segment.color = 'grey50') +
  scale_y_continuous(labels = f <- function(x) paste0("£", x)) +
  theme_minimal() +
  ggtitle("UK Universities Tuition Fees Increase\nBiological Sciences (2022/2023 - 2023/2024)") +
  theme(plot.title = element_text(size=12.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.2, color="grey"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position = "none")
})


