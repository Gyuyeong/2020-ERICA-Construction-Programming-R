library(data.table)
library(ggplot2)
library(scales)

df <- fread("»ï¼º¹°»ê.csv")

df$dates <- as.Date(df$dates)

ggplot(data=df, aes(x=dates, y=end_prices, group=1)) +
  geom_line() +
  scale_x_date(date_labels="%Y-%b-%d")
