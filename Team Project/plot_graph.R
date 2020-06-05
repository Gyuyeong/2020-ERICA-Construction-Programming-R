library(ggplot2)
library(scales)
library(TTR)

df <- read.csv("SK이노베이션.csv")

df$dates <- as.Date(df$dates)

for (i in which(is.na(df$end_price))) {
  df[i, "end_price"] <- as.integer(mean(df[(i-5):(i+4), "end_price"], na.rm=T))
}

ema.50 <- EMA(df$end_price, 50)
ema.100 <- EMA(df$end_price, 100)
ema.200 <- EMA(df$end_price, 200)
head(df)

df <- cbind(df, ema.50, ema.100, ema.200)

ggplot(data=df, aes(x=dates)) +
  geom_line(aes(y=end_price, color="end_price")) +
  geom_line(aes(y=ema.50, color="ema.50")) +
  geom_line(aes(y=ema.100, color="ema.100")) +
  geom_line(aes(y=ema.200, color="ema.200")) +
  scale_x_date(date_labels = "%Y-%b-%d") +
  ggtitle("SK Innovation Stock Price 2011-Now") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Date") +
  ylab("Closed Price") +
