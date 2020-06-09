library(httr)
library(rvest)
library(stringr)
library(dplyr)

# utility method ---------------------
parse_price <- function(s) {
  s <- str_replace_all(s, ",", "")
  s <- as.integer(s)
  
  return (s)
}

parse_date <- function(s) {
  s <- str_replace_all(s, "\\.", "-")
  s <- as.character(s)
  
  return (s)
}

# ------------------------------------

titles <- c()
dates <- c()

# 삼성물산 사이트 뉴스 퍼오기
for (i in 1:4) {
  url <- str_c("http://www.samsungcnt.com/news/list.do?pageNo=", i, "&newsKindCd=N2&seqNum=")
  
  response <- GET(url=url, encode="form")
  
  html <- response %>%
    as.character() %>%
    read_html()
  
  title <- html %>%
    html_nodes("span.boardTit") %>%
    html_text()
  
  titles <- c(titles, title)
  
  date <- html %>%
    html_nodes("span.date") %>%
    html_text()
  
  dates <- c(dates, date)
}

samsung_cnt_df <- data.frame(dates, titles, stringsAsFactors = FALSE)


stock_dates <- c()
end_prices <- c()
start_prices <- c()
high_prices <- c()
low_prices <- c()
amount <- c()

for (pgNum in 1:84) {
  
  stock_url <- str_c("https://finance.naver.com/item/sise_day.nhn?code=028260&page=", pgNum)
  
  response <- GET(url=stock_url, encode="form")
  
  html <- response %>%
    as.character() %>%
    read_html()
  
  trs <- html %>%
    html_nodes("tr")
  
  for (i in trs[c(3:7, 11:(length(trs) - 2))]) {
    values <- i %>%
      html_nodes("span") %>%
      html_text()
    stock_dates <- c(stock_dates, values[1])
    end_prices <- c(end_prices, values[2])
    start_prices <- c(start_prices, values[4])
    high_prices <- c(high_prices, values[5])
    low_prices <- c(low_prices, values[6])
    amount <- c(amount, values[7])
  }
}


stock_df <- data.frame(stock_dates, end_prices, start_prices, high_prices, low_prices, amount, stringsAsFactors = FALSE)


price_df <- apply(stock_df[,-1], 2, parse_price)

stock_df <- data.frame(stock_df[, 1], price_df, stringsAsFactors = FALSE)


colnames(stock_df)[1] <- "dates"


df <- merge(samsung_cnt_df, stock_df, by="dates", all=T)

df$dates <- sapply(df$dates, parse_date)

df$dates

write.csv(df, file="삼성물산.csv", row.names=F)
