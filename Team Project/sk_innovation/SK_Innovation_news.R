library(httr)
library(rvest)
library(stringr)
library(RSelenium)
source("parse_lib.R")

# SK 이노베이션 사이트 URL
url <- "http://www.skinnovation.com/company/press.asp"

# RSelenium 구동을 위한 port열기
remDr <- remoteDriver(port=4445L, browserName="chrome")

# browser 오픈
remDr$open()

remDr$navigate(url)

# 날짜와 헤드라인 저장할 벡터 생성
dates <- c()
titles <- c()

# 퍼오기
for (i in 1:4) {
  for (j in 4:13) {
    html <- remDr$getPageSource()[[1]]
    
    table <- html %>%
      read_html() %>%
      html_nodes("table")
    
    tds <- table %>%
      html_nodes("td")
    
    date <- c()
    
    for (i in 1:10) {
      date[i] <- tds[i * 3] %>%
        html_text()
    }
    
    dates <- c(dates, date)
    
    titles <- c(titles, table %>%
                  html_nodes("a") %>%
                  html_text())
    
    # 다음 페이지 버튼 찾아서 클릭
    nextbutton <- remDr$findElement(using="xpath", str_c('//*[@id="contents"]/div/div[3]/div[2]/a[', j, ']'))
    nextbutton$clickElement()
  }
}
for (k in 4:9) {
  
  html <- remDr$getPageSource()[[1]]
  
  table <- html %>%
    read_html() %>%
    html_nodes("table")
  
  tds <- table %>%
    html_nodes("td")
  
  date <- c()
  
  for (i in 1:10) {
    date[i] <- tds[i * 3] %>%
      html_text()
  }
  
  dates <- c(dates, date)
  
  titles <- c(titles, table %>%
                html_nodes("a") %>%
                html_text())
  
  nextbutton <- remDr$findElement(using="xpath", str_c('//*[@id="contents"]/div/div[3]/div[2]/a[', k, ']'))
  nextbutton$clickElement()
}

google_url <- "https://www.google.com/"

remDr$navigate(google_url)
searchElem <- remDr$findElement(using="xpath", '//*[@id="tsf"]/div[2]/div[1]/div[1]/div/div[2]/input')
searchElem$sendKeysToElement(list("SK이노베이션", key="enter"))

newsbutton <- remDr$findElement(using="xpath", '//*[@id="hdtb-msb-vis"]/div[2]/a')
newsbutton$clickElement()

headlines <- c()
news_dates <- c()

for (num in 1:20) {
  if (num %% 10 == 0) {
    Sys.sleep(2)
  }
  html <- remDr$getPageSource()[[1]]
  
  search_results <- html %>%
    read_html() %>%
    html_nodes("div.g")
  
  headline <- search_results %>%
    html_nodes("a.l.lLrAF") %>%
    html_text()
  
  for (i in 1:length(search_results)) {
    ndate <- search_results[i] %>%
      html_nodes("span.f.nsa.fwzPFf")
    
    news_date <- ndate[1] %>%
      html_text()
    
    news_dates <- c(news_dates, news_date)
  }
  
  headlines <- c(headlines, headline)
  if (num != 20) {
    nbutton <- remDr$findElement(using="xpath", '//*[@id="pnnext"]')
    nbutton$clickElement()
  }
}

google_news <- data.frame(news_dates, headlines)

# 뉴스 dataframe 생성 및 저장
SK_news <- data.frame(dates, titles)

# 주식 데이터 저장 공간 생성
stock_dates <- c()
end_price <- c()
start_price <- c()
high_price <- c()
low_price <- c()
amount <- c()

# URL 마다 스크레이핑 
for (pgNum in 1:232) {
  
  stock_url <- str_c("https://finance.naver.com/item/sise_day.nhn?code=096770&page=", pgNum)
  
  remDr$navigate(stock_url)
  
  html <- remDr$getPageSource()[[1]]
  trs <- html %>%
    read_html() %>%
    html_nodes("tr")
  
  for (i in trs[c(3:7, 11:(length(trs) - 2))]) {
    values <- i %>%
      html_nodes("span") %>%
      html_text()
    stock_dates <- c(stock_dates, values[1])
    end_price <- c(end_price, values[2])
    start_price <- c(start_price, values[4])
    high_price <- c(high_price, values[5])
    low_price <- c(low_price, values[6])
    amount <- c(amount, values[7])
  }
}

# 주식 데이터 dataframe 생성
stock_df <- data.frame(stock_dates, end_price, start_price, high_price, low_price, amount)

# 데이터 정제
SK_news <- apply(SK_news, 2, parse_string)
stock_df <- apply(stock_df, 2, parse_date)
stock_prices <- apply(stock_df[, 2:6], 2, parse_price)
stock_df <- data.frame(stock_df[, 1], stock_prices)
colnames(stock_df)[1] <- "dates"
stock_df[, 1] <- as.character(stock_df[, 1])
SK_news <- data.frame(SK_news)
SK_news[, 1] <- as.character(SK_news[, 1])

# 두 개의 dataframe을 날짜를 기준으로 병합
df <- merge(SK_news, stock_df, by="dates" , all=T)

# csv파일 생성
write.csv(df, file="SK이노베이션.csv", row.names=F)
write.csv(google_news, file="google_news.csv", row.names=F)
