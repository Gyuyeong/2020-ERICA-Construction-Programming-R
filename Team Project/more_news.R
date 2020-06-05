library(RSelenium)
library(rvest)
library(httr)
library(stringr)
source("parse_lib.R")


url <- "https://search.hankyung.com/apps.frm/search.news?query=SK%EC%9D%B4%EB%85%B8%EB%B2%A0%EC%9D%B4%EC%85%98"

remDr <- remoteDriver(port=4445L, browserName="chrome")

remDr$open()
remDr$navigate(url)

html <- remDr$getPageSource()


search_results <- html[[1]] %>%
  read_html() %>%
  html_nodes("ul.article") %>%
  html_nodes("li")

all_titles <- search_results %>%
  html_nodes("em.tit") %>%
  html_text()

all_dates <- search_results %>%
  html_nodes("span.date_time") %>%
  html_text()

all_paragraphs <- search_results %>%
  html_nodes("p.txt") %>%
  html_text()


keywords <- c("SK이노", "배터리", "전기차", "석유", "리튬", "전지", "전기", "정유", "유가", "석유화학")

titles <- c()
dates <- c()
paragraphs <- c()

for (i in 1:length(all_titles)) {
  values_t <- c()
  values_p <- c()
  for (word in keywords) {
    value_t <- str_detect(all_titles[i], word)
    value_p <- str_detect(all_paragraphs[i], word)
    values_t <- c(values_t, value_t)
    values_p <- c(values_p, value_p)
  }
  

  if (sum(values_t) != 0 | sum(values_p) != 0) {
    titles <- c(titles, all_titles[i])
    dates <- c(dates, all_dates[i])
    paragraphs <- c(paragraphs, all_paragraphs[i])
  }
}

titles <- c()
dates <- c()
paragraphs <- c()

keywords <- c("SK이노", "배터리", "전기차", "석유", "리튬", "전지", "전기", "정유", "유가", "석유화학")

cnt <- 1
for (pgNum in 1:500) {
  
  html <- remDr$getPageSource()
  
  
  search_results <- html[[1]] %>%
    read_html() %>%
    html_nodes("ul.article") %>%
    html_nodes("li")
  
  all_titles <- search_results %>%
    html_nodes("em.tit") %>%
    html_text()
  
  all_dates <- search_results %>%
    html_nodes("span.date_time") %>%
    html_text()
  
  all_paragraphs <- search_results %>%
    html_nodes("p.txt") %>%
    html_text()
  
  for (i in 1:length(all_titles)) {
    values_t <- c()
    values_p <- c()
    for (word in keywords) {
      value_t <- str_detect(all_titles[i], word)
      value_p <- str_detect(all_paragraphs[i], word)
      values_t <- c(values_t, value_t)
      values_p <- c(values_p, value_p)
    }
    
    
    if (sum(values_t) != 0 | sum(values_p) != 0) {
      titles <- c(titles, all_titles[i])
      dates <- c(dates, all_dates[i])
      paragraphs <- c(paragraphs, all_paragraphs[i])
    }
  }
  
  
  p <- pgNum %% 10
  
  if (p == 0) {
    if (cnt == 1) {
      nextButton <- remDr$findElement(using="xpath", str_c('//*[@id="content"]/div[1]/div/div[2]/div/a[2]'))
      nextButton$clickElement()
      cnt <- cnt + 1
    } else {
      nextButton <- remDr$findElement(using="xpath", str_c('//*[@id="content"]/div[1]/div/div[2]/div/a[3]'))
      nextButton$clickElement()
    }
  } else {
    nextButton <- remDr$findElement(using="xpath", str_c('//*[@id="content"]/div[1]/div/div[2]/div/span/a[', p, ']'))
    nextButton$clickElement()
  }
}

hankyung_df <- data.frame(dates, titles, paragraphs, stringsAsFactors = FALSE)

hankyung_df <- apply(hankyung_df, 2, parse_string)
hankyung_df[, "dates"] <- unlist(lapply(hankyung_df[, "dates"], parse_date))
hankyung_df <- data.frame(hankyung_df, stringsAsFactors = F)
dim(hankyung_df)
head(hankyung_df)

write.csv(hankyung_df, file="한경뉴스.csv", row.names = F)
