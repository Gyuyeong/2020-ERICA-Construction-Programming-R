library(httr)
library(rvest)
library(stringr)
library(dplyr)

parse_string <- function(s) {
  s <- str_replace_all(s, "[\t\n\r]", "")
  s <- str_replace_all(s, "▶", "")
  s <- str_replace_all(s, "◆", "")
  s <- str_replace_all(s, "◇", "")
  s <- str_replace_all(s, "▲", "")
  s <- str_replace_all(s, "■", "")
  s <- str_replace_all(s, "△", "")
  s <- str_replace(s, "^\\.\\.\\.", "")
  s <- str_replace(s, "\\.\\.\\.$", "")
  s <- str_replace_all(s, "[\U4E00-\U9FFF\U3000-\U303F]", "")
  s <- str_replace_all(s, "^\\s+", "")
  s <- str_replace_all(s, "[ ]+$", "")
  return (s)
}

parse_date <- function(s) {
  s <- substring(s, 1, 10)
  s <- str_replace_all(s, "\\.", "-")
  s <- as.character(s)
  return (s)
}

url <- "https://search.hankyung.com/apps.frm/search.news?query=%ED%98%84%EB%8C%80%EC%9E%90%EB%8F%99%EC%B0%A8&page=1"

response <- GET(url=url, encode="form")

html <- response %>%
  as.character() %>%
  read_html()

search_results <- html %>%
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

all_titles
all_paragraphs
all_dates

keywords <- c("현대차", "전기차", "현대자동차", "현대·기아차", "기아차", "수소자동차", "아반떼", "제네시스", "부품사", "지동차 수출", "팰리세이드", "맥스크루즈", "suv", "투싼", "그랜저", "베뉴", "코나", "싼타페", "이온", " i10", "i20", "ix20", "엑센트", "i30", "쏘나타", "i40", "아슬란", "에쿠스")

titles <- c()
dates <- c()
paragraphs <- c()

for (pgNum in 1:3000) {
  url <- str_c("https://search.hankyung.com/apps.frm/search.news?query=%ED%98%84%EB%8C%80%EC%9E%90%EB%8F%99%EC%B0%A8&page=", pgNum)
  
  response <- GET(url=url, encode="form")
  
  html <- response %>%
    as.character() %>%
    read_html()
  
  search_results <- html %>%
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
  
}
length(titles)
length(dates)
length(paragraphs)
head(titles)

df <- data.frame(dates, titles, paragraphs, stringsAsFactors = F)

df[, 2:3] <- apply(df[, 2:3], 2, parse_string)
head(df)
df$dates <- unlist(lapply(df$dates, parse_date))
head(df)

write.csv(df, "현대자동차_한경.csv", row.names = F)
