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

url <- "https://search.hankyung.com/apps.frm/search.news?query=%EC%82%BC%EC%84%B1%EB%AC%BC%EC%82%B0&page=100"

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

keywords <- c("삼성물산", "래미안", "건설", "건축", "토목", "반포아파트 3주구", "반포아파트", "반포3주구", "반포", "수주", "분양", "빌딩", "플랜트", "주택")

titles <- c()
dates <- c()
paragraphs <- c()

for (pgNum in 1:800) {
  if (pgNum %% 100 == 0) {
    print("==============================")
  }
  url <- str_c("https://search.hankyung.com/apps.frm/search.news?query=%EC%82%BC%EC%84%B1%EB%AC%BC%EC%82%B0&page=", pgNum)
  
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

write.csv(df, "삼성물산_한경.csv", row.names = F)
