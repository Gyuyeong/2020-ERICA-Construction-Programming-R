library(data.table)
library(KoNLP)
library(tm)
library(SnowballC)
library(stringr)
source("parse_lib.R")
Sys.setlocale("LC_ALL", "korean")

# bring the csv files
hankyung_df <- read.csv("한경뉴스.csv", fileEncoding="euc-kr")
sk_df <- read.csv("SK이노베이션.csv", fileEncoding="euc-kr")

# -------------------------------------------------------

# m <- "YYYY-mm" format input necessary
group_news <- function(m) {
  hankyung_titles <- subset(hankyung_df, grepl(m, hankyung_df$dates, fixed=T), select = titles)
  hankyung_paragraphs <- subset(hankyung_df, grepl(m, hankyung_df$dates, fixed=T), select = paragraphs)
  sk_news_titles <- subset(sk_df, grepl(m, sk_df$dates, fixed=T) & is.na(sk_df$titles) != 1, select = titles)
  
  colnames(hankyung_paragraphs) <- c("titles")
  
  news <- rbind(hankyung_titles, hankyung_paragraphs, sk_news_titles)
  return (news)
}

# -------------------------------------------------------

for (i in 1:6) {
  nam <- paste("news.20.0", i, sep="")
  assign(nam, group_news(str_c("2020-0", i)))
}

for (i in 1:9) {
  nam <- paste("news.19.0", i, sep="")
  assign(nam, group_news(str_c("2019-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.19.", i, sep="")
  assign(nam, group_news(str_c("2019-", i)))
}

for (i in 1:9) {
  nam <- paste("news.18.0", i, sep="")
  assign(nam, group_news(str_c("2018-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.18.", i, sep="")
  assign(nam, group_news(str_c("2018-", i)))
}

for (i in 1:9) {
  nam <- paste("news.17.0", i, sep="")
  assign(nam, group_news(str_c("2017-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.17.", i, sep="")
  assign(nam, group_news(str_c("2017-", i)))
}

for (i in 1:9) {
  nam <- paste("news.16.0", i, sep="")
  assign(nam, group_news(str_c("2016-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.16.", i, sep="")
  assign(nam, group_news(str_c("2016-", i)))
}

for (i in 1:9) {
  nam <- paste("news.15.0", i, sep="")
  assign(nam, group_news(str_c("2015-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.15.", i, sep="")
  assign(nam, group_news(str_c("2015-", i)))
}

for (i in 1:9) {
  nam <- paste("news.14.0", i, sep="")
  assign(nam, group_news(str_c("2014-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.14.", i, sep="")
  assign(nam, group_news(str_c("2014-", i)))
}
for (i in 1:9) {
  nam <- paste("news.13.0", i, sep="")
  assign(nam, group_news(str_c("2013-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.13.", i, sep="")
  assign(nam, group_news(str_c("2013-", i)))
}
for (i in 1:9) {
  nam <- paste("news.12.0", i, sep="")
  assign(nam, group_news(str_c("2012-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.12.", i, sep="")
  assign(nam, group_news(str_c("2012-", i)))
}

for (i in 1:9) {
  nam <- paste("news.11.0", i, sep="")
  assign(nam, group_news(str_c("2011-0", i)))
}
for (i in 10:12) {
  nam <- paste("news.11.", i, sep="")
  assign(nam, group_news(str_c("2011-", i)))
}

useNIADic()


news.20.06 <- as.matrix(news.20.06)

doc <- VectorSource(news.20.06) %>% VCorpus
inspect(doc)

doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, removePunctuation)
dim(news.20.06)
dtm <- DocumentTermMatrix(doc)
dtm$dimnames$Terms
dtm <- as.matrix(dtm)

dim(dtm)

news.20.05 <- as.matrix(news.20.05)

doc.20.05 <- VectorSource(news.20.05) %>% VCorpus
inspect(doc.20.05)

doc.20.05 <- tm_map(doc.20.05, content_transformer(tolower))
doc.20.05 <- tm_map(doc.20.05, removeWords, stopwords("english"))
doc.20.05 <- tm_map(doc.20.05, removePunctuation)

inspect(doc.20.05)

dtm.20.05 <- DocumentTermMatrix(doc.20.05)
dtm.20.05 <- as.matrix(dtm.20.05)
dim(dtm.20.05)
