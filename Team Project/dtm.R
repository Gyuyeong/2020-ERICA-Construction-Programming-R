library(data.table)
library(KoNLP)
library(tm)
library(SnowballC)
library(stringr)
source("parse_lib.R")
Sys.setlocale("LC_ALL", "korean")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

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

useNIADic()

doc <- VectorSource(hankyung_df[, 2]) %>% Corpus
inspect(doc)

doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, removePunctuation)
inspect(doc)

nouns <- extractNoun(doc)

wordcount <- table(unlist(nouns))
df_words <- as.data.frame(wordcount, stringsAsFactors = F)

df_words <- df_words[order(df_words$Freq, decreasing = T),]
df_words <- df_words[nchar(df_words$Var1) >=2, ]
wordcloud2(df_words, gridSize = 15)
