library(KoNLP)
library(data.table)
library(tm)
library(SnowballC)
library(wordcloud2)
library(dplyr)

df <- read.csv("현대자동차_한경.csv")

head(df)
doc <- VectorSource(df$titles) %>% Corpus
inspect(doc)

doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, tolower)

useNIADic()

nouns <- extractNoun(doc)

wordcount <- table(unlist(nouns))
df_words <- as.data.frame(wordcount, stringsAsFactors = F)

df_words <- df_words[order(df_words$Freq, decreasing = T),]
df_words <- df_words[nchar(df_words$Var1) >=2, ]
wordcloud2(df_words, gridSize = 20)
