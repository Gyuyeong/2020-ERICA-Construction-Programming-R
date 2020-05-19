library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

t <- readLines("https://en.wikipedia.org/wiki/Science")
d <- htmlParse(t, asText = T)

clean_doc <- xpathSApply(d, "//p", xmlValue)

doc <- VectorSource(clean_doc) %>% Corpus
doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, stripWhitespace)

dtm <- DocumentTermMatrix(doc)

m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=T)
d <- data.frame(word=names(v), freq=v)

pal <- brewer.pal(11, "Spectral")

wordcloud(words=d$word,
          freq=d$freq,
          min.freq = 1,
          max.words = 500,
          random.order = F,
          rot.per = 0.50, 
          color=pal)

findFreqTerms(dtm, lowfreq = 12)
findAssocs(dtm, terms="science", corlimit=0.7)
findAssocs(dtm, terms="research", corlimit=0.7)
findAssocs(dtm, terms="theory", corlimit=0.7)

barplot(d[1:10,]$freq,
        las=2,
        names.arg = d[1:10,]$word,
        col="lightblue",
        main="Top frequent words",
        ylab="Word frequency")
