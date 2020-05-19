library(httr)
library(rvest)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(KoNLP)
library(wordcloud2)

# 앞에 붙어있는 태그를 빼는 함수
parse_str <- function(s) {
  if (startsWith(s, "[건설뉴스]")) {
    str <- substring(s, 8)
  } else if (startsWith(s, "[뉴스]") | startsWith(s, "[인사]") | startsWith(s, "[정부]")) {
    str <- substring(s, 6)
  } else if (startsWith(s, "[기술인 신간 소개]")){
    str <- substring(s, 13)
  } else {
    str <- s
  }
  return (str)
}

url <- "http://www.gisulin.kr/news/index.html?section=1&category=3"

# 이상하게 rvest의 read_html을 쓰면 원본하고 다른 html이 읽혀서 이상한 결과가 나온다.
# 그래서 다른 방법으로 html 파일을 읽어왔다.
response <- GET(url=url, encode = "form")

titles <- response %>%
  as.character() %>%
  read_html %>%
  html_nodes(xpath='//*[@id="sect3_articlelist"]') %>%
  html_nodes("dl") %>%
  html_nodes("dt") %>%
  html_text() %>%
  data.frame()


for (i in 2:10) {
  new_url <- str_c(url, "&page=", i)
  print(new_url)
  res <- POST(url=new_url, encode="form")
  title <- res %>%
    as.character() %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="sect3_articlelist"]') %>%
    html_nodes("dl") %>%
    html_nodes("dt") %>%
    html_text() %>% 
    data.frame()
  titles <- rbind(titles, title)
}
class(titles)
titles <- apply(titles, 1, parse_str)
titles <- data.frame(titles)
# 분석 과정
doc <- VectorSource(as.matrix(titles)) %>% Corpus
inspect(doc)

doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, stripWhitespace)

dtm <- DocumentTermMatrix(doc)
inspect(dtm)

m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = T)
d <- data.frame(word=names(v), freq=v)

pal <- brewer.pal(9, "Set1")

wordcloud(words=d$word,
          freq=d$freq,
          min.freq = 1, 
          max.words = 100,
          random.order=F,
          rot.per=0.35,
          color=pal)
# "코로나"가 가장 많이 나온 듯하다.

findAssocs(dtm, terms="코로나", corlimit=0.5)

barplot(d[1:10,]$freq,
        las=2,
        names.arg=d[1:10,]$word,
        col="grey",
        main="Top 10 Frequent Words",
        ylab="Frequency")

# Using KoNLP
useNIADic()

nouns <- extractNoun(doc)

wordcount <- table(unlist(nouns))
df_words <- as.data.frame(wordcount, stringsAsFactors = F)

# 내림차순 정렬
df_words <- df_words[order(df_words$Freq, decreasing = TRUE),]

head(df_words)

# 한글자 단어들은 제거했다
df_words <- df_words[nchar(df_words$Var1) >= 2,]
wordcloud2(df_words)
