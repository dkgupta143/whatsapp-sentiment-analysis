library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(tm)
#get the data from whatsapp chat 
s2 <- readLines("place you chat file here", n = 1000)
s2 <- iconv(s2, "UTF-8", "ASCII", sub = "")
s2 <- gsub("<Media omitted>", " ", s2)

#create the corpus
docs <- Corpus(VectorSource(s2))

#clean the chat text file
trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, trans, "/")
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, trans, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
#stopwords you don't want
docs <- tm_map(docs, removeWords, stopwords("english"))
#to remove sender names
docs <- tm_map(docs, removeWords, c("john doe","friendName2"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

#create the document term matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing=TRUE)

#Data frame
data <- data.frame(word = names(v),freq=v)
head(data, 10)


#generate the wordcloud
set.seed(1056)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


#fetch sentiment words from text
Sentiment <- get_nrc_sentiment(s2)
head(Sentiment)
s2 <- cbind(s2,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(s2[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#Print the sentiment plot
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")
