tweet_text <- sapply(tweet_data, function(x) x$getText())
tweet_text_df <- do.call("rbind", lapply(tweet_data, as.data.frame))
tweet_text <- sapply(tweet_text_df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

#using text mining here

library(tm)

tweet_corpus <- Corpus(VectorSource(tweet_text))

#clean text before creating the word cloud
library(wordcloud2)
clean_tweet <- tm_map(tweet_corpus, removePunctuation)
clean_tweet <- tm_map(tweet_corpus, removeWords, stopwords("english"))
clean_tweet <- tm_map(tweet_corpus, removeNumbers)
clean_tweet <- tm_map(tweet_corpus, stripWhitespace)

wordcloud(clean_tweet, random.order=F, max.words=60, col=rainbow(50), scale=c(3.5,0.75))