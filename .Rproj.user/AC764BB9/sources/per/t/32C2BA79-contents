library(twitteR)
library(ROAuth)
library(RCurl)
library(installr)

#Twitter API Authentication

api_key <- "INKPhh0mCS9Yxte7a8QVZHrAJ"
api_key_secret <- "rQ56j10RSIwic76Jq0tttlxNIAlLhMBOz69R4Q6M1FvYfLpjuS"
access_token <- "1346604298629574656-bivKOwgjGQmxyJHF81MJz6Bvl1yD7S"
access_token_secret <- "E18VXrehdQcChd7kaHJq2DMHYUBwDZzQTos1dFluHWAsH"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #downloads the certificate

setup_twitter_oauth(api_key, api_key_secret, access_token, access_token_secret)

#Search twitter

tweet_data = searchTwitter('#MasterFilm', n = 1000)


#tweet_data data frame

data_frame <- do.call("rbind", lapply(tweet_data, as.data.frame))

data_frame$text <- sapply(data_frame$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

data_frame$text <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+","",data_frame$text)

sample <- data_frame$text

#match the data with positive and negative words

positive_words <- scan("C:/Users/panja/Desktop/ISA/IS 470/Tweet/positive.txt", what="character", comment.char=";")

negative_words <- scan("C:/Users/panja/Desktop/ISA/IS 470/Tweet/negative.txt", what="character", comment.char=";")


#score sentiment function 

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    sentence = gsub('\n','',sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}


#merge the data into single table with text, score, postive and negative value only

result = score.sentiment(sample, positive_words, negative_words)

library(reshape2)

data1 <- result[[1]]
data2 <- result[[2]]
data3 <- result[[3]]

data1$text <- NULL
data2$text <- NULL
data3$text <- NULL

s1 <- data1[1,]
s2 <- data2[1,]
s3 <- data3[1,]

s1_melt <- melt(s1, var='Score') 
s2_melt <- melt(s2, var='Positive')
s3_melt <- melt(s3, var='Negative')

s1_melt$Score <- NULL
s2_melt$Positive <- NULL
s3_melt$Negative <- NULL

table1 <- data.frame(result[[1]]$text, s1_melt)
table2 <- data.frame(result[[2]]$text, s2_melt)
table3 <- data.frame(result[[3]]$text, s3_melt)

merge_table <- data.frame(Text=table1$result..1...text, Score=table1$value, Positive=table2$value, Negative=table3$value )



##################################################


merge_table$Posivite_Percentage = merge_table$Positive/(merge_table$Positive+merge_table$Negative)

merge_table$Negative_Percentage = merge_table$Negative/(merge_table$Positive+merge_table$Negative)



merge_table$Posivite_Percentage[is.nan(merge_table$Posivite_Percentage)] <- 0
merge_table$Negative_Percentage[is.nan(merge_table$Negative_Percentage)] <- 0