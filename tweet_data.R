data_frame <- do.call("rbind", lapply(tweet_data, as.data.frame))

data_frame$text <- sapply(data_frame$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

data_frame$text <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+","",data_frame$text)

sample <- data_frame$text
