

####Histogram
#hist(merge_table$Positive)
#hist(merge_table$Negative)
#hist(merge_table$Score)

#Pie chart

total_pos_percent <- sum(merge_table$Positive)/(sum(merge_table$Positive)+sum(merge_table$Negative))
total_neg_percent <- sum(merge_table$Negative)/(sum(merge_table$Positive)+sum(merge_table$Negative))

slices <- c(sum(merge_table$Positive), sum(merge_table$Negative))
labels <- c(paste("Positive",round(total_pos_percent*100),"%"), paste("Negative",round(total_neg_percent*100),"%"))
colors <- c("green", "red")

library(plotrix)

pie(slices, labels = labels, col=colors, main = "Sentiment Analysis")

#3D Pie Chart

pie3D(slices, labels = labels, col=colors, explode=0.00, main = "Sentiment Analysis")
