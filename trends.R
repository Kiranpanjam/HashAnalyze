locations <- availableTrendLocations()

wid <- locations[which(locations$name == "San Diego"), 390]

total_trends <- getTrends(wid)

trends <- total_trends[1:2]

#data cleaning

top_trends  <- cbind(trends$name)
top_trends <- unlist(strsplit(top_trends, split = ", "))


