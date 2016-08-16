setwd("Documents/cug4063/")

library(streamR)
tweet.df <- parseTweets("tweets_BS.json", simplify = TRUE)

## a trial from documents online
library(ggplot2)
library(grid)
library(maps)
map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweet.df$lon), y = as.numeric(tweet.df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")

## 
library(tm)
library(SnowballC)
library(wordcloud)
TweetCorpus <- paste(unlist(tweet.df$text), collapse =" ")
TweetCorpus <- Corpus(VectorSource(TweetCorpus))
TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords('english'))
TweetCorpus <- tm_map(TweetCorpus, stemDocument)
TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
#TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)
