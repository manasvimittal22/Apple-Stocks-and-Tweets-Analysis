library(quantmod)
library(tidyquant)
library(dplyr)
library(tidyverse)
library(stringr)
library(tm)
library(tidytext)
library(tokenizers)
library(stopwords)
require(plyr)
library(syuzhet)
library(wordcloud)
library(ggplot2)
library(factoextra)
library(lubridate)
library(cluster)
library(fpc)

### Get $AAPL data from Yahoo Finance API through R.
get_market_data <- function() {
  getSymbols("AAPL", src = "yahoo")
  return(AAPL)
}

### Data collection
# Collect $AAPL data from the above function
aapl_tweets <- get_market_data()
#Read the CSV which contains tweets that have major companies in the body of the Tweet.
tweets <- read.csv("tweets/Tweet.csv")

### Data cleaning
# Here, we are filtering tweets that contain the ticket symbol $AAPL in the Tweet body.
tweets %>%
  filter(grepl('$AAPL', body,fixed = TRUE)) -> aapl_tweets
aapl_tweets <- data.frame(unique(aapl_tweets$body))

# Since many tweets contain links to external articles, we are removing "http" and "https" that might 
# hamper the clustering process.

aapl_tweets$unique.aapl_tweets.body. <- gsub("http.*", "", aapl_tweets$unique.aapl_tweets.body.)
aapl_tweets$unique.aapl_tweets.body. <- gsub("https.*", "", aapl_tweets$unique.aapl_tweets.body.)

# Converting all the characters in the tweet body to lower case and removing punctuation to maintain consistency 
aapl_tweets$unique.aapl_tweets.body. <- tolower(aapl_tweets$unique.aapl_tweets.body.)
aapl_tweets$unique.aapl_tweets.body. <- removePunctuation(aapl_tweets$unique.aapl_tweets.body.)
# Removing non-alphanumeric characters
aapl_tweets$unique.aapl_tweets.body. <- str_replace_all(aapl_tweets$unique.aapl_tweets.body., "[^[:alnum:]]", " ")

# Removing stop words
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
aapl_tweets$unique.aapl_tweets.body. <- str_replace_all(aapl_tweets$unique.aapl_tweets.body., stopwords_regex, '')

# Obtain sentiment of the Tweet based on the positive and the negative words that is being read
tweet_sentiment <- get_sentiment(aapl_tweets$unique.aapl_tweets.body.)

# Word Cloud
text_corpus <- Corpus(VectorSource(aapl_tweets$unique.aapl_tweets.body.))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)
set.seed(123)
wordcloud(text_corpus, min.freq = 1, max.words = 100, scale = c(2.2,1),
          colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)

# Drop N/A and NULL values
aapl_tweets <- aapl_tweets %>%
  drop_na()

# Histogram of sentiment of tweets
neutral <- length(which(tweet_sentiment == 0))
positive <- length(which(tweet_sentiment > 1))
negative <- length(which(tweet_sentiment < 0))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("Barplot of Sentiment type of $AAPL tweets")

# Number of positive and negative tweets about $AAPL for the month of October, 2018
october_data <- data.frame(post_date = tweets$post_date, tweet = tweets$body)
october_data$post_date <- as_datetime(october_data$post_date)
october_data$post_date <- format(october_data$post_date, "%Y-%m-%d")
october_data$post_date <- as.Date(october_data$post_date)
subset_dates <- interval(start = "2018-10-01", end = "2018-10-31")
october_data <- october_data[which(october_data$post_date %within% subset_dates), ]
october_data$sentiment <- get_sentiment(october_data$tweet)

neutral <- length(which(october_data$sentiment == 0))
positive <- length(which(october_data$sentiment > 1))
negative <- length(which(october_data$sentiment < 0))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
october_output <- data.frame(Sentiment,Count)
october_output$Sentiment<-factor(october_output$Sentiment,levels=Sentiment)
ggplot(october_output, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("Barplot of Sentiment type of $AAPL tweets for the month of October, 2018")
october_kmeans_sentiment <- kmeans(data.frame(october_data$sentiment), centers = 3, nstart = 20)
plotcluster(october_data$sentiment, october_kmeans_sentiment$cluster)

# Tweet sentiment clustering
tweets_2018 <- data.frame(post_date = tweets$post_date, tweet = tweets$body)
tweets_2018$post_date <- as_datetime(tweets_2018$post_date)
tweets_2018$post_date <- format(tweets_2018$post_date, "%Y-%m-%d")
subset_dates <- interval(start = "2018-09-29", end = "2018-12-31")
tweets_2018$post_date <- as.Date(tweets_2018$post_date)
tweets_2018 <- tweets_2018[which(tweets_2018$post_date %within% subset_dates), ]
tweets_2018$sentiment <- get_sentiment(tweets_2018$tweet)
average_sentiment <- tweets_2018 %>%
 group_by(post_date) %>%
  dplyr::summarise(dplyr::across((sentiment), mean))
kmeans_sentiment <- kmeans(data.frame(average_sentiment$sentiment), centers = 3, nstart = 20)
plotcluster(average_sentiment$sentiment, kmeans_sentiment$cluster)
