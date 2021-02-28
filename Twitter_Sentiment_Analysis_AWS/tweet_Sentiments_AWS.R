---
  title: "Analyzing Tweet Sentiments using AWS Comprehend"
author: "Fasih Atif"
date: "12/5/2020"
output: html_document
---
  
 
install.packages("rtweet")
install.packages("aws.translate", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))


library(rtweet)
library(tidyverse)
library(aws.translate)
library(aws.comprehend)
library(data.table)
```

# whatever name you assigned to your created app
appname <- "fasihatif"

## api key (example below is not a real key)
key <- "P1RQTIVJpQ8cC8LbGYvHdjuAv"

## api secret (example below is not a real key)
secret <- "dJIbdSycSfjB1yMxT5lwmCY4rHzPGuHNoKQvgulLTCGwcdWhHI"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = "449573277-WOjsvtLNMPvS0irkL0c1hLZGEQ4fDHPvh9yraPtg",
  access_secret = "6ewo8RNZUHI2XFLMwOTicDo9wpwUvNHVm3UkaUMtEv2ir")



keyTable <- read.csv("accessKeys.csv", header = T) # accessKeys.csv == the CSV downloaded from AWS containing your Acces & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

tweets_all <- search_tweets(q = "GlobalWarming  -filter:retweets -filter:quote",
                            n = 3000, type = "mixed", include_rts = FALSE)

tweets_all <- rename(tweets_all, "tweets" = "text")

ts_plot(tweets_all, by = "hours") +
  ggtitle("Frequency of Global Warming tweets over past week")




tweetsDf <- tweets_all %>% select(tweets)



tweetsDf$tweets <- gsub("http[[:alnum:][:punct:]]*", "", tweetsDf$tweets) #Remove https and URLS

tweetsDf$tweets <- enc2native(tweetsDf$tweets) # Covnert emojis to native encoding
tweetsDf$tweets <- gsub("<.*.>", "", tweetsDf$tweets)

tweetsDf$tweets <- trimws(tweetsDf$tweets) # Remove leading whitespaces from the beginning
tweetsDf$tweets <- gsub('@\\S+', '', tweetsDf$tweets)
#tweetsDf$tweets <- str_remove_all(tweetsDf$tweets,"@[[:alnum:]]+")
#tweetsDf$tweets <- gsub("^@$","", tweetsDf$tweets) # Remove sole @
tweetsDf$tweets <- plain_tweets(tweetsDf$tweets)

tweetsDf <- unique(tweetsDf) #removing a big chunk of duplicate tweets to remove bias and diversify tweet sample


tweetsDf <- tweetsDf %>% 
  filter(nchar(tweets)!=0)


# Sentiment Detection
sentiment_func <- function(x){
  
  sentiment_type = detect_sentiment(x) %>% pull(Sentiment)
  return(sentiment_type)
}

sentiment_data <- data.frame(unlist(unname(lapply(tweetsDf$tweets,sentiment_func))))
sentiment_df <- cbind(tweetsDf$tweets,sentiment_data)
colnames(sentiment_df) <- c("tweet","sentiment")

# Visualization for sentiments of tweets
sentiment_df %>%
  group_by(sentiment) %>%
  summarize(count =n()) %>%
  ggplot(aes(x = sentiment, y = count, fill = sentiment, label = count)) +
  geom_col() + geom_text(vjust = -0.5) +
  ggtitle("Tweets by Sentiments")


entity_func <- function(x){
  
  entity_type <- detect_phrases(x)
  return(entity_type)
}


entity_data <- unname(sapply(tweetsDf$tweets,entity_func))
entity_df <- cbind(tweetsDf$tweets, entity_data)
colnames(sentiment_df) <- c("tweet","sentiment")

# Entity Detection
entity <- function(row, df) {
  record <- tweetsDf[row,]
  entities <- detect_entities(as.character(record$tweets))   # Get sentiment from Amazon's Comprehemd
  merged <- merge(entities, record) # Merge the sentiment result to the original data
  return (merged)}
row_seq <- seq(1,nrow(tweetsDf)) # Define argument for lapply
entities <- lapply(row_seq, entity, df=tweetsDf)
entities_df <- rbindlist(entities, fill = TRUE) # Append list of dataframes together

data <- entities_df[!duplicated(entities_df$tweets), ]

data %>%
  filter(!is.na(Text)) %>%
  filter(!Type == "QUANTITY") %>%
  filter(!Type == "DATE") %>%
  filter(!Text == '@') %>%
  filter(!Text == '/') %>%
  filter(!Text == '@PakuloPapi') %>%
  group_by(Text) %>%
  summarize(count = n()) %>%
  arrange(-count) %>% 
  head(5) %>%
  ggplot(aes(x = count, y = Text, fill = Text)) +
  geom_col() + labs(x = "count", y ="Entity", title = "Most entities mentioned on Twitter in reference to Global Warming")


# Proportion of entities mentioned on Twitter in reference to Global Warming
data %>%
  filter(!is.na(Text)) %>%
  filter(!Type == "QUANTITY") %>%
  filter(!Type == "OTHER") %>%
  filter(!Type == "COMMERCIAL_ITEM") %>%
  filter(!Type == "DATE") %>%
  filter(!Text == '@') %>%
  filter(!Text == '/') %>%
  filter(!Text == '@PakuloPapi') %>%
  group_by(Type) %>%
  summarize(count = n()) %>%
  arrange(-count) %>% 
  ggplot(aes(x = '',y = count, fill = Type, label = count)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(position = position_stack(vjust = 0.5)) +
  ggtitle("Proportion of Entities mentioned on Twitter in reference to Global Warming")


