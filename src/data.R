################### START BOILERPLATE ################### 
library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
require(plyr)
require(stringr)
token <- create_token(
  app = "LG Uni Stuff",
  consumer_key = "KBK8hlcLCbF929a84qaxbsuZm",
  consumer_secret = "usoIaO80kT2QYCgXqrs91murPhmClvhcF3tE8YnDrc641iyuOk",
  access_token = "2956795121-qiovE9Foe8XAgr3Sr6EzHE4JAJI17KtAmWIkwpo",
  access_secret = "cyuYuHV3jRInbukXAURD5Z0YimtSeEvneZYmpk5d329vu")
################### END BOILERPLATE ################### 

# main function
get_account <- function(account, first_party=FALSE) {
  ###
  # Query an account to construct two data frame: account tweets, and user replies to those tweets.
  #
  # parameters:
  #   account : query string for obtaining account tweets (can be: "@<account>", "<user_id>")
  #   first_party : boolean, indicates if account uses first party Twitter, or a third party app.
  #
  # output:
  #   timeline : normalised timeline of original account tweets (excluding replies and retweets)
  #   timeline_replies : normalised data frame of replies to original account tweets (excluding retweets), grouped by original account tweet status ids
  ###
  
  # get original tweets from user (excluding RT and replies)
  timeline_raw <- get_timeline(
    user=account,           # get tweets by @ (account)
    n=10000,                # query 50000 tweets (excluded RT and replies count towards total)
    home=FALSE,             # home parameter returns tweets from account AND people they follow, disable to return on user tweets
    exclude_replies=TRUE,   # exclude replies
    include_rts=FALSE,      # exclude retweets
    retryonratelimit=TRUE,  # twitter API limits ~3600 tweets per period. To exceed this in a single request we must wait for rate limit to reset.
    lang = "en",            # limit to English language tweets
    verbose = TRUE
  )
  timeline <- normalise_dataframe(timeline_raw)                 # normalize brand tweets data set

  return(timeline)
}


# helper function
normalise_dataframe <- function(frame, first_party=FALSE) {
  ###
  # Strip noise to produce normalized data set.
  #
  # parameters:
  #   frame : data frame containing Twitter API data.
  #   first_party : boolean, indicates if account uses first party Twitter, or a third party app.
  #
  # output:
  #   frame : normalized data set.
  ###
  
  frame$text <- tolower(frame$text)             # lowercase all strings
  frame$text <- gsub("http\\S*", "", frame$text) # remove http URLs
  frame$text <- gsub("#", "", frame$text)       # remove # character, preserve tag text 
  frame$text <- gsub("@\\S*", "", frame$text)    # remove tagged usernames
  
  if (first_party) {
    # NOTE: first_party is not always desired. 
    # E.g.: @TwitchSupport brand account uses a third party app exclusively, 
    # so applying this operation would result in an empty data set.
    
    frame <- frame[                               # preserve tweets only from Twitter first party apps
      grep("Twitter", frame$source, invert = FALSE),
    ]
  }
  
  return(frame)
}

rtweet_normalise <- function(frame) {
  
  frame$text <- plain_tweets(frame$text)
  frame$text <- tolower(frame$text)
  
  frame <- get_stop_words(frame)
  
  return(frame)
}

get_stop_words <- function(frame) {
  ###
  # Remove stop words to create list for frequency calculation and sentiment analysis.
  #
  # parameters:
  #   frame : data frame containing Twitter API data.
  #
  # output:
  #   frame : set of non-stop words.
  ###
  
  # define default stopwords
  data("stop_words")
  # clean for default stopwords
  frame <- frame %>% anti_join(stop_words) # return all rows from climate_tweets_clean where there are not matching values in stop_words
  
  # define additional stopwords
  my_stop_words <- data.frame(word = c("NA", "youtube", "channel", "hey","dear","teamyoutube","twitter","t.co","video","dm", "twitch"))
  # clean for additional stopwords
  
  frame <- frame %>% anti_join(my_stop_words)
  
  return(frame)
}

get_sentiment <- function(text) {
  positive = scan('positive-words.txt', what = 'character', comment.char = ';')
  negative = scan('negative-words.txt', what = 'character', comment.char = ';')
  
  # add your list of words below as you wish if missing in above read lists
  pos.words = c(positive, 'good')
  neg.words = c(negative, 'bad')
  
  analysis <- score.sentiment(text, pos.words, neg.words)
  
  return(analysis)
}

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # we are giving vector of sentences as input. 
  # plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub() function:
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

engagement_by_favorites <- function(timeline, ratio) {
  ###
  # Add a new metric score ("engagement") which adjusts favorite count by ratio
  # 
  ###
  timeline <- timeline %>% mutate(engagement_by_favorites=favorite_count * ratio)
  
  return(timeline)
}

engagement_by_percentage <- function(timeline, follower_count) {
  timeline <- timeline %>% mutate(engagement_by_percentage=favorite_count / follower_count)
  
  return(timeline)
}




