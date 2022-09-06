library(rtweet)
library(ggplot2)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
require(plyr)
library(dplyr)
require(stringr)

#install.packages('devtools')
#devtools::install_github('bbc/bbplot')
library(bbplot)


lims <- as.POSIXct(strptime(c("2021-04-01 00:00", "2021-12-31 23:59"), 
                            format = "%Y-%m-%d %H:%M"))

twitch_timeline_raw <- get_timeline(
  user="@Twitch",           # get tweets by @ (account)
  n=10000,                # query 50000 tweets (excluded RT and replies count towards total)
  home=FALSE,             # home parameter returns tweets from account AND people they follow, disable to return on user tweets
  exclude_replies=TRUE,   # exclude replies
  include_rts=FALSE,      # exclude retweets
  retryonratelimit=TRUE,  # twitter API limits ~3600 tweets per period. To exceed this in a single request we must wait for rate limit to reset.
  lang = "en",            # limit to English language tweets
  verbose = TRUE
)
youtube_timeline_raw <- get_timeline(
  user="@Youtube",           # get tweets by @ (account)
  n=1000000,                # query 50000 tweets (excluded RT and replies count towards total)
  home=FALSE,             # home parameter returns tweets from account AND people they follow, disable to return on user tweets
  exclude_replies=TRUE,   # exclude replies
  include_rts=FALSE,      # exclude retweets
  retryonratelimit=TRUE,  # twitter API limits ~3600 tweets per period. To exceed this in a single request we must wait for rate limit to reset.
  lang = "en",            # limit to English language tweets
  verbose = TRUE
)

twitch_timeline <- normalise_dataframe(twitch_timeline_raw)
youtube_timeline <- normalise_dataframe(youtube_timeline_raw)

twitch_rss <- get_rss("twitch")
youtube_rss <- get_rss("youtube")

twitch_rss <- twitch_rss[ , c("item_title", "item_pub_date")]
youtube_rss <- youtube_rss[ , c("item_title", "item_pub_date")]


# add new column - indicating week this tweet belongs to (by date)
twitch_timeline <- twitch_timeline %>% mutate(root_week_date=floor_date(as.Date(created_at, "%Y-%m-%d"), unit="week"))
youtube_timeline <- youtube_timeline %>% mutate(root_week_date=floor_date(as.Date(created_at, "%Y-%m-%d"), unit="week"))

twitch_timeline_aggregate <- twitch_timeline
twitch_timeline_aggregate$root_week_date <- as.Date(twitch_timeline_aggregate$root_week_date)
twitch_timeline_aggregate <- twitch_timeline_aggregate %>% group_by(root_week_date) %>% summarise(n=n())

twitch_rss_aggregate <- twitch_rss
twitch_rss_aggregate$root_week_date <- as.Date(twitch_rss_aggregate$root_week_date)
twitch_rss_aggregate <- twitch_rss_aggregate %>% group_by(root_week_date) %>% summarise(n=n())

#save(twitch_timeline, youtube_timeline, file="timeline.RData")
#save(twitch_rss, youtube_rss, file="rss.RData")


plot <- ggplot() +
  geom_line(data=twitch_timeline_aggregate, aes(x=root_week_date, y=n, color="Twitter")) +
  geom_line(data=twitch_rss_aggregate, aes(x=root_week_date, y=n, color="Google News")) +
  labs( x = "Time", 
        y = "Count",
        title = "Frequency of @Twitch Twitter statuses and Twitch-related Google News articles",
        subtitle = "Twitter status (tweet) count and Google News articles aggregated every week") +
  scale_x_date(
    date_labels = "%b-%d-%Y",
    limits = as.Date(strptime(c("2021-03-16 00:00", "2021-12-30 23:59"), format = "%Y-%m-%d %H:%M"))) +
  scale_color_manual(
    breaks = c("Twitter", "Google News"),
    values = c("#1DA1F2", "#0F9D58")) +
  bbc_style()
finalise_plot(plot, source_name = "Source: Data collected from Twitter's REST API via rtweet and Google's RSS feed via tidyRSS")

save(twitch_timeline_aggregate, twitch_rss_aggregate, file="media_vs_rss.RData")

# statistics gathered from SocialBlade 05/12/21
twitch_followers <-   8900000
youtube_followers <- 73700000

youtube_twitch_ratio <- twitch_followers / youtube_followers
twitch_youtube_ratio <- youtube_followers / twitch_followers

youtube_engagement <- engagement_by_favorites(youtube_timeline, youtube_twitch_ratio)
twitch_engagement <- engagement_by_favorites(twitch_timeline, twitch_youtube_ratio)
youtube_engagement <- youtube_engagement[ , c("created_at", "engagement_by_favorites")]
twitch_engagement <- twitch_engagement[ , c("created_at", "engagement_by_favorites")]

youtube_percentage_engagement <- engagement_by_percentage(youtube_timeline, youtube_followers)
twitch_percentage_engagement <- engagement_by_percentage(twitch_timeline, twitch_followers)
youtube_percentage_engagement <- youtube_percentage_engagement[ , c("created_at", "engagement_by_percentage")]
twitch_percentage_engagement <- twitch_percentage_engagement[ , c("created_at", "engagement_by_percentage")]

plot <- ggplot() +
  geom_point(data=twitch_engagement, aes(x=created_at, y=engagement_by_favorites, color="Twitch")) +
  geom_point(data=youtube_engagement, aes(x=created_at, y=engagement_by_favorites, color="Youtube")) +
  geom_smooth(data = twitch_engagement, aes(x=created_at, y=engagement_by_favorites), method = "lm", col = "#6441A4") +
  geom_smooth(data = youtube_engagement, aes(x=created_at, y=engagement_by_favorites), method = "lm", col = "#FF0000") +
  labs( x = "Time", 
        y = "Number of favourites",
        title = "Twitter status (tweet) favourites for @Youtube and @Twitch",
        subtitle = "Engagement of @Twitch and @Youtube statuses by of favourites normalised for follower count") +
  scale_x_datetime(limits = as.POSIXct(strptime(c("2021-09-16 00:00", "2021-12-5 23:59"), 
                                                format = "%Y-%m-%d %H:%M")), 
                   expand = c(0, 0)) +
  scale_color_manual(
    breaks = c("Twitch", "Youtube"),
    values = c("#6441A4", "#FF0000")) +
  scale_y_continuous(trans='log2') +
  bbc_style()
finalise_plot(plot, source_name = "Source: Data collected from Twitter's REST API via rtweet")

plot <- ggplot() +
  geom_line(data=twitch_percentage_engagement, aes(x=created_at, y=engagement_by_percentage, color="Twitch")) +
  geom_line(data=youtube_percentage_engagement, aes(x=created_at, y=engagement_by_percentage, color="Youtube")) +
  labs( x = "Time", 
        y = "% Engagement",
        title = "Relative engagement of @Youtube and @Twitch",
        subtitle = "Percentage engagement of @Twitch and @Youtube accounts according to favourite count as a percentage of follower count") +
  scale_x_datetime(limits = as.POSIXct(strptime(c("2021-09-16 00:00", "2021-12-5 23:59"), 
                                                format = "%Y-%m-%d %H:%M")), 
                   expand = c(0, 0)) +
  scale_color_manual(
    breaks = c("Twitch", "Youtube"),
    values = c("#6441A4", "#FF0000")) +
  bbc_style()
finalise_plot(plot, source_name = "Source: Data collected from Twitter's REST API via rtweet")

#save(youtube_engagement, twitch_engagement, youtube_percentage_engagement, twitch_percentage_engagement, file="engagement.RData")

# view top 10 tweets by favorite count
#View(youtube_timeline_raw[order(-youtube_engagement$engagement_by_favorites),][1:10,][, c("created_at", "text", "favorite_count")])
#View(twitch_timeline[order(-twitch_engagement$engagement_by_favorites),][1:10,][, c("created_at", "text", "favorite_count")])

library("lubridate")

twitch_timeline <- twitch_timeline %>% mutate(root_week_date=floor_date(as.Date(created_at, "%Y-%m-%d"), unit="week"))
youtube_timeline <- youtube_timeline %>% mutate(root_week_date=floor_date(as.Date(created_at, "%Y-%m-%d"), unit="week"))

# split timeline into week segments
twitch_timeline_weekly <- split(twitch_timeline, f = list(twitch_timeline$root_week_date))
youtube_timeline_weekly <- split(youtube_timeline, f = list(youtube_timeline$root_week_date))

# apply same to RSS data
twitch_rss <- twitch_rss %>% mutate(root_week_date=floor_date(as.Date(item_pub_date, "%Y-%m-%d"), unit="week"))
youtube_rss <- youtube_rss %>% mutate(root_week_date=floor_date(as.Date(item_pub_date, "%Y-%m-%d"), unit="week"))

twitch_rss_weekly <- split(twitch_rss, f = list(twitch_rss$root_week_date))
youtube_rss_weekly <- split(youtube_rss, f = list(youtube_rss$root_week_date))



intersection_score <- function(timeline, rss) {
  data("stop_words")
  
  output <- tibble(date=as.POSIXct(character()), score=integer(), set_score=integer(), text=character())
  
  for (df in timeline) {
    root_week_date <- df["root_week_date"][1, ][[1]]
    root_week_char <- as.character(root_week_date)
    
    df_stopped <- df %>% select(text) %>% unnest_tokens(word, text)
    df_stopped <- df_stopped %>% anti_join(stop_words)
    
    # check if rss week data exists
    if (root_week_char %in% names(rss)) {
      # get rss articles
      articles <- rss[[root_week_char]]
      articles_stopped <- articles %>% select(item_title) %>% unnest_tokens(word, item_title)
      articles_stopped <- articles_stopped %>% anti_join(stop_words)
      
      # get intersections
      similarities_score <- nrow(merge(df_stopped, articles_stopped))
      
      similarities <- intersect(df_stopped, articles_stopped)
      
      similarities_string <- paste(similarities, sep = ", ")
      similarities_string <- gsub('^c\\(', '', similarities_string)
      similarities_string <- gsub('\\)$', '', similarities_string)
      similarities_string <- gsub('"', '', similarities_string)

      output <- output %>% add_row(date=root_week_date, score=similarities_score, set_score=nrow(similarities), text=similarities_string)
    }
  }
  
  output$date <- as.POSIXct(output$date, format="%Y-%m-%d")
  
  return(output)
}


twitch_timeline_sentiment <- twitch_timeline %>% select(root_week_date, text) %>% unnest_tokens(word, text)
twitch_timeline_sentiment <- twitch_timeline_sentiment %>% anti_join(stop_words)

twitch_rss_sentiment <- twitch_rss %>% select(root_week_date, item_title) %>% unnest_tokens(word, item_title)
twitch_rss_sentiment <- twitch_rss_sentiment %>% anti_join(stop_words)

twitch_rss_bing <- twitch_rss_sentiment %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = ifelse(sentiment=='positive', 1, -1))

twitch_timeline_bing <- twitch_timeline_sentiment %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = ifelse(sentiment=='positive', 1, -1))

twitch_timeline_bing$root_week_date <- as.Date(twitch_timeline_bing$root_week_date)
twitch_timeline_bing <- aggregate(twitch_timeline_bing$sentiment, by=list(twitch_timeline_bing$root_week_date), sum)

twitch_rss_bing$root_week_date <- as.Date(twitch_rss_bing$root_week_date)
twitch_rss_bing <- aggregate(twitch_rss_bing$sentiment, by=list(twitch_rss_bing$root_week_date), sum)


plot <- ggplot() +
  geom_line(data=twitch_timeline_bing, aes(x=Group.1, y=x, color="Twitter")) +
  geom_line(data=twitch_rss_bing, aes(x=Group.1, y=x, color="Google News")) +
  labs( x = "Time", 
        y = "Sentiment",
        title = "Sentiment score between Twitter and Media",
        subtitle = "") +
  scale_x_date(
    date_labels = "%b-%d-%Y",
    limits = as.Date(strptime(c("2021-03-16 00:00", "2021-12-30 23:59"), format = "%Y-%m-%d %H:%M"))) +
  scale_color_manual(
    breaks = c("Twitter", "Google News"),
    values = c("#1DA1F2", "#0F9D58")) +
  bbc_style()
finalise_plot(plot, source_name = "Source: Data collected from Twitter's REST API via rtweet and Google's RSS feed via tidyRSS")

plot <- ggplot() +
  geom_point(data=twitch_timeline_bing, aes(x=Group.1, y=x, color="Twitter")) +
  geom_point(data=twitch_rss_bing, aes(x=Group.1, y=x, color="Google News")) +
  geom_smooth(data = twitch_timeline_bing, aes(x=Group.1, y=x, col = "Twitter"), method = "lm") +
  geom_smooth(data = twitch_rss_bing, aes(x=Group.1, y=x, col = "Google News"), method = "lm") +
  labs( x = "Time", 
        y = "Sentiment",
        title = "Sentiment score between Twitter and Media",
        subtitle = "") +
  scale_x_date(
    date_labels = "%b-%d-%Y",
    limits = as.Date(strptime(c("2021-03-16 00:00", "2021-12-30 23:59"), format = "%Y-%m-%d %H:%M")),
    expand = c(0.1, 0)) +
  scale_color_manual(
    breaks = c("Twitter", "Google News"),
    values = c("#1DA1F2", "#0F9D58")) +
  bbc_style()
finalise_plot(plot, source_name = "Source: Data collected from Twitter's REST API via rtweet and Google's RSS feed via tidyRSS")


twitch_intersection <- intersection_score(twitch_timeline_weekly, twitch_rss_weekly)
twitch_rss_aggregate_date <- twitch_rss_aggregate
twitch_rss_aggregate_date$root_week_date <- as.POSIXct(twitch_rss_aggregate_date$root_week_date, format="%Y-%m-%d")

plot <- ggplot(data=twitch_intersection, aes(x=date, y=score, color="Twitter")) +
  geom_line() +
  geom_label( 
    data = twitch_intersection %>% filter(set_score > 2), # Filter data first
    aes(label=text)
  ) +
  geom_line(data=twitch_rss_aggregate_date, aes(x=root_week_date, y=n, color="Google News")) +
  labs( x = "Time", 
        y = "Sentiment",
        title = "Intersection between Twitter and Media",
        subtitle = "Intersection score between Twitch Twitter account and media headlines for Twitch") +
  scale_x_datetime(
    date_labels = "%b-%d-%Y",
    limits = as.POSIXct(strptime(c("2021-03-16 00:00", "2021-12-30 23:59"), format = "%Y-%m-%d %H:%M")),
    expand = c(0.1, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(
    breaks = c("Twitter", "Google News"),
    values = c("#1DA1F2", "#0F9D58")) +
  bbc_style() +
  theme(legend.position = "none")
finalise_plot(plot, source_name = "Source: Data collected from Twitter's REST API via rtweet and Google's RSS feed via tidyRSS")

save(twitch_timeline_bing, twitch_rss_bing, twitch_intersection, twitch_rss_aggregate_date, file="intersection_and_sentiment.RData")









