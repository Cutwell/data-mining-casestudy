get_rss <- function(term) {
  require(tidyRSS)
  
  url <- paste("https://news.google.com/rss/search?q=", term, "&tbs=sbd:1&hl=en-GB&gl=GB&ceid=GB:en", sep="")
  
  rss <- tidyfeed(
    url,
    config = list(),
    clean_tags = FALSE,
    list = FALSE,
    parse_dates = TRUE
  )
  
  return(rss)
}
