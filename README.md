# Casestudy: Twitch/Youtube sentiment in the media and on Twitter
NLP and data mining for insights into Twitch and Youtube brand perception within media and on Twitter. 

## _Hypothesis 1: There is correlation between Follower Count and Tweet engagement_
![](.github\images\twitter_favourites.png)

* We show that, when engagement measured by favorite count is normalized proportional to the ratio of @Youtube and @Twitch brand account followers, the @Twitch brand account clearly has significantly higher relative engagement with its followers.

![](.github\images\relative_engagement.png)

* We further show that, when engagement is adjusted relative to the total followers of the @Twitch and @Youtube brand accounts, while both brands engage actively with a small percentage of their overall followers, @Twitch has higher relative engagement over @Youtube.

## _Hypothesis 2: There is correlation between media sentiment and Twitter brand sentiment_

* Tweets from a brand account will share contents with media headlines for a given time period.
* The media will reflect a similar sentiment to the brand account.

![](.github\images\sentiment_twitter_media.png)

* We show that there is negligible correlation between media and Twitter sentiment.
* This is likely due to article headlines taking a more neutral tone to appear factual, whilst Tweets are more emotive to engage with an audience.

## _Hypothesis 3: There is intersection between article headline and Twitter tweet text_

![](.github\images\intersection_twitter_media.png)

* We show that there is a high intersection between article headlines and a brand's Twitter feed.
* We also show that this correlation is relevant, as the intersection correlates with peaks in article count, indicating these intersections are not accidental.
* Notable examples for the @Twitch brand account include spikes for new features as well as controversies.
* There exists an increased volume of articles (and thus, increased intersection score) in recent weeks due to the limitations of Google's news feed.

## Limitations of collecting data using Google's RSS feed

![](.github\images\frequency_twitter_google.png)

* We collected article data for our media analysis using Google's RSS feed for the topic of "Twitch".
* Relative to the available data collected from Twitter for the @Twitch brand account, we found that the RSS feed articles were not evenly distributed over time, resulting in a concentration of articles within the last few weeks.
* Whilst this limited the scope of our analysis, the data was still provably useful.
