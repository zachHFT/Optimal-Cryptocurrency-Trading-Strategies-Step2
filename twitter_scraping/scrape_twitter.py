
import tweepy
import snscrape.modules.twitter as sntwitter
import pandas as pd

consumer_key = '4GbbyFnaYOCLSQqeAyeoWu2Gf'
consumer_secret = '4xqcP1zanh3e0MQMVya6POyABrcMfWFVCgSHi4QBiZqe1nTnMD'
access_token = '1455123690186739714-2UJBAHEBkM6R7eE30XsdE4L6FD898X'
access_token_secret = 'tNvRHsJoJACZoaCZ6OH2IWLc520QlY7BeDnUPIgM9cl1o'
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth, wait_on_rate_limit=True)





# Creating list to append tweet data to
tweets_list2 = []

# Using TwitterSearchScraper to scrape data and append tweets to list
for i, tweet in enumerate(
        sntwitter.TwitterSearchScraper( '(dogecoin OR DOGE) since:2016-11-10 until:2021-11-10 from:elonmusk').get_items()):
    if i > 500:
        break
    tweets_list2.append([tweet.date, tweet.id, tweet.content, tweet.user.username])

# Creating a dataframe from the tweets list above
tweets_df2 = pd.DataFrame(tweets_list2, columns=['Datetime', 'Tweet Id', 'Text', 'Username'])



tweets_df2.to_csv(r'C:\Users\MSI\PycharmProjects\pythonProject\datwitter_doge2_elon.csv')
