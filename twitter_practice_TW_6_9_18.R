### tutorial guide 'Twitter Sentiment analysis using R' ###
# http://dataaspirant.com/2018/03/22/twitter-sentiment-analysis-using-r/
getwd()
setwd("/Users/callumrichards/Documents/Careers/ThamesWater_September2018/TW_EV_Social/Data")
rm(list = ls())

# Can use package to extract tweets from Twitter,‘twitteR’
library('twitteR')
#‘Syuzhet’ package will be used for sentiment analysis
# ‘tm’ and ‘SnowballC’ packages are used for text mining and analysis.
library('syuzhet')
library('tm')
library('SnowballC')
library('dplyr')
library('readr')
library('stringi')
library('textclean')

## Clean twitter data downloaded from social bearing
# load in csv that is in UTS-8 encoding in order to ensure characters such as "'", " etc 
# are read in correctly
tweet_df<-read.csv('tweet-report_socialbear_20_9_18_10days.csv', stringsAsFactors = FALSE, header=TRUE)
# 'stringsAsFactors ensures that string or characters are not automatically converted to a factor variable
attach(tweet_df)


# remove rows 1-49 to remove overview data and leave only raw data from tweets themselves
tweet_dfclean <- tweet_df[-c(1:48), ]
attach(tweet_dfclean)
# make first row of data column headers
names(tweet_dfclean) <- as.matrix(tweet_dfclean[1, ])
tweet_dfclean <- tweet_dfclean[-1, ]
colnames(tweet_dfclean)

## now clean a data and prepare to carry out your own semantic analysis on the data
# based on template code from http://dataaspirant.com/2018/03/22/twitter-sentiment-analysis-using-r/

# For 'tweet' field, it contains the tweet, hashtags, and URLs. 
# tutorial advises that we need to remove hashtags and URLs from the text field 
# so that we are left only with the main tweet part to run the sentiment analysis.
# retain the origignal field though as well though, as it may be usuful to 
# carry out some kind of analysis on URLs and in particular hashtags later

# can also the 'textclean' package
# replacing all non-ascii language such as curly single and double quotes
# install.packages('textclean')
# library('textclean')
# Encoding(tweet_dfclean$Tweet) <- "UTF-8"
# tweet_dfclean$Tweet
# Encoding="latin1"
# replace_curly_quote(tweet_dfclean$Tweet)
# tweet_dfclean$Tweet
# replace_non_ascii(tweet_dfclean$Tweet, remove.nonconverted = FALSE)
# tweet_dfclean$Tweet

# Remove URLs, hashtags and other twitter handles
# Can remove using the gsub function - although be careful as if you remove twitter
# handles using this method, seems to be removing all info after the handle featured

# filtering out emojis and other non-ASCII - You can specify character classes in regex's with [ ]. 
# When the class description starts with ^ it means everything except these characters. 
# So specifiy everything except characters 1-127, i.e. everything except standard ASCII and specify 
# that they should be replaced with the empty string. 
tweet_dfclean$tweet.txt<-gsub("[^\x01-\x7F]", "", tweet_dfclean$Tweet)
# remove URLs
tweet_dfclean$tweet.txt <- gsub("http.*","",tweet_dfclean$tweet.txt)
# remove further URLs
tweet_dfclean$tweet.txt <- gsub("https.*","",tweet_dfclean$tweet.txt)
# remove hashtags - do this again later and be careful however as do not 
# want to remove useful data eg locatinal descriptors that are surrounded by hashtags
tweet_dfclean$tweet.txt <- gsub("#.*","",tweet_dfclean$tweet.txt)
tweet_dfclean$tweet.txt
# also remove instances where users have embedded pictures and left pic address
tweet_dfclean$tweet.txt <- gsub("pic.twitter.*","",tweet_dfclean$tweet.txt)
tweet_dfclean$tweet.txt

# lets leave the option to remove further handles alone for the moment, until we
# figure out if we can retain parts of the tweet beyond the use of a handle
# tweet_dfclean$tweet.txt <- gsub("@.*","",tweet_dfclean$tweet.txt)
# Danger is that twitter handles will now be analysed for 'sentiment', when they're
# not actually a part of the user expressing themselves as such 

## Getting sentiment score for each tweet
# 'Syuzhet’ breaks emotion into 10 different emotions – anger, anticipation, 
# disgust, fear, joy, sadness, surprise, trust, negative and positive.

# first isolate our tweets into a range of character values, just focussing on
# the tweets themselves no as part of the data set
tweets.df2 <- tweet_dfclean$tweet.txt

# now utilise Syuzhet package 
word.df <- as.vector(tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df2, emotion.df) 
## need to figure out how it is defining this 'sentiment score'
# ie mechanics behind nrc
# as they're are clearly certain trigger words, and it is not always that accurate
# eg the second tweet of 'The @thameswater Customer Challenge Group's tour of
# #WalthamstowWetlands took place today with @wfcouncil &amp; @WildLondon. 
# Whilst on the guided walk, a member of the @WildLondon team found a grass snake in the meadow 
#- the grass snake is our longest snake &amp; totally harmless!"
# this was given a 'sentimennt score' of 1 anger ; 1 fear ; 1 negative ; 1 positive
# this clearly isn't that accurate and has maybe picked up on the use of 
# snake an fear as negative when in actual fact it is quite positive
# so CONTEXT is key - eg even in customer interaction tweets that will have sarcasm
# in them etc quite often

head(emotion.df2)
# The above output (ie look at datatable) shows us the different emotions present in each of the tweets.

# Can then use the get_sentiment function to extract sentiment score for each of the tweets.

sent.value <- get_sentiment(word.df)
help()
most.positive <- word.df[sent.value == max(sent.value)]
most.positive
# here the tweet featured and its high positive score is quite accurate 
# towards Thames

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative
# as is the most negative, and it also represents an example
# where this very unhappy customer also represents an account of a wastewater 
# problem, and has also given locational data 
# need to figure out how to isolate/assess this locational data

# Let's see how the score of each of the tweets has been calculated. 
# In all, there are 1488 tweets that we are evaluating, so there should be 1488 positive/negative scores, one for each of the tweets.
sent.value

# Segregating positive and negative tweets
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)
# can already see 'positivity' scores are not very clear cut 
# ' "@thameswater A TW van parks right outside my mother's driveway every 
# night and blocks her visibility. I'm certain that it'll result in her car 
# being hit as she drives out one of these days. Can anything be done?' 
# classed as positive
negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0] 
head(neutral.tweets)

## 'Alternate' way to classify as Positive, Negative or Neutral tweets
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)
# The break of total number of tweets by sentiment is
table(category_senti)

# Sentiment analysis can even be extended to a far greater extent, even to images as well.
# Are various tools available in the market to carry out such analyses as well
# which is what Thames might have used for 'Brandwatch'
# but practical knowledge of how it works under the hood is good while R also provides 
# a level of flexibility and customization eg to apply to our customer analytics tool 


### Unpacking 'syuzhet' and sentiment extraction methodolgy ### 
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html

#! article also has good info on how to visualize the data from sentiment and
# similar analysis
# See if we can customize it to suit our use of identifying customer-thames interaction language and operational categorization

# You send the word data to the get_sentiment function, which will assesses
# the sentiment of each word or sentence. This function takes two arguments:
# a character vector (of sentences or words)
# and a “method" -  determines which of the four available sentiment extraction methods to employ.
# “syuzhet” (default) method, “bing”, “afinn”, “nrc”, and “stanford”

# The documentation for the function provides bibliographic citations for the 
# dictionaries - which we may be able to manipulate to create out own dictionary
# To see the documentation, simply enter ?get_sentiment into your console.
?get_sentiment
# syuzhet" is a custom sentiment dictionary developed in the Nebraska Literary Lab.
#The default dictionary should be better tuned to fiction as the terms were extracted
# from a collection of 165,000 human coded sentences taken from a small corpus of contemporary novels
## so need to consider if you can get a dictionary of terms that is obejetionable
# for thames and its operations

## another good walkthrough that could reveal more and further, more complex analysis
##################################################################################
# From amazon AWS
# http://rstudio-pubs-static.s3.amazonaws.com/283881_efbb666d653a4eb3b0c5e5672e3446c6.html
rm(list = ls())

library(syuzhet)
library(plotly)
library(tm)
library(wordcloud)

# packages from earlier analysis aswell
library('twitteR')
library('syuzhet')
library('tm')
library('SnowballC')
library('dplyr')
library('readr')
library('stringi')
library('textclean')

setwd("/Users/callumrichards/Documents/Careers/ThamesWater_September2018/TW_EV_Social/Data")


# load data again - removing rows 1-49 to remove overview data and leave only raw data from tweets themselves
# and specifiy headers
tweet_dfclean <- tweet_df<-read.csv('tweet-report_socialbear_20_9_18_10days.csv', stringsAsFactors = FALSE,skip=49, sep=",")[ ,1:28]
attach(tweet_dfclean)

# run against a subset-sample of the first 50 tweets
tweet_dfclean<-slice(tweet_df, 1:50 )

# steps of tweet cleaning, again using gsub
# remove retweet entities
clean_tweets <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', tweet_dfclean$Tweet)
# remove at people
clean_tweets <- gsub('@\\w+', '', clean_tweets)
# remove punctuation
clean_tweets <- gsub('[[:punct:]]', '', clean_tweets)
# remove numbers
clean_tweets <- gsub('[[:digit:]]', '', clean_tweets)
# remove html links
clean_tweets <- gsub('http\\w+', '', clean_tweets)
# remove unnecessary spaces
clean_tweets <- gsub('[ \t]{2,}', '', clean_tweets)
clean_tweets <- gsub('^\\s+|\\s+$', '', clean_tweets)
# remove emojis or special characters
clean_tweets <- gsub('<.*>', '', enc2native(clean_tweets))
## also remove instances where users have embedded pictures and left pic address
clean_tweets <- gsub("pictwitter.*", '',clean_tweets)

clean_tweets = tolower(clean_tweets)
## again consider if this 'date cleansing' step is also getting rid of potentially
# useful information that may not apply to sentiment analysis
# but may still be useful for our particular application

# This time round, we would like to see if the sentiment score varies across time as well.
# To do this, we have to extract the timestamp for each tweet

# convert timestamp to standard POSIX format.
colnames(tweet_dfclean)[colnames(tweet_dfclean)=="Created At"] <- "Created.At"
format.str <- "%a %b %d %H:%M:%S %z %Y"
timestamp<-as.POSIXct(strptime(tweet_dfclean[,"Created.At"], format.str, tz = "GMT"), tz = "GMT")
timestamp <- timestamp[!duplicated(clean_tweets)]
clean_tweets <- clean_tweets[!duplicated(clean_tweets)]
clean_tweets

# subset into 

# The syuzhet package incorporates four sentiment lexicons: afinn, bing, nrc, 
# and the default syuzhet itself https://github.com/mjockers/syuzhet
# Let’s compare the four lexicons and put them into a data frame
  
# Get sentiments using the four different lexicons
syuzhet <- get_sentiment(clean_tweets, method="syuzhet")
bing <- get_sentiment(clean_tweets, method="bing")
afinn <- get_sentiment(clean_tweets, method="afinn")
nrc <- get_sentiment(clean_tweets, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc, timestamp)

# get the emotions using the NRC dictionary
emotions <- get_nrc_sentiment(clean_tweets)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
# gives counts of emotion counts

# Visualize the results
# comparing the sentiment scores across the four methods:
# plot the different sentiments from different methods

plot_ly(sentiments, x=~timestamp, y=~syuzhet, type="scatter", mode="jitter", name="syuzhet") %>%
  add_trace(y=~bing, mode="lines", name="bing") %>%
  add_trace(y=~afinn, mode="lines", name="afinn") %>%
  add_trace(y=~nrc, mode="lines", name="nrc") %>%
  layout(title="Recent sentiments of Tweets to @thameswater",
         yaxis=list(title="score"), xaxis=list(title="date"))


# Visualize the emotions from NRC sentiments
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for Tweets to @thameswater (10-20 September 2018)")


### Finally, let’s see which word contributes to which emotion:
  
# Comparison word cloud

  all = c(
    paste(clean_tweets[emotions$anger > 0], collapse=" "),
    paste(clean_tweets[emotions$anticipation > 0], collapse=" "),
    paste(clean_tweets[emotions$disgust > 0], collapse=" "),
    paste(clean_tweets[emotions$fear > 0], collapse=" "),
    paste(clean_tweets[emotions$joy > 0], collapse=" "),
    paste(clean_tweets[emotions$sadness > 0], collapse=" "),
    paste(clean_tweets[emotions$surprise > 0], collapse=" "),
    paste(clean_tweets[emotions$trust > 0], collapse=" ")
  )
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))


# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]

# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
# elements in this matrix represent the occurrence of a term (a word or an n-gram) in a document of the corpus.
# so is nice overview of what words are being classified under which 'emotional'
# category in the data set
# can also use to see nice overview over words being classified under categories
# within a custom dictionary as well 

# construct WordCloud
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)


# As a note, I need to exclude words with more than 11 characters so that the 
# words can fit nicely into the wordcloud. In practice we can also shorten these 
# long words instead of removing them.