### CLEAR ENVIRONMENT
rm(list = ls())

getwd()
setwd("/Users/callumrichards/Documents/Careers/ThamesWater_September2018/TW_EV_Social/Data")

# Required packages
library(twitteR)
library(syuzhet)
library(tm)
library(SnowballC)
library(dplyr)
library(readr)
library(stringi)
library(textclean)
library(plotly)
library(wordcloud)
library(pacman)
library(sentimentr)
library(magrittr)
library(data.table)


## Have utilise sentiment analysis at
# http://dataaspirant.com/2018/03/22/twitter-sentiment-analysis-using-r/
# and https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
# to carry out practice sentiment analysis on twitter data downloaded from social.bearing

# Attempt to pull dictionary from syuzhet or similar package and manipulate
# it for our own key words that have TW operational relevance
# ie using a custom dictionary/lexicon

# 'sentimentr' package (https://github.com/trinker/sentimentr#making-and-updating-dictionaries)
# offers the capability to make and update your own dictionaries
# (materials are also in a download file from GitHub)
# very useful article to understand the nuts and bolts behind sentiment
# analysis and good points to consider for methodological approach 
# while also launching the debate around the effectiveness of different
# semantic analysis tools such as syuzhet etc 
# eg http://www.matthewjockers.net/2015/03/04/some-thoughts-on-annies-thoughts-about-syuzhet/


#  Incorporates weighting for valence shifters 
# (negation and amplifiers/deamplifiers)
# Eg/ A negator flips the sign of a polarized word (e.g., "I do *not* like it."). 
# lexicon::hash_valence_shifters[y==1] for examples. 
lexicon::hash_valence_shifters[y==1]
# An amplifier (intensifier) increases the impact of a polarized word (e.g., "I really like it."). 
lexicon::hash_valence_shifters[y==2]
### etc etc on article
# these valence shifts are important eg In the case of negators and adversative 
# conjunctions the entire sentiment of the clause may be reversed or overruled. 
# and a simple dictionary lookup may not be modeling the sentiment appropriately.

### also has the 'replace_emoticon' function, that replaces emoticons with word equivalent
# so you dont lose data when customers use emoticons 

# PROCESSING WORFLOW

# load data again
# specifying which rows and columns you want to load in 
tweet_df<-read.csv('tweet-report_socialbear_20_9_18_10days.csv', stringsAsFactors = FALSE,skip=49, sep=",")[ ,1:28]
# 'stringsAsFactors ensures that string or characters are not automatically converted to a factor variable
attach(tweet_df)

# steps of tweet cleaning, again using gsub to get raw sentances
# Replace blank space (“rt”) # removes rt, but does not completely remove retweeted tweets
tweet_df$clean_tweets <- gsub("rt", "", Tweet)
# Replace @UserName
tweet_df$clean_tweets  <- gsub("@\\w+", "", tweet_df$clean_tweets )
# removes @thames water (obviously they will all have this present as this sample is from twets to @thameswater)
# Remove punctuation
tweet_df$clean_tweets <- gsub("[[:punct:]]", "", tweet_df$clean_tweets)
# Remove links
tweet_df$clean_tweets <- gsub("http\\w+", "", tweet_df$clean_tweets)
# Remove blank spaces at the beginning
tweet_df$clean_tweets <- gsub("^ ", "", tweet_df$clean_tweets)
# Remove blank spaces at the end
tweet_df$clean_tweets <- gsub(" $", "", tweet_df$clean_tweets)
# remove emojis or special characters 
## !!!! this also removed hashtags - so if you want to keep hashtags in then find a different function
tweet_df$clean_tweets <- gsub('<.*>', '', enc2native(tweet_df$clean_tweets))
# also remove instances where users have embedded pictures and left pic address
tweet_df$clean_tweets <- gsub("pictwitter.*", '', tweet_df$clean_tweets)

tweet_df$clean_tweets = tolower(tweet_df$clean_tweets)
attach(tweet_df)
## again consider if this 'date cleansing' step is also getting rid of potentially
# useful information that may not apply to sentiment analysis
# but may still be useful for our particular application
# eg can try and run with the 'replace_emoticon' function to conver
# emoticons to word expressed emotions
head(clean_tweets)

# Split your text data into sentences - 'sentence boundary disambiguation'
# via the get_sentences function. 
# This can be handled within sentiment (i.e., you can pass a raw character 
# vector) but it slows the function down and should be done one time rather 
# than every time the function is called. Additionally, a warning will be thrown 
# if a larger raw character vector is passed. The preferred workflow is to spit 
# the text into sentences with get_sentences before any sentiment analysis is done.
clean_tweets <- get_sentences(clean_tweets)
# ie this is treating the text as if the tweets are one big long, conjoined text 
# this is a way to treat the data, as long as you can then identify invididual 
# twitter handles etc
#### NEED TO THINK ABOUT THE TREATMENT OF THE DATA FOR THIS, AND SEE IF IT CAN 
# APPLY TO TWITTER DATA

# ie this function may not be breaking the text up as it should, ie by handle

# Access sentiment scores
sentiment(clean_tweets)


## can also aggregate by grouping variables use sentiment_by using the by argument.
# eg group via timestamp and user's handle
# first convert timestamp of each tweet to readable format
colnames(tweet_df)[colnames(tweet_df)=="Created At"] <- "Created.At"
format.str <- "%a %b %d %H:%M:%S %z %Y"
tweet_df$timestamp<-as.POSIXct(strptime(tweet_df[,"Created.At"], format.str, tz = "GMT"), tz = "GMT")
attach(tweet_df)
length(timestamp)

# using sentiment by, allows you to determine the grouping variable, setting
# by = NULL will go to default or using the original rows/indices in the dataframe
# which is what we want - or you could specify by handle etc
help(sentiment_by)

(out <- with(
  tweet_df, 
  sentiment_by(
    get_sentences(clean_tweets), 
    list(Handle, timestamp)
  )
))
# this is now ordered in alphabetical of handle - and I think it is aggregating by rows

## Plotting at Aggregated Sentiment
plot(out)
# plots sentiment of tweets by groups. ie grouping the tweets from each unique
# twitter handle
# because there are so many handles that have tweeted thames, then plot is extremely messy
# so we may have to group handles somehow, or a change to the dictionary may also 
# alter how the plot looks
# or break up the data to focus on only a subset - for ease of presentation etc

## But this is quite cool, as you could in 'theory' plot 'operational sentiment'
# over time, from a set of tweets, showing how the polarity shifts over time, as well
# as variation over different handles (as shown)
# and if you could geotag tweets, you could attempt to show how the operational
# relevance of different tweets changes spatially eg in one DMA compared to another
# and therefore indicate  'strain/demand' put on different parts of TW operations
# IF you can somehow pull geotagged information from tweets.
# and you can assess the sentiment of different isolate tweets

## Plotting at the sentance level
# The plot method for the class sentiment uses syuzhet's get_transformed_values combined with 
# ggplot2 to make a reasonable, smoothed plot for the duration of the text based on percentage, 
# allowing for comparison between plots of different texts, giving the overall shape of the text's sentiment.
# See syuzhet::get_transformed_values for more details.
plot(uncombine(out))
# shows sentiment as you're going through the text ie as if all the 
# tweets are lined up as one long sentance.  - not that useful to us in this
# format as we want to isolate handles/customers

### Making your own dictionary

# Positive  and negative words within input sentances are tagged with a +1 and −1 
#respectively,
# or a positive/negative weighting that the user can determine if the user
# provides the sentiment dictionary).
# and you can even provide a weight (z) to be utilized with amplifiers/de-amplifiers 
# (default is .8; de-amplifier weight is constrained to −1 lower bound).

# To create a key from scratch need to create a 2 column data.frame
# with words on the left and values on the right 
# (see ?lexicon::hash_sentiment_jockers_rinker 
#& ?lexicon::hash_valence_shifters for what the values mean). 

### EG 

set.seed(10) # produce 10 random numbers
help(set.seed)
key <- data.frame(
  words = sample(letters),
  polarity = rnorm(26),
  stringsAsFactors = FALSE
)

# This is not yet a key. 
# sentimentr provides the is_key function to test if a table is a key.
is_key(key)

# needs to be data.table-ified. 
# use as_key function to coerce a data.frame to a data.table 
# left column named x and the right column named y. 
# function also checks the key against another key to make sure there is not overlap using the compare argument. 
# By default as_key checks against valence_shifters_table
# ASSUMING the user is creating a sentiment dictionary.
# If the user is creating a valence shifter key then a sentiment key needs to be passed to compare instead and set the argument sentiment = FALSE.

mykey <- as_key(key)
is_key(mykey)
# The key is ready for use
# And you can also visualise whats in the key, 
# seeing the values of a key that correspond to a word using data.table syntax
mykey[c("a", "b")][[2]]
# so these 'scores' correspond to a and b

## so for our 'OPERATIONS KEY' you can set each word you identify
# as relevant to a different operation , as belonging to the same 'score'
# This will create very discrete categories, with all words under one operational
# category sharing the same score
# It is also possible to score words on an operational 'continuum', creating 
# a scoring scale where certain words (or even phrases, if you use valence)
# can be related very much/high risk to a certain operational department
# and therefore score very highlighy, and others that are not as 'high risk'
# and therefore score lower -> this step can therefore incorporate
#' level of risk' and make this tool very powerful

########################################################################
# BUT it does show how important the design of your dictionary/key is,
# and you should really think carefully about its designv (!!!!!!) 
#######################################################################

# Can also update the key with 'update_key function'
# so can add or drop terms via the x (add a data.frame) and drop (drop a term) arguments. 
# so you do not have to create a whole new key everytime

mykey_dropped <- update_key(mykey, drop = c("a", "h"))
nrow(mykey_dropped)

# add 'dog' and 'cat' and their sentiment values to the key
mykey_added <- update_key(mykey, x = data.frame(x = c("dog", "cat"), y = c(1, -1)))


### Forming my own 'operational key'
## lets have a look through the tweets and phrases used - going through an initial
# 'hand-scoring' for operational relevance
# eg like approach used by Rinker on the Git Hub code that used Mean Directional
# Accuracy (MDA) and merely matches the sign of an algorithm to the human coded 
# output to determine accuracy rate of the algorithm against a human coded method
# (more info on MDA https://en.wikipedia.org/wiki/Mean_Directional_Accuracy)
# a measure of prediction accuracy of a forecasting method in statistics, 
# comparing the forecast direction (upward or downward) to the actual realized 
# direction.

# create a copy of the cleaned data table for column deletion etc in prep for printing
tweet_df.2 <- copy(tweet_df)

# remove uneccessary columns that are a bit overkill for R - using subsetting

tweet_df.3 <- subset(tweet_df.2, select = -c(Screen.Name,Created.At,Links, Language,
                                             Source, Domain, URL, Tweet.Link, Pic.Link,
                                             Retweeting.User, Retweeting.Handle, Tweet.ID) )
# move 'timestamp' column to 2nd column in subset
# and clean_tweet to 4th 
tweet_df.3<-tweet_df.3[,c(1,18,2,17,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

# export 'cleaned' table as csv for printing
write.csv(tweet_df.3, file = "tweets_cleaned_28_9_18.csv", row.names=FALSE, fileEncoding = "UTF-8")

## BUILD A CONCEPT DICTIONARY/KEY
set.seed(10) # produce 10 random numbers
help(set.seed)

help("data.frame")


# set up a demo key with 4 operational categories - scored in a discrete manner
# to begin with
# 1 = supply and distribution
# 2 = revnue
# 3 = wastewater
# 4 = fleet operations

operat.key <- data.frame(
 Words = c('supplying', 'supply', 'leak', 'potholes', 'spouting', 'roadworks',
           'pouring', 'delay', 'delays', 'digging', 'dig', 'fix',
           'account', 'customer', 'bill', 'reference', 'payment', 'charged', 
           'charge', 'sewer', 'sewage','waste', 'repair', 'manhole', 'van', 
           'contractor'),
  Operational.Relevance = c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,4,4),
  stringsAsFactors = FALSE
)

### could we attempt word based lexicon? (chr. key was not compatible with syuzhet approach below)
#
operat.key.words <- data.frame(
  Words = c('supplying', 'supply', 'leak', 'potholes', 'spouting', 'roadworks',
           'pouring', 'delay', 'delays', 'digging', 'dig', 'fix',
           'account', 'customer', 'bill', 'reference', 'payment', 'charged', 
           'charge', 'sewer', 'sewage','waste', 'repair', 'manhole', 'van', 
           'contractor'),
  Operational.Relevance = c('supply','supply','supply','supply','supply','supply','supply','supply','supply',
            'supply','supply','supply','revenue','revenue','revenue','revenue','revenue',
            'revenue','revenue','waste','waste','waste','waste','waste','fleet','fleet'),
  stringsAsFactors = FALSE
)

# convert to  data.table, using as_key function to coerce data.frame to a 
# data.table 
# left column named x and the right column named y. 
# function also checks the key against another key to make sure there is not overlap using the compare argument. 
# By default as_key checks against valence_shifters_table
# ASSUMING the user is creating a sentiment dictionary.
# If the user is creating a valence shifter key then a sentiment key needs to be passed to compare instead and set the argument sentiment = FALSE.
operat.key.demo <- as_key(operat.key)
is_key(operat.key.demo)
# The key is ready for use
# And you can also visualise whats in the key, 
# seeing the values of a key that correspond to a word using data.table syntax

# run against a sample of the first 50 tweets
tweets_50<-slice(tweet_df, 1:50 )

# run sentiment_by function against this subset of tweets with the new keys

(demo <- with(
  tweets_50, 
  sentiment_by(
    get_sentences(clean_tweets), 
    list(Handle, timestamp), polarity_dt = operat.key.demo
  )
))

plot(demo)

# text highlighting
set.seed(2)

tweets_50 %>%
  filter(timestamp %in% sample(unique(timestamp), 20)) %>%
  mutate(review = get_sentences(clean_tweets)) %$%
  sentiment_by(clean_tweets, timestamp) %>%
  highlight()

# assessing output (demo and plot)shows that the ave.sentiment is not a definitive 1,2,3,4, 
# as the function is assessing the whole tweet, and averaging sentiment out across the tweet
# including a score of 0 if there is no dicitionary based words - so is giving
# a 'grade' of operational relevance

### Text Highlighting base code ####################
###################################################
### NOTE that this package and code repository also allows very useful funciton of
# 'Text highlighting'
# Allowing you to see the output from sentiment_by line by line with 
# positive/negative sentences highlighted. 
# The highlight function wraps a sentiment_by output to produces a highlighted HTML file that opens in your web browser 
# (positive = green; negative = pink). 
# eg looking at 3 random reviews from Hu and Liu's (2004) Cannon G3 Camera Amazon product reviews.
library(magrittr)
library(dplyr)
set.seed(2)

cannon_reviews %>%
  filter(number %in% sample(unique(number), 3)) %>%
  mutate(review = get_sentences(text)) %$%
  sentiment_by(review, number) %>%
  highlight()

# try with our data
(out <- with(
  tweet_df, 
  sentiment_by(
    get_sentences(clean_tweets), 
    list(Handle, timestamp)
  )
))

set.seed(2)

tweet_df %>%
  filter(timestamp %in% sample(unique(timestamp), 10)) %>%
  mutate(review = get_sentences(clean_tweets)) %$%
  sentiment_by(clean_tweets, timestamp) %>%
  highlight()
# also gives you a good idea of what kind of words and phrases are being
# designated to certain keys


####### let's try a slightly different approach, using Syuzhet package ##########
######## forgiving its apparent failings, as pointed out by Rinker     ###############################

## Updated protocol for using Syuzhet package - following intro from Jockers
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
rm(list = ls())

library('zoo')
library('panda')

# load up data again and clean
tweet_df<-read.csv('tweet-report_socialbear_20_9_18_10days.csv', stringsAsFactors = FALSE,skip=49, sep=",")[ ,1:28]
# 'stringsAsFactors ensures that string or characters are not automatically converted to a factor variable
attach(tweet_df)

# numer of measures to get a good look at text data (advice from https://www.datacamp.com/community/blog/text-mining-in-r-and-python-tips)

# steps of tweet cleaning, again using gsub to get raw sentances
# remove retweet entities
tweet_df$clean_tweets <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', Tweet)
# remove at people
tweet_df$clean_tweets <- gsub('@\\w+', '', tweet_df$clean_tweets)
# remove punctuation
tweet_df$clean_tweets <- gsub('[[:punct:]]', '', tweet_df$clean_tweets)
# remove numbers
tweet_df$clean_tweets <- gsub('[[:digit:]]', '', tweet_df$clean_tweets)
# remove html links
tweet_df$clean_tweets <- gsub('http\\w+', '', tweet_df$clean_tweets)
# remove unnecessary spaces
tweet_df$clean_tweets <- gsub('[ \t]{2,}', '', tweet_df$clean_tweets)
tweet_df$clean_tweets <- gsub('^\\s+|\\s+$', '', tweet_df$clean_tweets)
# remove emojis or special characters
tweet_df$clean_tweets <- gsub('<.*>', '', enc2native(tweet_df$clean_tweets))
## also remove instances where users have embedded pictures and left pic address
tweet_df$clean_tweets <- gsub("pictwitter.*", '', tweet_df$clean_tweets)

tweet_df$clean_tweets = tolower(tweet_df$clean_tweets)
attach(tweet_df)


# Tutorial suggests to again parse text into a vector of sentances - using the 
# get_sentences() function which implements the openNLP sentence tokenizer.
#  eg sentances<- get_sentences(my_example_text)
# however, since our tweets are already seperated into 'tweets' rather than defined sentances
# we can retain the clean_tweets chr.vector, without convertin it to sentances
# producing a character vector, containing 1488 items, each actually an individual
# tweet rather than a sentance'
class(clean_tweets)
str(clean_tweets)
head(clean_tweets)

# get_text_as_string function may be useful if you wish to load a larger file. eg book/chapter text
# get_tokens function also allows you to tokenize by words instead of sentences.
# You can enter a custom regular expression for defining word boundaries. By default, the function uses the “\W” regex to identify word boundaries. 

# Send 'sentances' (or tweet text) to the get_sentiment function to asses the 
# sentiment of each tweet or sentence.
# function takes two arguments: a character vector (of sentences or words) 
# and a “method.” (sentiment extraction method:  “syuzhet” (default); bing”, “afinn”, “nrc”, or “stanford”)
# again, see documentation for bibliographies on this methods

syuzhet_vector <- get_sentiment(clean_tweets, method="syuzhet")
# syuzhet_vector contains a set of 1488 corresponding to 1488 tweets, with the values
# representing the model's assessment of the sentiment in each sentance
head(syuzhet_vector)

# different methods will return slightly different results, with each method
# using a slightly different scale.

bing_vector <- get_sentiment(clean_tweets, method="bing")
head(bing_vector)

afinn_vector <- get_sentiment(clean_tweets, method="afinn")
head(afinn_vector)

nrc_vector <- get_sentiment(clean_tweets, method="nrc", lang = "english")
head(nrc_vector)

# Stanford Example: Requires installation of coreNLP and path to directory
# tagger_path <- "/Applications/stanford-corenlp-full-2014-01-04"
# stanford_vector <- get_sentiment(poa_v, method="stanford", tagger_path)
# head(stanford_vector)

# compare different method's scores using R’s built in sign function. 
# The sign function converts all positive number to 1, all negative numbers to
# -1 and all zeros remain 0.

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)),
  sign(head(nrc_vector))
)

# can then to sum the values from to get an overall measure emotional valence 
# across the whole set of tweets
sum(syuzhet_vector)
# not that informative, but 581.55 is positive, indicting tweets are generally considered
# positive through this syuzhet measure

# or can consider the central tendency, the mean emotional valence 
mean(syuzhet_vector)
# other summary statistics to offer a better sense of how the emotions in the 
# overall set (1488) of tweets are distributed, getting a broad sense of how sentiment
# (or operational relevance) is distributed through the text
summary(syuzhet_vector)

# Now into how these positive and negative sentiments, or operational relevance
# is activated across the text.

# eg May be useful to plot the values in a graph where the x-axis represents the passage of time from 
# the beginning of the twitter period to the end y-axis measures the degrees of 
# positive and negative sentiment, or can do so via operational relevance
# (although depends how discrete the variables are)
plot(
  syuzhet_vector, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# because we have such as large chunk of text, from all the tweets taken as a whole,
# the plot is difficult to interpret, although is typical for large scale text
# or tweet analysis where we therefore get a lot of 'affectual noise'

# This raw data may be useful for certain applications, but can make it easier to 
# visualize by removing noise and reveal the simple shape of the trajectory
# eg applying a trend line. The next plot applies a moving average trend line to the simple example text containing twelve sentences.

# Can also use the get_percentage_values function to divide a text into an equal 
# number of “chunks” and then calculates the mean sentiment valence for each. 
# In the example below, the sentiments from twitter data are binned into 15 chunks and then plotted.
# # and can use to control how many sentences are included inside each percentage based chunk:

percent_vals <- get_percentage_values(syuzhet_vector, bins = 15)
plot(
  percent_vals, 
  type="l", 
  main="Example Plot Trajectory 2", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

# bear in mind you should carefully consider how you bin the text and rationale,
# with extremes of emotional valence tending to get watered down when a series of 
# sentance values are combined into larger chunks
# especially true when the segments of text that percentage based chunking returns are especially large.
# with the means of longer passages tending to converge toward 0

## can alternatively use transformations
# eg using Discrete Cosine Transformations in combination with a low pass filter.
# (various arguments of the function are described in the documentation)
help("get_dct_transform")
dct_values <- get_dct_transform(
  syuzhet_vector, 
  low_pass_size = 5, 
  x_reverse_len = 1488,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main= 'Transformed Grapth',  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
# shows positivity increasing in further tweets, that are acutally going back in time

## The 'simple_plot function' takes a sentiment vector and applies three smoothing methods. 
# include a moving average, loess, and discrete cosine transformation. 
# Producing two plots stacked. The first shows all three smoothing methods on the same graph. 
# The second graph shows only the DCT smoothed line, but does so on a normalized time axis. 
install.packages('zoo')
library(zoo)
windows()
par("mar")
par(mar=c(1,1,1,1)) # make sure plot window can accomodate the plot
simple_plot(syuzhet_vector)

# can though use the 'rescale_x_2' function to re-scale values to a normalized x and y 
# axis. This is useful for comparing two sentiment arcs from a text / set of tweets

# code also displays how to make use of 'get_nrc_sentiment' to implement Saif 
# Mohammad’s NRC Emotion lexicon (See http://www.purl.org/net/NRCemotionlexicon).
## plotting and display methods may be usuful when using our own dictionary


###### USING CUSTOM SENTIMENT LEXICONS WITH SYUZHET #####

# You need to create your custom lexicon as a data frame 
# with at least two columns named “word” and “value.”

# lets act it on a subset of data, of the first 50 tweets
tweets_50<-slice(tweet_df, 1:50 )
class(tweets_50$clean_tweets)
str(tweets_50$clean_tweets)

method <- "custom"

# set up custom lexicon as a dataframe, using same dictionary as attempted with
# sentimentr above
#demo key with 4 operational categories - scored in a discrete manner
# to begin with (ie 'value' within custom_lexicon dataframe)
# 1 = supply and distribution
# 2 = revenue
# 3 = wastewater
# 4 = fleet operations

custom_lexicon <- data.frame(
  word = c('supplying', 'supply', 'leak', 'potholes', 'spouting', 'roadworks',
            'pouring', 'delay', 'delays', 'digging', 'dig', 'fix',
            'account', 'customer', 'bill', 'reference', 'payment', 'charged', 
            'charge', 'sewer', 'sewage','waste', 'repair', 'manhole', 'van', 
            'contractor'),
  value = c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,4,4),
  stringsAsFactors = FALSE
)

# measure sentiment
syuzhet_vector_50 <- get_sentiment(tweets_50$clean_tweets, method=method,
                                   lexicon= custom_lexicon)
syuzhet_vector_50

plot(
  syuzhet_vector_50, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
# the custom method here has allocated values that we did not perscibe within the lexicon
# not sure how its calculated it  - may need to investigate the use of custom lexicons further
### MAY BE BETTER TO FOLLOW A TEXT MINING APPROACH WITH A NON-EMOTIONAL/SENTIMENT
# APPROACH AND DICTIONARY
