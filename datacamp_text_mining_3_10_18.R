### Moving to a more nuetral 'text mining' rather than seniment approach
### Datacamp "Text Mining: Bag of Words" course
# https://www.datacamp.com/courses/intro-to-text-mining-bag-of-words/
# reccomended by article at https://www.datacamp.com/community/blog/text-mining-in-r-and-python-tips

# also provides elements of analysis that move beyond initial analytics offered by social
# bearing.com  - so you can look at the language used when planning a dictionary to build

# but doesn't entangle too much into complexities of semantic analysis

  ## RUNNING TUTORIAL ON SAMPLE TWITTER DATA

rm(list = ls())

getwd()
setwd("/Users/callumrichards/Documents/Careers/ThamesWater_September2018/TW_EV_Social/Data")

# RELEVANT PACKAGES
library(qdap)
library(tm)
library(ggplot2)


# packages from previous twitter analysis
library(twitteR)
library(syuzhet)
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

# 'Bag of words' method will treat text as while block eg if its data from a number of tweets
# will combine each tweet into a long string of text 

# Processing our twitter data
tweet_df<-read.csv('tweet-report_socialbear_20_9_18_10days.csv', stringsAsFactors = FALSE,skip=49, sep=",")[ ,1:28]
# 'stringsAsFactors ensures that string or characters are not automatically converted to a factor variable
attach(tweet_df)


# Convert 'Created.At' column to timestamp of each tweet in a readable format
colnames(tweet_df)[colnames(tweet_df)=="Created At"] <- "Created.At"
format.str <- "%a %b %d %H:%M:%S %z %Y"
tweet_df$timestamp<-as.POSIXct(strptime(tweet_df[,"Created.At"], format.str, tz = "GMT"), tz = "GMT")
attach(tweet_df)
length(timestamp)

###########################

# 'tweet specific' cleaning steps we have taken in earlier scripts
# remember re-checking the tweets after every step of cleaning ie 
# head(Tweet) vs head(tweet_df$clean_tweets)

# using gsub to get raw sentances and move into new variable
# of 'clean_tweets' (steps taken from https://wetlands.io/maps/Twitter_Analytics_R.nb.html)

# Replace blank space (“rt”) # removes rt, but does not completely remove retweeted tweets
tweet_df$clean_tweets <- gsub("rt", "", Tweet)
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets )
# Replace @UserName
tweet_df$clean_tweets  <- gsub("@\\w+", "", tweet_df$clean_tweets )
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# removes @thames water (obviously they will all have this present as this sample is from twets to @thameswater)
# Remove punctuation
tweet_df$clean_tweets <- gsub("[[:punct:]]", "", tweet_df$clean_tweets)
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# Remove links
tweet_df$clean_tweets <- gsub("http\\w+", "", tweet_df$clean_tweets)
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# Remove blank spaces at the beginning
tweet_df$clean_tweets <- gsub("^ ", "", tweet_df$clean_tweets)
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# Remove blank spaces at the end
tweet_df$clean_tweets <- gsub(" $", "", tweet_df$clean_tweets)
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# remove emojis or special characters 
## !!!! this also removed hashtags - so if you want to keep hashtags in then find a different function
tweet_df$clean_tweets <- gsub('<.*>', '', enc2native(tweet_df$clean_tweets))
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# also remove instances where users have embedded pictures and left pic address
tweet_df$clean_tweets <- gsub("pictwitter.*", '', tweet_df$clean_tweets)
# initial tweets
head(Tweet)
# cleaned tweets
head(tweet_df$clean_tweets)
# #convert all text to lower case
tweet_df$clean_tweets <- tolower(tweet_df$clean_tweets)
attach(tweet_df)
## again consider if this 'date cleansing' step is also getting rid of potentially
# useful information that may not apply to sentiment analysis
# but may still be useful for our particular application
# eg can try and run with the 'replace_emoticon' function to conver
# emoticons to word expressed emotions
head(clean_tweets)

  ### Form Corpuses for text mining, from the now pre-prepared 'clean_tweets'

# qdap package offers word count functionality eg find the top 10 most frequent terms
# (including ties) in text by calling the freq_terms function and specifying 10.

frequent_terms <- freq_terms(clean_tweets, 10) # object stores all unique words and their count
plot(frequent_terms)
# (may be worth subsetting sample into first 50 tweets (chronologically)
# no surprise thameswater is up there, as well as common stop words eg 'the', 'is' etc

# Build a corpus - highlighting functional nouns and taking into account word type
# Corpus = collection of documents eg tweets

# specify the tweet text as your text source, so the tm package can recognise it
# in the tm domain, r recognises corpuses as a data type

# note two types of corpus
# PCorpus = collection of docs in the corpus saved on disk
# VCorpus = held in computer's RAM

# Making a VCorpus - R needs to interpret each element in our vector of text, clean_tweets, 
# as a document. The tm package providesSource functions to do so, forming a Source object
# eg VectorSource() because our text data is contained in a vector

# Make a vector source: coffee_source
tweets_source<-VectorSource(clean_tweets)
# pass the source VCorpus() to form a 'volatile corpus'
tweets_corpus<-VCorpus(tweets_source)
# produces a nested list,  at each index of the VCorpus object,
# there is a PlainTextDocument object, which is a list containing actual text data 
# (content), and some corresponding metadata (meta), that can help to visualize a 
# VCorpus object to conceptualize the whole thing
print(tweets_corpus)

# To review a single document object you subset with double square brackets. eg 5th object
tweets_corpus[[5]]
# To review the actual text you index the list twice. 
tweets_corpus[[5]][1]
tweets_corpus[[5]][2]
# To access the document's metadata, like timestamp, change [1] to [2].
# Another way to review the plain text is with the content() function which doesn't 
# need the second set of brackets.
content(tweets_corpus[[5]])

# Can also make a VCorpus from a data frame- when your text data is in a data frame 
# Using DataframeSource() for the analysis. The data frame passed to DataframeSource()
# though must have a specific structure:
## Column one must be called doc_id and contain a unique string for each row.
## Column two must be called text with "UTF-8" encoding (pretty standard).
## Any other columns, 3+ are considered metadata and will be retained as such.

# DOWNSIZE the.csv df downloaded from social bearing bearing again that we made in previous analysis

# remove uneccessary columns that are a bit overkill for R - using subsetting
tweet_df_cleaned <- subset(tweet_df, select = -c(Screen.Name,Created.At,Links, Language,
                                             Source, Domain, URL, Tweet.Link, Pic.Link,
                                             Retweeting.User, Retweeting.Handle, Tweet.ID) )
# move 'timestamp' column to 2nd column in subset
tweet_df_cleaned<-tweet_df_cleaned[,c(1,17,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18)]


# adjust file to the following conditions:
## Column one must be called doc_id and contain a unique string for each row.
## Column two must be called text with "UTF-8" encoding (pretty standard).

# add a first column with a unique identifer
tweet_df_cleaned <- tweet_df_cleaned %>% mutate(doc_id = row_number())
# move 'doc_id' column to 1st column in subset
tweet_df_cleaned<-tweet_df_cleaned[,c(19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]

# rename 'clean_tweets' as 'text' column and move to second column
colnames(tweet_df_cleaned)[colnames(tweet_df_cleaned)=="clean_tweets"] <- "text"
tweet_df_cleaned<-tweet_df_cleaned[,c(1,19,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
# df now ready for conversion to corpus

# Create a DataframeSource: tweet_df_source
tweet_df_source<-DataframeSource(tweet_df_cleaned)
# Convert df_source to a volatile corpus: df_corpus
tweet_df_corpus<-VCorpus(tweet_df_source)
# Examine df_corpus
tweet_df_corpus
# contains 1488 'documents' ie each individual tweet in our dataframe
# and 17 document level metadata points
# Examine df_corpus metadata
meta(tweet_df_corpus)
# all the extra data that comes along with the tweets, besides ID and the cleaned tweet text

  ### CLEANING AND PREPROCESSING TEXT - ! bear in mind that tweets have already gone through
# one tweet specific cleaning process, so be wary of what extra cleaning steps you 
# are carrying out to the text here 

# tm function descriptions on help() or on txt.mining DC slides - always be sure you know what
# each command is doing, and is creating text that is suitable for downsteam analysis
# tolower(): Make all characters lowercase
# removePunctuation(): Remove all punctuation marks
# removeNumbers(): Remove numbers
# stripWhitespace(): Remove excess whitespace

# cleaning functions also contained within qdap()
# bracketX(): Remove all text within brackets (e.g. "It's (so) cool" becomes "It's cool")
# replace_number(): Replace numbers with their word equivalents (e.g. "2" becomes "two")
# replace_abbreviation(): Replace abbreviations with their full text equivalents (e.g. "Sr" becomes "Senior")
# replace_contraction(): Convert contractions back to their base words (e.g. "shouldn't" becomes "should not")
# replace_symbol() Replace common symbols with their word equivalents (e.g. "$" becomes "dollar")
# overall, used correctly, these cleaning operations can make text more conducive to a 'standard'
# set of search terms, that then do not need to be adjusted to all possible semantic variation
# * for compatability base R and qdap functions need to be wrapped in content_transformer() to function however

# can also filter out stop words, words that are frequent but provide little information (the, of etc)
# tm package contains 174 common English stop words
stopwords("en")

# and you can add to this list during the analysis, for words that don't add any insight and 
# will cause it to be overemphasized in a frequency analysis. 
# eg add words 1 and 2 to the default list of English stop words
# all_stops <- c("word1", "word2", stopwords("en"))

# then use removeWords() function on your text to remove stop words

# further hone analysis and define adaptable set of search terms using "Word Stemming"
# Reduceing words to unify across documents. 
# For example, the stem of "computational", "computers" and "computation" is "comput". 
# But because "comput" isn't a real word, then re-constructing the words in 'stem completion' so that 
#"computational", "computers", and "computation" all refer a recognizable word, such as "computer". 

#  stemDocument() in tm package can get to a word's root autmatically as well, eg
stemDocument(c("computational", "computers", "computation"))
# and can the use stemCompletion() reconstruct these word roots back into a known term, 
# taking two terms a character vector and a completion dictionary ( character vector or a Corpus object)
# process
# Create complicate
complicate<-c("complicated", "complication", "complicatedly")
# Perform word stemming: stem_doc
stem_doc<-stemDocument(complicate)
# Create the completion dictionary: comp_dict
comp_dict<-c("complicate")
# Perform stem completion: complete_text 
complete_text<-stemCompletion(stem_doc, comp_dict)
# Print complete_text
complete_text

# if you want to carry out stemming on a complete sentance (or even corpus) however
# you first remove the punctation marks with removePunctuation() and then strsplit() this 
# character vector of length 1 to length n, unlist(), then proceed to stem and re-complete.
#  because stemDocument() treats the whole sentence as one word,
# so the document is a character vector of length 1, instead of length n (number of words in the document)

## now apply the appropriate preprocessing steps to your corpus of tweets
# using tm_map() to apply cleaning functions to the entire corpus, making the cleaning steps easier. 

# easiest to utlise a custom cleaning function, which you can alter to fit your purpose,
# applying the same functions over multiple corpora as it takes one argument, corpus, 
# and applies a series of cleaning functions to it in order, then returns the updated corpus.
# ** bear in mind, the order of cleaning steps makes a difference. 
# For example, if you removeNumbers() and then replace_number(), the second function won't find anything to change **
# ALWAYS CHECK YOUR RESULTS!!

# apply to corpus
tweets_corpus
tweets_corpus[[5]][1]

# Alter the function code to match your requirements for sample twitter data
clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords - IMPORTANT that these are being removed
  stopwords("en")
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug"))
  # Strip whitespace
  corpus<-tm_map(corpus,stripWhitespace)
  # remove all text within brackets
  corpus<-tm_map(corpus, content_transformer(bracketX))
  # convert contractions back to their base words eg shouldn't = should not
  corpus<-tm_map(corpus, content_transformer(replace_contraction))
  return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_tweets_corp<-clean_corpus(tweets_corpus)

# Print out a cleaned up tweet
clean_tweets_corp[[5]][1]

  ### MANIPULATE THE DATA STRUCTURE
# Now have a 'clean' corpus, now need to change the data structure ready for analysis
# into a TDM (words/terms as rows, 'documents' as columns) 
# or DTM (vice verca) -  useful if data is arranged chronologically and you want to preserve the time series. 
# and in their simplest form, can indicate word frequencies within each document
# while you can also construct a 'word frequency matrix' (WFM) within the qdap package

# might be easier to work from a subset of the first 50  tweets here, to limit the amount of data
# we are handling

# Create the DTM from the corpus: coffee_dtm
tweets_dtm<-DocumentTermMatrix(clean_tweets_corp)
# Convert dtm to a matrix
tweets_m<-as.matrix(tweets_dtm)
# Print the dimensions 
dim(tweets_m)
# 1488 rows (ie documents) and 3872 columns (terms)

# Review a portion of the matrix to get some Starbucks
print(tweets_m[1:50, 100-20])

# create the TDM - taking the trasnpose of the DTM, and is often used for language analysis
# as you likely have more terms than documents  - and forms the 'bag of words' for further analysis
# Create a TDM from clean_corp: coffee_tdm
tweets_tdm<-TermDocumentMatrix(clean_tweets_corp)
# Print coffee_tdm data
tweets_tdm
# Convert coffee_tdm to a matrix: coffee_m
tweets_m2<-as.matrix(tweets_tdm)
# print dimensions
dim(tweets_m2) 
# dimensions now switched around
# again can review a portion by same command above


###############################################################################################
###############################################################################################

 ### CHAPTER 2: COMMON TEXT MINING VISUALS

### Can clear and work from a subset of the first 100 (most recent) tweets from our corpus
# if you want a subsetted view of the data
rm(list = ls())
getwd()
setwd("/Users/callumrichards/Documents/Careers/ThamesWater_September2018/TW_EV_Social/Data")
tweet_df<-read.csv('tweet-report_socialbear_20_9_18_10days.csv', stringsAsFactors = FALSE,skip=49, sep=",")[ ,1:28]
# 'stringsAsFactors ensures that string or characters are not automatically converted to a factor variable
attach(tweet_df)
tweets_100<-slice(tweet_df, 1:100 )
# Convert 'Created.At' column to timestamp of each tweet in a readable format
colnames(tweets_100)[colnames(tweets_100)=="Created At"] <- "Created.At"
format.str <- "%a %b %d %H:%M:%S %z %Y"
tweets_100$timestamp<-as.POSIXct(strptime(tweets_100[,"Created.At"], format.str, tz = "GMT"), tz = "GMT")
attach(tweets_100)
length(timestamp)
# 'tweet specific' cleaning steps
# Replace blank space (“rt”) # removes rt, but does not completely remove retweeted tweets
tweets_100$clean_tweets <- gsub("rt", "", Tweet)
# Replace @UserName
tweets_100$clean_tweets  <- gsub("@\\w+", "", tweets_100$clean_tweets )
# Remove punctuation
tweets_100$clean_tweets <- gsub("[[:punct:]]", "", tweets_100$clean_tweets)
# Remove links
tweets_100$clean_tweets <- gsub("http\\w+", "", tweets_100$clean_tweets)
# Remove blank spaces at the beginning
tweets_100$clean_tweets <- gsub("^ ", "", tweets_100$clean_tweets)
# Remove blank spaces at the end
tweets_100$clean_tweets <- gsub(" $", "", tweets_100$clean_tweets)
# remove emojis or special characters 
## !!!! this also removed hashtags - so if you want to keep hashtags in then find a different function
tweets_100$clean_tweets <- gsub('<.*>', '', enc2native(tweets_100$clean_tweets))
# also remove instances where users have embedded pictures and left pic address
tweets_100$clean_tweets <- gsub("pictwitter.*", '', tweets_100$clean_tweets)
# #convert all text to lower case
tweets_100$clean_tweets <- tolower(tweets_100$clean_tweets)
attach(tweets_100)
frequent_terms <- freq_terms(tweets_100$clean_tweets, 10) # object stores all unique words and their count
plot(frequent_terms)
# Make a vector source: coffee_source
tweets_source<-VectorSource(clean_tweets)
# pass the source VCorpus() to form a 'volatile corpus'
tweets_corpus<-VCorpus(tweets_source)
print(tweets_corpus)
tweets_corpus[[5]]
# review the actual text you index the list twice. 
tweets_corpus[[5]][1]
# To access the document's metadata, like timestamp, change [1] to [2].
tweets_corpus[[5]][2]
# Construct VCorpus from a data frame- 
# DOWNSIZE the.csv df downloaded from social bearing bearing again that we made in previous analysis
# remove uneccessary columns that are a bit overkill for R - using subsetting
tweet_df_cleaned <- subset(tweets_100, select = -c(Screen.Name,Created.At,Links, Language,
                                                 Source, Domain, URL, Tweet.Link, Pic.Link,
                                                 Retweeting.User, Retweeting.Handle, Tweet.ID) )
# move 'timestamp' column to 2nd column in subset
tweet_df_cleaned<-tweet_df_cleaned[,c(1,17,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18)]
# adjust file to the following conditions:
## Column one must be called doc_id and contain a unique string for each row.
## Column two must be called text with "UTF-8" encoding (pretty standard).
tweet_df_cleaned <- tweet_df_cleaned %>% mutate(doc_id = row_number())
# move 'doc_id' column to 1st column in subset
tweet_df_cleaned<-tweet_df_cleaned[,c(19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
# rename 'clean_tweets' as 'text' column and move to second column
colnames(tweet_df_cleaned)[colnames(tweet_df_cleaned)=="clean_tweets"] <- "text"
tweet_df_cleaned<-tweet_df_cleaned[,c(1,19,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
# df now ready for conversion to corpus
# Create a DataframeSource: tweet_df_source
tweet_df_source<-DataframeSource(tweet_df_cleaned)
# Convert df_source to a volatile corpus: df_corpus
tweet_df_corpus<-VCorpus(tweet_df_source)
# Examine df_corpus
tweet_df_corpus
# contains 100 'documents' ie each individual tweet in our dataframe - rather 1488 of full dataframe
# and 17 document level metadata points
# Examine df_corpus metadata
meta(tweet_df_corpus)
# all the extra data that comes along with the tweets, besides ID and the cleaned tweet text
### CLEANING AND PREPROCESSING TEXT  - tm functionality 
# Alter the function code to match your requirements for sample twitter data
clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords - IMPORTANT that these are being removed
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug"))
  # Strip whitespace
  corpus<-tm_map(corpus,stripWhitespace)
  # remove all text within brackets
  corpus<-tm_map(corpus, content_transformer(bracketX))
  # convert contractions back to their base words eg shouldn't = should not
  corpus<-tm_map(corpus, content_transformer(replace_contraction))
  return(corpus)
}
# Apply your customized function to the tweet_corp: clean_corp
clean_tweets_corp<-clean_corpus(tweets_corpus)
# Print out a cleaned up tweet
clean_tweets_corp[[5]][1]
### MANIPULATE THE DATA STRUCTURE
# Create the DTM from the corpus: coffee_dtm
tweets_dtm<-DocumentTermMatrix(clean_tweets_corp)
# Convert dtm to a matrix
tweets_m<-as.matrix(tweets_dtm)
# Print the dimensions 
dim(tweets_m)
# 100 rows (ie documents) and now only 653 columns (terms)
# Review a portion of the matrix to get some Starbucks
print(tweets_m[1:50, 100-20])
# create the TDM - taking the transpose of the DTM, and is often used for language analysis
# as you likely have more terms than documents  - and forms the 'bag of words' for further analysis
# Create a TDM from clean_corp: coffee_tdm
tweets_tdm<-TermDocumentMatrix(clean_tweets_corp)
# Print tweets_tdm data
tweets_tdm
# Convert coffee_tdm to a matrix: coffee_m
tweets_m2<-as.matrix(tweets_tdm)
# print dimensions
dim(tweets_m2) 
# dimensions now switched around
# again can review a portion by same command above

  ##### DATA VISUALISATION
# working from full dataset - rather than sub-set of 100 for the moment 

  ### Frequent terms plotting

# make sure tweets_tdm is a matrix
tweets_m2<-as.matrix(tweets_tdm)
# Calculate the rowSums: term_frequency - ggregates all the terms used in a passage.
term_frequency<-rowSums(tweets_m2)
# Sort term_frequency in descending order
term_frequency<-sort(term_frequency, decreasing=TRUE)
# View the top 10 most common words
term_frequency[1:10]
# Plot a barchart of the 10 most common words - using ggplot2 barplot
barplot(term_frequency[1:10], col="tan", las=2)
# already seeing potential search terms appearing eg water, road, fatberg, leak 
# anyway to introduce phrases akin to the search terms present in Lithium

# and faster way to get frequent terms is via qdap
# qdap has its own list of stop words that differ from those in tm
# function accepts a text variable, can specify the top number of terms to show with the top argument, 
# a vector of stop words to remove with the stopwords argument, and the minimum character length of a word to be included with the at.least argument. 
