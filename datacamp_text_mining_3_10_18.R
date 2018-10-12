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
library(wordcloud)
library(viridisLite)
library(lubridate)
library(dplyr)
library(plotrix) # required to build pyramid plots

# packages from previous twitter analysis
library(twitteR)
library(syuzhet)
library(SnowballC)
library(readr)
library(stringi)
library(textclean)
library(plotly)
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
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
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
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
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
# a vector of stop words to remove with the stopwords argument and the minimum character length 
# of a word to be included with the at.least argument. 
# Create frequency
frequency <- freq_terms(
  tweet_df_cleaned$text, 
  top = 10, # limit to top 10 terms
  at.least = 3, # at least 3 letters per term
  stopwords = "Top200Words" # dictionary of defined stop words
)
stopwords(Top200Words)
# Make a frequency barchart - calling plot() on freq_terms() object
plot(frequency)

# now create a frequency plot with a different stopwords - from english dictionary
frequency2 <- freq_terms(
  tweet_df_cleaned$text, 
  top = 10, # limit to top 10 terms
  at.least = 3, # at least 3 letters per term
  stopwords("english") # dictionary of defined stop words
)
stopwords("english")
# Make a another frequency barchart
plot(frequency2, col="blue")
# better/more extensive suite of stopwords

# Word clouds
# 'Visually engaging' - but personally do not like them as they are not very quantative 
# and can easily give misleading messages behind the data

# simply wordcloud - size of words ~ frequency
# importance of prepreocessing text and selecting appropriate stopwords is still important
# eg adjust clean_corpus function so you can identify proper nouns etc
# eg or remove very very common words that do no reveal much insight eg water,
# that can mask underlying insight from other word patterns (like an outlier)
#so can adjust function to remove the words you 'expect'

# Print the first 10 entries in term_frequency
term_frequency[1:10]
#Extract the terms using names() on term_frequency. Call the vector of strings terms_vec.
terms_vec<-names(term_frequency)
# construct wordcloud - using terms_vec as the words, and term_frequency as the values. 
# Add the parameters max.words = 50 and colors = "red", and can adjust, to custommize the cloud
wordcloud(terms_vec, term_frequency, max.words=50, colors="red")

# word cloud clean up
# Review a "cleaned" tweet - using content() to show you a specific tweet for comparison. 
content(clean_tweets_corp[[11]])
# indicates that '&' has been coded as 'amp' - and it is coming up in the word cloud
# so this, along with water, should maybe be included in our list of stop words
# Add to stopwords
stops <- c(stopwords(kind = 'en'), 'water', 'amp')
# Review last 6 stopwords 
tail(stops) # 'water' and 'amp' no included
# Apply to a corpus to form a cleaned_tweets_corp_2 with tm_map() by passing in the cleaned_tweets_corp, 
# the function removeWords and finally the stopwords, stops.
clean_tweets_corp_2 <- tm_map(clean_tweets_corp, removeWords, stops)
# Review a "cleaned" tweet again
content(clean_tweets_corp_2[[11]])
# remove unnecc. whitespaces again procued from removal of words
clean_tweets_corp_2<-tm_map(clean_tweets_corp_2,stripWhitespace)
content(clean_tweets_corp_2[[11]])

# now have updated 'cleaned' corpus - so build improved word cloud
# convert to tdm
tweets_tdm_2<-TermDocumentMatrix(clean_tweets_corp_2)
# Print tweets_tdm data
tweets_tdm_2
# Convert coffee_tdm to a matrix: coffee_m
tweets_m3<-as.matrix(tweets_tdm_2)
# produce a name vector 
term_frequency_2<-rowSums(tweets_m3)
# Sort into descending order
sorted_tweet_words<-sort(term_frequency_2, decreasing=TRUE) 
# Print the 10 most frequent terms
sorted_tweet_words[1:10]
# Get a terms vector
terms_vec_2<-names(sorted_tweet_words)
# Create a wordcloud for the values in word_freqs
wordcloud(terms_vec_2, sorted_tweet_words, # can also use term_frequency_2 to get unordered word list 
             max.words = 50, colors = "red", fixed.asp=TRUE, rot.per=0)

help(wordcloud)

# improving the appearance of word clouds
# can further specifiy colours, using vector of named colours
# reference: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=2ahUKEwi5p-iO0oDeAhWoLcAKHQfCBMoQFjABegQIBxAC&url=http%3A%2F%2Fwww.stat.columbia.edu%2F~tzheng%2Ffiles%2FRcolor.pdf&usg=AOvVaw1XIn-mwZ73RgG8bxh4Lan4
# eg use "grey80", "darkgoldenrod1", and "tomato" - best practice to start with three colors,
# each with increasing vibrancy, helping to naturally divide the term frequency into "low", 
# "medium" and "high" for easier viewing.
wordcloud(terms_vec_2, sorted_tweet_words,
          max.words=100, 
          colors= c("grey80","darkgoldenrod1", "tomato"))
# or can use viridisLite package to pick perceptually-uniform colours pallete for you 
# There are multiple color palettes each with a convenience function - specify n to select the number of colors needed.
# and each function returns a vector of hexadecimal colors based on n.eg
# Select 5 colors 
color_pal<-cividis(n=5)
# Examine the palette output
color_pal

wordcloud(terms_vec_2, sorted_tweet_words,
          max.words=100, 
          colors= color_pal)

  ### COMPARISON WORDCLOUDS AND PYRAMID PLOTS

# COMMONALITY WORDCLOUDS - shared words, ' conjunction' in the venn diagram

# load in data covering period between 28/9/18 - 12/10/18
# containing tweets from focal period of 2/10/18 - 3/10/18 between which main burst occured
# at Lea bridge road #E3

# CLEAR the environment for this new stagecof the analysis
rm(list = ls())

getwd()
setwd("/Users/callumrichards/Documents/Careers/ThamesWater_September2018/TW_EV_Social/Data")

tweet_df_oct<-read.csv('tweets_1_10_18-12_10_18.csv', stringsAsFactors = FALSE,skip=50, sep=",")[ ,1:28]
# 'stringsAsFactors ensures that string or characters are not automatically converted to a factor variable
attach(tweet_df_oct)

# Convert 'Created.At' column to timestamp of each tweet in a readable format
colnames(tweet_df_oct)[colnames(tweet_df_oct)=="Created At"] <- "Created.At"
format.str <- "%a %b %d %H:%M:%S %z %Y"
tweet_df_oct$timestamp<-as.POSIXct(strptime(tweet_df_oct[,"Created.At"], format.str, tz = "GMT"), tz = "GMT")
attach(tweet_df_oct)
length(timestamp)

# remove uneccessary columns that are a bit overkill for R - using subsetting
cleaned_tweets_oct<- subset(tweet_df_oct, select = -c(Screen.Name,Created.At,Links, Language,
                                                 Source, Domain, URL, Tweet.Link, Pic.Link,
                                                 Retweeting.User, Retweeting.Handle, Tweet.ID) )
# move 'timestamp' column to 1st column in subset
cleaned_tweets_oct<-cleaned_tweets_oct[,c(17,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

# from clean_tweets with tweet specific cleaning operations
# Replace blank space (“rt”) # removes rt, but does not completely remove retweeted tweets
cleaned_tweets_oct$clean_tweets <- gsub("rt", "", Tweet)
# Replace @UserName
cleaned_tweets_oct$clean_tweets  <- gsub("@\\w+", "", cleaned_tweets_oct$clean_tweets )
# Remove punctuation
cleaned_tweets_oct$clean_tweets <- gsub("[[:punct:]]", "", cleaned_tweets_oct$clean_tweets)
# Remove links
cleaned_tweets_oct$clean_tweets <- gsub("http\\w+", "", cleaned_tweets_oct$clean_tweets)
# Remove blank spaces at the beginning
cleaned_tweets_oct$clean_tweets <- gsub("^ ", "", cleaned_tweets_oct$clean_tweets)
# Remove blank spaces at the end
cleaned_tweets_oct$clean_tweets <- gsub(" $", "", cleaned_tweets_oct$clean_tweets)
# remove emojis or special characters 
## !!!! this also removed hashtags - so if you want to keep hashtags in then find a different function
cleaned_tweets_oct$clean_tweets <- gsub('<.*>', '', enc2native(cleaned_tweets_oct$clean_tweets))
# also remove instances where users have embedded pictures and left pic address
cleaned_tweets_oct$clean_tweets <- gsub("pictwitter.*", '', cleaned_tweets_oct$clean_tweets)
# #convert all text to lower case
cleaned_tweets_oct$clean_tweets <- tolower(cleaned_tweets_oct$clean_tweets)
attach(cleaned_tweets_oct)
# move 'clean_tweets' to 3rd column 
cleaned_tweets_oct<-cleaned_tweets_oct[,c(1,2,18,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
# df now ready for conversion to corpus


# subset data to only produce tweets across 2 days, into seperate dataframes from which
# you can form comparison corpura
str(cleaned_tweets_oct)
# tweets from 2018-10-02
tweets_02 <- subset(cleaned_tweets_oct, timestamp >= as.POSIXct('2018-10-02 00:00:00', tz="UTC") &
                  timestamp <= as.POSIXct('2018-10-02 23:59:59', tz="UTC"))
# tweets from 2018-10-03
tweets_03 <- subset(cleaned_tweets_oct, timestamp >= as.POSIXct('2018-10-03 00:00:00', tz="UTC") &
                  timestamp <= as.POSIXct('2018-10-03 23:59:59', tz="UTC"))
### at first glance we can already see that there were far more tweets to @thameswater on 
# 3/10 (506 obs) vs 2/10 (140 obs) - already an indication that something interesting is happening


# Combine our 'corpora' and the text columns
all_02<-paste(tweets_02$clean_tweets, collapse = "")
all_03<-paste(tweets_03$clean_tweets, collapse = "")

# Concatenate two together
all_tweets <- c(all_02, all_03)
# Convert to corpus
all_tweets <- VectorSource(all_tweets)
all_corpus <- VCorpus(all_tweets)
# clean according to your clean_corupus function

clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords - IMPORTANT that these are being removed
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), 'water','amp'))
  # Strip whitespace
  corpus<-tm_map(corpus,stripWhitespace)
  # remove all text within brackets
  corpus<-tm_map(corpus, content_transformer(bracketX))
  # convert contractions back to their base words eg shouldn't = should not
  corpus<-tm_map(corpus, content_transformer(replace_contraction))
  return(corpus) 
}

  
all_clean <- clean_corpus(all_corpus)

# since you collapsed the document to two columns in the tdm, simply change it to a matrix
# and pass it to the commonality cloud function, WITHOUT using rowsums()
all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)

# Make commonality cloud - subsetting the terms to only words that are shared between corpura
commonality.cloud(all_m, colors = "steelblue1", max.words = 100)
# 

# COMPARISON CLOUD - IE where text between two days differs - 'disjunction' in the venn diagram

all_02_2<-paste(tweets_02$clean_tweets, collapse = "")
all_03_2<-paste(tweets_03$clean_tweets, collapse = "")

# Concatenate two together
all_tweets_2 <- c(all_02_2, all_03_2)
# Convert to corpus
all_tweets_2 <- VectorSource(all_tweets_2)
all_corpus_2 <- VCorpus(all_tweets_2)
all_clean_2 <- clean_corpus(all_corpus_2)
all_tdm_2 <- TermDocumentMatrix(all_clean_2)
# but once organised into a tdm you should explicitly define the column names
# in colnames and pass in a vector of names
colnames(all_tdm_2) <- c("2nd Oct", "3rd Oct")
# convert to matrix and pass to comparison cloud with some aesthetics, identifying the words
# that are dissimilar, 
all_m_2 <- as.matrix(all_tdm_2)

# Make comparison cloud
comparison.cloud(all_m_2,colors = c("orange", "blue"), max.words = 50, 
                 title.size = 2, match.colors = FALSE, title.bg.color='white')
# clear pattern of 'flood', 'burst', 'pipe' etc appearing far more post event on 03_10 vs 02_10
# but this is not ordered by word frequency -  pyramid help with this?
help(comparison.cloud)

  ###   PYRAMID PLOT
# can also use a polarized tag plot to analyze the conjunction between two corpura, using 
# a pyramid plot

# instead of passing the matrix to wordcloud, you can subset it to identify the terms that both
# documents share
# Identify terms shared by both documents
common_words <- subset(all_m,
  all_m[, 1] > 0 & all_m[, 2] > 0
)
# Find most commonly shared words - calculate absolute difference between the common words
# order by the difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                     decreasing = TRUE), ]
# make a small top terms df 
top25_df <- data.frame(x = common_words[1:25, 1],
                         y = common_words[1:25, 2],
                         labels = rownames(common_words[1:25, ]))
# pass top 25 terms into the plot.df function along with some aesthetics
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 12, unit = NULL, lxcol="tan", rxcol="steelblue1",
             top.labels = c("2nd Oct",
                            "Words",
                            "3rd Oct"))
help("pyramid.plot")

# Create word network with word.associate (function does most things for you - basic pre-processing
# and adjusting the ajaceny network of connections)

