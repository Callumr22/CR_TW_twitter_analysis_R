# Rinker (2018) sentimentr package - GitHub tutorial continuation
citation(package = "sentimentr")
# https://github.com/trinker/sentimentr

# Worked through example of Annie Swafford's critique of Jocker (2015) 'Syuzhet' package
# for sentiment analysis
# https://annieswafford.wordpress.com/2015/03/02/syuzhet/
# useful reply from Jocker - http://www.matthewjockers.net/2015/03/04/some-thoughts-on-annies-thoughts-about-syuzhet/

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

# testing each of Jocker's 4 dictionary approaches (syuzhet, Bing, NRC, Afinn), 
# his Stanford wrapper (note use Rinker's own GitHub Stanford wrapper package based off of Jocker's approach) 
# the RSentiment package, the lookup based SentimentAnalysis package, 
# the meanr package (written in C level code), and my own algorithm with default
# combined Jockers (2017) & Rinker's augmented Hu & Liu (2004) polarity lexicons 
# as well as Hu & Liu (2004) and Baccianella, Esuli and Sebastiani's (2010) 
# SentiWord lexicons available from the lexicon package.

# load packages using for testing 
pacman::p_load_gh("trinker/sentimentr", "trinker/stansent", "sfeuerriegel/SentimentAnalysis", "wrathematics/meanr")
pacman::p_load(syuzhet, qdap, microbenchmark, RSentiment)

### have not ran code for the moment in order to save memory and time
## full code is within GitHub repository however at the link above

# does show how sentiment scores can very a great deal depending on the method
# used, with each having their own strong points and downfalls and the accuracy 
# of an algorithm weighs heavily into the decision as to what approach to take 
# in sentiment detection.

