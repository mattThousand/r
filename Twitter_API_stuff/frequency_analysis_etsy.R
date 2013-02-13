# =========================================================================
# Title: Twitter_XML_Analysis_Etsy.R
# Author: Matt Buckley
# Description: Mining Twitter for tweets about Etsy, Artfire, and Ebay
# Liscense: MIT license: http://www.opensource.org/licenses/mit-license.php 
# =========================================================================


# STEPS:
# 1. Collect tweets in english containing the word 'etsy'
# 2. Store the results of the twitter queries as XML
# 3. Remove common english words
# 4. Create a term document matrix giving the frequencies of each word
# 5. Extract frequency metrics from tweets
# 6. Visualize results

# load packages
require(XML)
require(tm)
require(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(stringr)


# Let's use the XML package to parse some tweets containing the term "etsy"
# define twitter search url (following the atom standard)
twitter_url <- "http://search.twitter.com/search.atom?"

# vector to store results
etsy_tweets <- character(0)

# perform 20 queries so that a large number of tweets is collected
for (page in 1:20)
{
  # tweets in english containing 'etsy'
  twitter_search <- paste(twitter_url, "q=etsy",
                         "&rpp=100&lang=en&page", page, sep="")
  
  # parse the query using xmlParseDoc
  tweets<-xmlParseDoc(twitter_search, asText=FALSE)
  
  
  # extract the text content of each tweet
  title<-xpathSApply(tweets, "//s:entry/s:title", xmlValue, namespaces=c('s'='http://www.w3.org/2005/Atom'))
  
  # aggregate new tweets with previous ones
  etsy_tweets<-c(title, etsy_tweets)
}


# create a corpus
corpus <- Corpus(VectorSource(etsy_tweets))

# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("etsy","via", stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m <- as.matrix(tdm)

# get word counts in decreasing order
word_freqs <- sort(rowSums(m), decreasing=TRUE) 

# get a list of the tweets subdivided by the words that they contain
words_by_tweet <- strsplit(etsy_tweets,' ')

# words per tweet
words_per_tweet <- sapply(words_by_tweet , length)
# barplot
barplot(table(words_per_tweet), col='#9AE4E8', border=NA,
        main="Distribution of words per tweet", xlab='Frequency',
        ylab='Unique Words', cex.main=1)


# length of words per tweet
chars_per_tweet <- sapply(words_by_tweet , function(x) mean(nchar(x)))
# barplot
barplot(table(round(chars_per_tweet)), border=NA,
        xlab = "Character Length of Tweets", col="#9AE4E8", ylab='Number of Tweets', col='#9AE4E8',
        main="Distribution of Tweet Lengths (# of characters)", cex.main=1)

# How many unique words per tweet?
uniq_words_per_tweet <- sapply(words_by_tweet , function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet), col='#9AE4E8', border=NA,
        main="Distribution of # of Unique Words per Tweet", cex.main=1)

# How many hashtags per tweet?
hash_per_tweet <- sapply(words_by_tweet , function(x) length(grep("#", x)))
barplot(table(hash_per_tweet), col=c('#EEEEEE', '#9AE4E8'), xlab='Num Hashes', ylab='Num Tweets',
    main="Number of #\'s per Tweet")

# How many @mentions per tweet?
@_per_tweet <- sapply(words_by_tweet , function(x) length(grep("@", x)))
barplot(table(@_per_tweet), col=c('#EEEEEE', '#9AE4E8'), xlab='Num Mentions', ylab='Num Tweets',main='Number of @\'s per Tweet')

# how many http links per tweet?
links_per_tweet <- sapply(words_by_tweet , function(x) length(grep("http", x)))
pie(table(links_per_tweet), col=c('#EEEEEE', '#9AE4E8'), labels=c('zero','one'),
    main='Number of Links per Tweet')

# create a comprehensive dataframe with the information gathered thus far
worddf <- data.frame(
  word=names(word_freqs)
  freq=word_freqs
  chars=chars_per_tweet,
  words = words_per_tweet,
  lengths = chars_per_tweet,
  uniqs = uniq_words_per_tweet,
  hashes = hash_per_tweet,
  @ = @_per_tweet,
  links = links_per_tweet)


# Lexical diversity: number of unique words / total number of words
# all unique words 
uniq_words <- unique(unlist(words_by_tweet ))

# lexical diversity
length(uniq_words) / length(unlist(words_by_tweet ))


# top-10 most frequent words
most_freq_10 <- head(word_freqs, 10)

# barplot
barplot(most_freq_10, col='#9AE4E8', border=NA, las=2, main="10 Most Frequently Appearing Terms", cex.main=1)

# create a data frame with words and their frequencies from the term-document-matrix that excludes stopwords (like 'etsy')
etsy_df <- data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud 
wordcloud(etsy_df$word[1:500], etsy_df$freq[1:500], random.order=FALSE, max.words=400, colors=brewer.pal(8,"Dark2"), title='Frequently Occurring Words in Tweets about Etsy')

# plot hastag wordcloud
hashtags <- str_extract_all(etsy_tweets,  "#\\w+")
hashtags <- unlist(hashtags)
hashtag_freq <- table(hashtags)
wordcloud(names(hashtag_freq), hashtag_freq, random.order=FALSE, max.words=400, colors=brewer.pal(8,"Dark2"), title='Hashtags in Tweets about Etsy')


# Comparison Wordcloud
# Artfire (Etsy competitor)

# vector to store results
artfire_tweets <- character(0)

# perform 20 queries so that a large number of tweets is collected
for (page in 1:20)
{
  # tweets in english containing 'etsy'
  artfire_search <- paste(twitter_url, "q=artfire",
                          "&rpp=100&lang=en&page", page, sep="")
  
  # parse the query using xmlParseDoc
  tweets<-xmlParseDoc(artfire_search, asText=FALSE)
  
  
  # extract the text content of each tweet
  title<-xpathSApply(tweets, "//s:entry/s:title", xmlValue, namespaces=c('s'='http://www.w3.org/2005/Atom'))
  
  # aggregate new tweets with previous ones
  artfire_tweets<-c(title, artfire_tweets)
}

# create a corpus
corpus <- Corpus(VectorSource(artfire_tweets))

# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("artfire","via", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m <- as.matrix(tdm)

# get word counts in decreasing order
artfire_word_freqs <- sort(rowSums(m), decreasing=TRUE) 

artfire_df <- data.frame(af_word=names(artfire_word_freqs), af_freq=artfire_word_freqs)


# Ebay, the anti-Etsy...

# vector to store results
ebay_tweets<-character(0)

for (page in 1:20)
{
  # tweets in english containing 'ebay'
  twitter_search <- paste(twitter_url, "q=ebay",
                          "&rpp=100&lang=en&page", page, sep="")
  
  # parse the query using xmlParseDoc
  tweets<-xmlParseDoc(twitter_search, asText=FALSE)
  
  
  # extract the text content of each tweet
  title<-xpathSApply(tweets, "//s:entry/s:title", xmlValue, namespaces=c('s'='http://www.w3.org/2005/Atom'))
  
  # aggregate new tweets with previous ones
  ebay_tweets<-c(title, ebay_tweets)
}

# create a corpus
corpus <- Corpus(VectorSource(ebay_tweets))

# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("ebay","via", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m <- as.matrix(tdm)

# get word counts in decreasing order
ebay_word_freqs <- sort(rowSums(m), decreasing=TRUE) 

ebay_df <- data.frame(eb_word=names(ebay_word_freqs), eb_freq=(ebay_word_freqs))

  

clean = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

etsy_cleaned <- paste(clean(etsy_tweets), collapse=' ')
artfire_cleaned <- paste(clean(artfire_tweets), collapse=' ')
ebay_cleaned <- paste(clean(ebay_tweets), collapse=' ')

# smush all the tweets together
all_tweets<-c(etsy_cleaned,artfire_cleaned,ebay_cleaned)

# create a corpus of the combined tweets
corpus<-Corpus(VectorSource(all_tweets))

# ...then a term-document matrix
tdm_all<-TermDocumentMatrix(corpus,
                            control = list(removePunctuation = TRUE,
                                           stopwords = c("etsy","via","ebay,","artfire", stopwords("english")),
                                           removeNumbers = TRUE, tolower = TRUE))

tdm_all<-as.matrix(tdm_all)
colnames(tdm_all) = c("Etsy", "Artfire", "Ebay")

# plot comparison wordcloud
comparison.cloud(tdm_all, random.order=FALSE, 
                 colors=c("orange", "red","#FFCC11"),
                 title.size=1.5, max.words=500)