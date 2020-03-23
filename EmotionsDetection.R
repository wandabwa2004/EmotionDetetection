
---
Title: " New Zealanders Response to Corona Virus"
Author: "Herman Wandabwa "
Description: "Data with no polarity scores. Result is the polarity score representative of New Zealanders sentiments after 28th Feb 2020 when the 1st Corona  case was announced in NZ"
---
  
  #load required packages
library(dplyr)
library(stringr)
library(plyr)
library(tm)
library(SnowballC)
library(tm)
library(syuzhet)#Emotions related to 8 expressions here https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

require(tidyverse)
require(tidytext)

require(gplots)
theme_set(theme_bw(12))

##close/clear global environment
closeAllConnections()

rm(list=ls())

##garbage collection - manual trigger
gc()

setwd("Path to the folder with Tweets.csv file")

##Set up filename 
datafile <- "Tweets_copy.csv" #Removed usernames for privacy purposes

# read reviews/comments into R
tweets.df =read.csv(datafile, header = TRUE, stringsAsFactors = TRUE, sep=',')
glimpse(tweets.df) #same as str() but in a fancier way

#Data clean up. Stopwords wont be removed as they are significant in negation 

mydata.df <- gsub("http.*","",tweets.df$text)
mydata.df <- gsub("https.*","",mydata.df)
mydata.df <- gsub("[[:punct:]]", "", mydata.df)
mydata.df <- gsub("[[:cntrl:]]", "", mydata.df)
mydata.df <- gsub("\\d+", "", mydata.df)

head(mydata.df)

#mydataCopy is a term document,generated from cleaning sentiments.csv
mydataCopy.df <- mydata.df

#carryout sentiment mining using the get_nrc_sentiment()function
tweets_sentiments <- get_nrc_sentiment(as.character(mydataCopy.df))

#change sentiments from a list to a data frame and transpose it 
tweets_sentiments1<-data.frame(t(tweets_sentiments))
#rowSums computes column sums across rows for each level of a #grouping variable.
tweets_sentiments_new <- data.frame(rowSums(tweets_sentiments1))
#name rows and columns of the dataframe
names(tweets_sentiments_new)[1] <- "count"
tweets_sentiments_new <- cbind("sentiment" = rownames(tweets_sentiments_new), tweets_sentiments_new)
rownames(tweets_sentiments_new) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=tweets_sentiments_new[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("New Zealanders Emotions Distribution after 28th Feb 2020 (1st COVID-19 Case Announcement)")
#plot the last 2 rows ,positive and negative
qplot(sentiment, data=tweets_sentiments_new[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("New Zealanders Overall Sentiments after 28th Feb 2020 (1st COVID-19 Case Announcement)")

#=========================


word.df <- as.vector(mydata.df) #convert data frame to vector format
emotion.df <- get_nrc_sentiment(word.df) #assign polarity score to each word. Stop words except in negation are neutral
emotion2.df <- cbind(mydata.df, emotion.df) #merge comments and scores

sent.value <- get_sentiment(word.df) #sentiment value
most.positive <- word.df[sent.value == max(sent.value)]
most.negative <- word.df[sent.value == min(sent.value)]
most.negative #makes sense
positive.tweets <- word.df[sent.value > 0]
negative.tweets <- word.df[sent.value < 0]
neutral.tweets <- word.df[sent.value == 0]

# Write CSV in R
write.csv(positive.tweets, file = "positive_tweets.csv")
write.csv(negative.tweets, file = "negative_tweets.csv")
write.csv(neutral.tweets, file = "neutral_tweets.csv")

positive = "positive_tweets.csv"
negative = "negative_tweets.csv"
neutral = "neutral_tweets.csv"

negative.df =read.csv(negative, header = TRUE, stringsAsFactors = TRUE, sep=',') #Negative subset dataframe
positive.df =read.csv(positive, header = TRUE, stringsAsFactors = TRUE, sep=',')#Positive subset dataframe
neutral.df =read.csv(neutral, header = TRUE, stringsAsFactors = TRUE, sep=',')#Neutral subset dataframe

wordsToRemove = c("the","can","will","just","dont","now") #stopword list is missing it out. Not working asp expected

# generate a function to analyse corpus text
analyseText_1 = function(text_to_analyse){
  
  # analyse text and generate matrix of words
  # Returns a dataframe containing 1 comment per row, one word per column
  # and the number of times the word appears per comment
  
  CorpusTranscript = Corpus(VectorSource(text_to_analyse))
  #CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordToRemove)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, stopwords("english"))
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordsToRemove)
  CorpusTranscript = tm_map(CorpusTranscript, content_transformer(tolower))
  # CorpusTranscript = tm_map(CorpusTranscript, PlainTextDocument, lazy = T)
  CorpusTranscript = DocumentTermMatrix(CorpusTranscript)
  
  CorpusTranscript = removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
  CorpusTranscript = as.data.frame(as.matrix(CorpusTranscript))
  colnames(CorpusTranscript) = make.names(colnames(CorpusTranscript))
  return(CorpusTranscript)
}

negative_words_analysis_3 = analyseText_1(negative.df$x) #Convert negative df to vector format 
positive_words_analysis_3 = analyseText_1(positive.df$x)#Convert positive df to vector format 
neutral_words_analysis_3 = analyseText_1(neutral.df$x)#Convert neutral df to vector format 
negative_words_analysis_3
positive_words_analysis_3
neutral_words_analysis_3

#=====================================
freqWords_neg = colSums(negative_words_analysis_3) #compute frequency of negative words in the negative subset
dim(negative_words_analysis_3)
glimpse(negative_words_analysis_3)

freqWords_neg = freqWords_neg[order(freqWords_neg, decreasing = T)]
freqWords_neg[0:10] #top 10 most frequent words mentioned in a negative aspect. Can be changed to any number within the dictionary

#========================
freqWords_pos = colSums(positive_words_analysis_3) #compute frequency of positive words in the positive subset
dim(positive_words_analysis_3)
glimpse(positive_words_analysis_3)

freqWords_pos = freqWords_pos[order(freqWords_pos, decreasing = T)]
freqWords_pos[0:10] #top 10 most frequent words mentioned in a positive aspect. Can be changed to any number within the dictionary

#=================================

freqWords_neutral = colSums(neutral_words_analysis_3)#compute frequency of neutral words in the neutral subset
dim(neutral_words_analysis_3)
glimpse(neutral_words_analysis_3)


freqWords_neutral = freqWords_neutral[order(freqWords_neutral, decreasing = T)]
freqWords_neutral[0:10] #top 10 most frequent words mentioned in a neutral aspect. Can be changed to any number within the dictionary
#=================================

#Convert subsets (positive, neutral and negative) to vector format for word association computation

#======================================

# Word associations with positive class
dim(positive.df)
pos_corpus <- VCorpus(VectorSource(positive.df$x))
pos_corpus <- tm_map(pos_corpus, PlainTextDocument)
pos_corpus <- tm_map(pos_corpus, removePunctuation)
pos_corpus <- tm_map(pos_corpus, removeWords, stopwords('english'))

# Word cloud representation of most dominant words in the positive  subset
wordcloud(pos_corpus, max.words = 70, random.order = FALSE, rot.per=0.8, colors=brewer.pal(8, "Dark2"))
tdm_pos <-TermDocumentMatrix(pos_corpus)

# Word associations with negative  class
dim(negative.df)

neg_corpus <- VCorpus(VectorSource(negative.df$x))
neg_corpus <- tm_map(neg_corpus, PlainTextDocument)
neg_corpus <- tm_map(neg_corpus, removePunctuation)
neg_corpus <- tm_map(neg_corpus, removeWords, stopwords('english'))

# Word cloud representation of most dominant words in the negative subset
wordcloud(neg_corpus, max.words = 50, random.order = FALSE, rot.per=0.9, colors=brewer.pal(9, "Dark2"))
tdm_neg <-TermDocumentMatrix(neg_corpus)

neg_associac <- findAssocs(tdm_neg, c('coronavirus', 'covid','fuck','shit','virus','time','bad','trump','really'), .1)
neg_associac
lapply(neg_associac, function(x) write.table( data.frame(x), 'associac_negative.csv'  , append= T, sep=',' )) 
#each x in the CSV represents the above keywords. Output couldnt capture it. Manual insertion in the above order should work. 

#=================================


