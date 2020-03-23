
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
library(syuzhet)#Emotions related to 8 expressions jere https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
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


setwd("Folder location with the tweets file")
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