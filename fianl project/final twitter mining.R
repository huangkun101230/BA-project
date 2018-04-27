getwd()
setwd("Documents/info264 wo/fianl project/")

install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("cluster")
install.packages("fpc")
install.packages("igraph")
install.packages("httr")
install.packages("biclust")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("plyr")
install.packages("stringr")
install.packages("sentimentr")
install.packages("dplyr")
install.packages("scales")
install.packages("syuzhet")
install.packages("reshape2")
install.packages("lubridate")
install.packages("zoom")

library(twitteR)
library(ROAuth) #login twitterlibrary
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(cluster)
library(fpc)
library(igraph)
library(httr)
library(biclust)
library(SnowballC)
library(topicmodels)
library(reshape2)
library(plyr)
library(stringr)
library(sentimentr)
library(dplyr)
library(scales)
library(syuzhet)
library(lubridate)
library(reshape2)
library(zoom)

tweets<-readRDS("dominos.Rds")
(n.tweet<-length(tweets))

tweets.df<-twListToDF(tweets)
tweets.df[190, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

writeLines(strwrap(tweets.df$text[190], 60))

# build a corpus, and specify the source to be character vectors 
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case 
myCorpus <- tm_map(myCorpus, content_transformer(tolower)) 
# remove URLs 
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 
# remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) 

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "@")
myCorpus <- tm_map(myCorpus, toSpace, "\\|")

# remove extra whitespace 
myCorpus <- tm_map(myCorpus, stripWhitespace)
# remove stopwords 
myStopwords <- c(stopwords("english"), c("you","could","also","dont","us","were",
                                         "are","is","was","that","this","these",
                                         "those","to","didnt","oh","may","a","an",
                                         "of","with","will","let","for","its",
                                         "hi","o","dont","if","now","in","at",
                                         "next","you","we","that","did","were",
                                         "get","thats","my","okay","just","never",
                                         "havent","i","take","th","letting","can",
                                         "shouldent","enable","oh","thank","yo",
                                         "thanks","h","wasnt","ty","isnt","well",
                                         "got","better"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# keep a copy for stem completion later 
myCorpusCopy <- myCorpus

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf))) 
tdm
inspect(tdm)

(freq.terms <- findFreqTerms(tdm, lowfreq = 10))

term.freq <- rowSums(as.matrix(tdm)) 
term.freq <- subset(term.freq, term.freq >= 180) 
df <- data.frame(term = names(term.freq), freq = term.freq) 
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=7))

term.freq <- subset(term.freq, term.freq >= 20) 

m <- as.matrix(tdm) 
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T) 
# colors 
pal <- brewer.pal(9, "BuGn")[-(1:4)] 

# plot word cloud using wordcloud 
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

#Set parameters for Gibbs sampling
findAssocs(tdm, "pls", 0.1)

pos<-scan("positive_word.txt",what = "character",comment.char = ";")
neg <- scan("negative_word.txt",what="character",comment.char=";")
source("sentiment.R")

# classify emotion
class_emo <- classify_emotion(tdm, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

sb<-myCorpusCopy
ss<-iconv(myCorpusCopy,to='utf-8-mac')
s<-get_nrc_sentiment(ss)
head(s)
ss[4]
get_nrc_sentiment("delay")

barplot(colSums(s),
        las=2,
        col = rainbow(10),
        ylab = "Count",
        main = "Sentiment Score for Domino's Tweets",
        ylim = c(0,80),
        xlim = c(0,12),
        cex.axis = 1)
