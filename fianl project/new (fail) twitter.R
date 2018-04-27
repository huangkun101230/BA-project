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

library(twitteR)
library(ROAuth)
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

getwd()
setwd("Documents/info264 wo/fianl project/")
tweets<-readRDS("dominos.Rds")
(n.tweet<-length(tweets))
tweets.df<-twListToDF(tweets)
tweets.df[190,c("id","created","screenName","replyToSN","favoriteCount","retweetCount",
                "longitude","latitude","text")]
myCorpus<-Corpus(VectorSource(tweets.df$text))
myCorpus<-tm_map(myCorpus,content_transformer(tolower))
removeURL<-function(x)gsub("http[^[:space:]]*","",x)
myCorpus<-tm_map(myCorpus,content_transformer(removeURL))
removeNumPunct<-function(x)gsub("[^[:alpha:][:space:]]*","",x)
myCorpus<-tm_map(myCorpus,content_transformer(removeNumPunct))
myCorpus<-tm_map(myCorpus,stripWhitespace)
myStopwords <- c(stopwords("english"), c("hacker","hack","hacking","hackers","hacks","you","could","also",
                                         "dont","us","were","are","is","was","that","this","these","those","to",
                                         "didnt","oh","may","a","an","of","with","will","let","wo","for","its",
                                         "hi","o","dont","lo","if","now","in","at","next","you","we","that","did",
                                         "were","oh","get","thats","my","okay","just","never","havent","dm","amp",
                                         "i","ff","take","th","ff","pls","letting","can","shouldent","enable",
                                         "ds","ev","jc","kv","oh"))
myCorpus<-tm_map(myCorpus,removeWords,c(stopwords("english"), c("you","could","also","dont","us","were","are","is","was","that","this","these","those","to","didnt","oh","may","a","an","of","with","will","let","wo","for","its","hi","o","dont","lo","if","now","in","at","next","you","we","that","did","were","oh","get","thats","my","okay","just","never","havent","dm","amp","i","ff","take","th","pls","letting","can","shouldent","enable","ds","ev","jc","kv","oh"))
myCorpusCopy<-myCorpus
tdm<-TermDocumentMatrix(myCorpus,control = list(wordLengths=c(1,Inf)))
tdm
inspect(tdm)
(freq.terms<-findFreqTerms(tdm,lowfreq = 10))

term.freq <- rowSums(as.matrix(tdm)) 
term.freq <- subset(term.freq, term.freq >= 20) 
df <- data.frame(term = names(term.freq), freq = term.freq) 
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=7))

m <- as.matrix(tdm) 
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T) 
# colors 
pal <- brewer.pal(9, "BuGn")[-(1:4)] 

# plot word cloud using wordcloud 
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)
