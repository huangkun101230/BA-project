getwd()
setwd("Documents/info264 wo/ws5/ws5/")

install.packages("twitteR")
install.packages("ROAuth") #login twitterlibrary
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

setup_twitter_oauth("HPJGYG2dFiTULom178mnQf2M2",
                    "9AjlrfLdhW1wr6fcFpgVVaNFfLI4aWYel2dkcBifBZyxPeM3vo",
                    "924990688692920320-jt1lv2qB4xdfRQRF0g2UVM94APuAIF5",
                    "eAghFrsI0yXrcMq1GJEqmwMKtoBKu3hei1FUYRynG9oU3")

(n.tweet<-length(tweets))

tweets.df <- twListToDF(tweets) 
#tweet 190 
tweets.df[190, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

# print tweet #190 and make text fit for slide width 
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
# remove extra whitespace 
myCorpus <- tm_map(myCorpus, stripWhitespace) 
# remove stopwords 
myStopwords <- c(stopwords("english"), c("hacker","hack","hacking","hackers","hacks","you","could","also",
                                         "dont","us","were","are","is","was","that","this","these","those","to",
                                         "didnt","oh","may","a","an","of","with","will","let","wo","for","its",
                                         "hi","o","dont","lo","if","now","in","at","next","you","we","that","did",
                                         "were","oh","get","thats","my","okay","just","never","havent","dm","amp",
                                         "i","ff","take","th","ff","pls","letting","can","shouldent","enable",
                                         "ds","ev","jc","kv","oh")) 
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# keep a copy for stem completion later 
myCorpusCopy <- myCorpus

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf))) 
tdm

inspect(tdm)

(freq.terms <- findFreqTerms(tdm,lowfreq = 10))

#ggplot2
term.freq <- rowSums(as.matrix(tdm)) 
term.freq <- subset(term.freq, term.freq >= 150) 
df <- data.frame(term = names(term.freq), freq = term.freq) 
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=7))

#wordcloud
m <- as.matrix(tdm) 
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T) 
# colors 
pal <- brewer.pal(9, "BuGn")[-(1:4)] 

# plot word cloud using wordcloud 
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, random.order = F, colors = pal)

findAssocs(tdm, "expert", 0.1)