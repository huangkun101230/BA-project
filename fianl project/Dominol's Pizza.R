getwd()
setwd("Documents/info264 wo/fianl project/")

install.packages("SnowballC")
install.packages("biclust")

library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(cluster)
library(fpc)
library(igraph)
library(SnowballC)
library(biclust)

file.path <- "2016_Annual_Report_DPZ.txt"
text <- readLines(file.path)
docs <- Corpus(VectorSource(text))

toSpace <- content_transformer(function(x,pattern)gsub(pattern,"",x))
docs <- tm_map(docs,toSpace,"/")
docs <- tm_map(docs,toSpace,"@")
docs <- tm_map(docs,toSpace,"\\|")

docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))


docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs,removeNumbers)

docs <- tm_map(docs,removeWords,stopwords("english"))

docs <- tm_map(docs,removeWords,c("the","that","this","who","where","when",
                                  "which","a","able","about","above","all","i"
                                  ,"so","see","an","are","is","was","were","be",
                                  "them","then","c","b","t","value","new","and",
                                  "may","will","can","may","also","within","fair",
                                  "use","per","change","used","made","current",
                                  "changes","ended","us","l","k"))

docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word=names(v),freq=v)
head(d,10)

(freq.terms <- findFreqTerms(dtm,lowfreq = 20))

set.seed(1234)
wordcloud(words = d$word,freq = d$freq,min.freq = 1,max.words = 200,
          random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))

tdm <- TermDocumentMatrix(docs,control = list(wordLengths=c(1,Inf)))
tdm
inspect(tdm)
(freq.terms <- findFreqTerms(tdm, lowfreq = 10))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq,term.freq>=90)
df <- data.frame(term=names(term.freq),freq=term.freq)
ggplot(df,aes(x=term,y=freq))+geom_bar(stat = "identity")+xlab("Terms")+
  ylab("Count")+coord_flip()+theme(axis.text = element_text(size=7))

m <- as.matrix(tdm)
word.freq <- sort(rowSums(m),decreasing = T)
pal <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(words = names(word.freq),freq = word.freq,min.freq = 3,random.order = F,
          colors = pal)

findAssocs(tdm,"access",0.4)

tdm2 <- removeSparseTerms(tdm,sparse = 0.99)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix,method="ward.D")
plot(fit)
rect.hclust(fit,k=6)

plot(fit)
rect.hclust(fit,k=4)
