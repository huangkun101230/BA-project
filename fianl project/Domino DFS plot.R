getwd()
setwd("Documents/info264 wo/fianl project/")
myData<-read.csv("fianl data(Domino).csv")
View(myData)

plot(myData$Domestic.franchise.stores,myData$Domestic.franchise..m.,
     main = "FRANCHISE STORES",xlab = "Domestic franchise stores",ylab = "Domestic franchise stores' Revenue")
fit<-lm(myData$Domestic.franchise..m.~myData$Domestic.franchise.stores)
abline(fit)

fit
