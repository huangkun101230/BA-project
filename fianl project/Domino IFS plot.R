getwd()
setwd("Documents/info264 wo/fianl project/")
myData<-read.csv("fianl data(Domino).csv")
View(myData)

plot(myData$International.stores,myData$International.franchise,
     main = "INTERNATIONAL FRANCHISE STORES",xlab = "International franchise stores",ylab = "International franchise stores' Revenue")
fit<-lm(myData$International.franchise~myData$International.stores)
abline(fit)

fit
