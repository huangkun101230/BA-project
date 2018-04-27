getwd()
setwd("Documents/info264 wo/fianl project/")
myData<-read.csv("fianl data(Domino).csv")
View(myData)

plot(myData$Domestic.Company.owned.stores,myData$Domestic.Company.owned.stores..m.,
     main = "COMPANY-OWNED STORES",xlab = "Domestic Company-owned stores",ylab = "Domestic Company-owned stores' Revenue")
fit<-lm(myData$Domestic.Company.owned.stores..m.~myData$Domestic.Company.owned.stores)
abline(fit)

fit
