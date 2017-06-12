
install.packages("e1071",repos = "http://cran.us.r-project.org")
library(e1071)
install.packages("RTextTools",repos = "http://cran.us.r-project.org")
library(RTextTools)

library(tm)
setwd("/Users/Douwe/DADM")
directory.location <-   paste(getwd(),"/reviews/",sep = "")  

#reviews inlezen in de wd
docs <- Corpus(DirSource(directory.location, encoding = "UTF-8"))
summary(docs)
writeLines(as.character(docs[[5]]))
getTransformations()

#overbodige tekens en woorden uit de dataset halen
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)

#train dataset maken
train <-data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)
dim(train)
head(train, 8)

#aangegven of reviews positief/negatief zijn
train$sentiment <-as.factor(c("up","up","up","up","down","up","up","down"))
dim(train)
head(train, 1)

#classifier trainen met traindata
classifier = naiveBayes(sentiment~.,data = train) 
summary(classifier)
str(classifier)

#raden pos/neg van de reviews
predicted = predict(classifier, train[,-1]); 
predicted

#resultaten weergeven
table(train$sentiment, predicted)
RTextTools::recall_accuracy(train$sentiment, predicted) #accuracy weergeven

#classifier gebruiken om zelf review te raden
test <- data.frame ("very great movie")
predicted = predict(classifier, test[,-1]); 
predicted






