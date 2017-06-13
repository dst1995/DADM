
install.packages("e1071",repos = "http://cran.us.r-project.org")
install.packages("RTextTools",repos = "http://cran.us.r-project.org")

library(e1071)
library(RTextTools)
library(readr)
library(tm)
library(dplyr)
#setwd("/Users/Douwe/DADM")
directory.location <-   paste(getwd(),"/reviews/",sep = "")  

#data inlezen
labeledTrainData <- read_delim("~/R projects/DADM/reviews/labeledTrainData.tsv", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)

docs <- Corpus(VectorSource(labeledTrainData$review))

summary(docs)
writeLines(as.character(docs[[5]]))
getTransformations()

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)

#train dataset maken
train <- data.frame(text=sapply(docs, identity), 
                        stringsAsFactors=F)
train$text <- as.vector(train$text)
#train$sentiment <- labeledTrainData$sentiment
dim(train)
head(train, 8)

#classifier trainen met traindata
#classifier <- naiveBayes(sentiment~.,data = train) 
classifier <- naiveBayes(labeledTrainData$sentiment,train$text, data = train, laplace = 1)
summary(classifier)
str(classifier)

#raden pos/neg van de reviews
predicted = predict(classifier, train[,-1]); 
predicted

#resultaten weergeven
table(labeledTrainData$sentiment, predicted)
RTextTools::recall_accuracy(train$sentiment, predicted) #accuracy weergeven

#classifier gebruiken om zelf review te raden
test <- data.frame ("very great movie")
predicted = predict(classifier, test[,-1]); 
predicted






