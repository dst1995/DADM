setwd("C:/Users/Douwe/Documents/R projects/DADM")



library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)
registerDoMC(cores=detectCores())  # Use all available cores

#inlezen van data 
df<- read.csv("movie-pang02.csv", stringsAsFactors = FALSE)
View(df)

#data randomize
set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
View(df)

# Convert the 'class' variable from character to factor.
df$class <- as.factor(df$class)

#Bag of Words Tokenistation 
corpus <- Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:3])

#Data cleanup
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

#Matrix representation of Bag of Words : The Document Term Matrix
dtm <- DocumentTermMatrix(corpus.clean) #standard functie uit tm library
inspect(dtm[40:50, 10:15])

#===

#partitioning the data
df.train <- df[1:1500,]
df.test <- df[1501:2000,]

dtm.train <- dtm[1:1500,]
dtm.test <- dtm[1501:2000,]

corpus.clean.train <- corpus.clean[1:1500]
corpus.clean.test <- corpus.clean[1501:2000]

#Feature selection, woorden eruit filteren die minder dan 5 reviews voorkomen
dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# Train the classifier
system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$class )

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$class)
conf.mat

conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']
