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
