#Naive bayes
#attach Data
View(sms_raw_NB)
str(sms_raw_NB)

sms_raw_NB$type<-as.factor(sms_raw_NB$type)
str(sms_raw_NB)

table(sms_raw_NB$type)
library(tm)
# Prepare corpuse for the text data 
sms_corpous<-Corpus(VectorSource(sms_raw_NB$text))
class(sms_corpous)

# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(sms_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)

corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
# Do not run the plainTextDocument
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
#as.character(corpus_clean)
# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
sms_dtm <- DocumentTermMatrix(corpus_clean) 
class(sms_dtm)
as.character(sms_dtm)

# creating training and test datasets
sms_raw_train <- sms_raw_NB[1:3901, ]
sms_raw_test  <- sms_raw_NB[3902:5559, ]

sms_dtm_train <- sms_dtm[1:3901, ]
sms_dtm_test  <- sms_dtm[3902:5559, ]

sms_corpus_train <- corpus_clean[1:3901]
sms_corpus_test  <- corpus_clean[3902:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
###  ham      spam 
#0.8649064 0.1350936 

prop.table(table(sms_raw_test$type))
#ham      spam 
#0.8649064 0.1350936 

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm, 5)


sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_dict


temp <- as.data.frame(as.matrix(sms_train))


View(temp)
dim(sms_train)
#[1] 3901 1624

dim(sms_test)
#1658 1624

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)
# sms_train <- as.data.frame(as.matrix(sms_train))
# sms_test <- as.data.frame(as.matrix(sms_test))
##  Training a model on the data ----

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
sms_test_pred2

confusionMatrix(sms_test_pred2,sms_raw_test$type)


library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier
mean(sms_test_pred2==sms_raw_test$type)############0.97466
