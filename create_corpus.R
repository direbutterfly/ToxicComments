#setwd("/Users/disha.gupta1@ibm.com/Desktop/ToxicOnlineComments")
#list.files()
#trainS <- read.csv("train.csv")
#testS <- read.csv("test.csv")

#library(tm)
data = trainS$comment_text
n = nrow(trainS)
s = 1
corpus = Corpus(VectorSource(data[s:n]))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtmTrain = DocumentTermMatrix(corpus, control = list(dictionary = Terms(dtm)))
m = as.matrix(dtmTrain)

#add different kind of text features for the data
m = cbind(m, text_features(data[s:n]))
