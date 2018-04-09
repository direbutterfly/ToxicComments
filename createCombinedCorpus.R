setwd("/Users/disha.gupta1@ibm.com/Desktop/ToxicOnlineComments")
list.files()
trainS <- read.csv("train.csv")
testS <- read.csv("test.csv")

library(tm)
data = c(as.character(trainS$comment_text),as.character(testS$comment_text))
n = nrow(trainS)+nrow(testS)
s = 1
corpus = Corpus(VectorSource(data[s:n]))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))

lb = 0.05 * n
ub = 0.95 * n
dtm = DocumentTermMatrix(corpus, control = list(bounds = list(global = c(lb, ub))))
