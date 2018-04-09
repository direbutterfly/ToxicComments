setwd("/Users/disha.gupta1@ibm.com/Desktop/ToxicOnlineComments")
list.files()
trainS <- read.csv("train.csv")

library(tm)
data = trainS$comment_text
n = nrow(trainS)
s = 1
corpus = Corpus(VectorSource(data[s:n]))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))

lb = 0.01 * n
ub = 0.7 * n
dtm = DocumentTermMatrix(corpus, control = list(bounds = list(global = c(lb, ub))))
inspect(dtm)
findFreqTerms(dtm)
m = as.matrix(dtm)
##add more features
library(stringr)
wordcount <- str_count(data[s:n], pattern = " ") #accuracy goes up
m = cbind(m, wordcount)

library(caret)
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 2)
#fitControl <- trainControl(method = "cv",number = 5)
train= round(0.8 * (n - s+1))
train=100
y=as.factor(trainS$toxic)[1:train]
X=as.matrix(as.character(trainS$comment_text[1:train]))
data_corpus <- cbind(y, data.frame(m[1:train,]))
data_string <- cbind(y, X)
ranger_fit <- train(y~., data = data_corpus, method = "ranger", trControl = fitControl)
svmExpoString <- train(y~X, data = data_string, method = "svmExpoString", trControl = fitControl)
nb_fit <- train(y~., data = data_corpus, method = "nb", trControl = fitControl)
mxnet <- train(y~., data = data_corpus, method = "nnet", trControl = fitControl)
