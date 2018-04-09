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
train=1000
y=as.factor(trainS$toxic)[1:train]
x <- as.list(as.character(trainS$comment_text[1:train]))
x <- matrix(x, ncol = 1)
colnames(x) <- "text"

#bad
cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
set.seed(849)
test_class_cv_model <- train(x, y, 
                             method = "svmExpoString",
                             tuneLength = 2, 
                             trControl = cctrl1)
test_class_pred <- predict(test_class_cv_model, x)
out <- data.frame(prediction=test_class_pred,actual=y)
table(out)

#good
x=data.frame(m[1:train,])
data_corpus <- cbind(y, x)
ranger_fit <- train(y~., data = data_corpus, method = "ranger", trControl = fitControl)
test_class_pred <- predict(ranger_fit, x)
out <- data.frame(prediction=test_class_pred,actual=y)
table(out)

#bad
nb_fit <- train(y~., data = data_corpus, method = "nb", trControl = fitControl)
test_class_pred <- predict(nb_fit, x)
out <- data.frame(prediction=test_class_pred,actual=y)
table(out)

#bad
nnet_fit <- train(y~., data = data_corpus, method = "nnet", trControl = fitControl)
test_class_pred <- predict(nnet_fit, x)
out <- data.frame(prediction=test_class_pred,actual=y)
table(out)
