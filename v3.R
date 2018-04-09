setwd("/Users/disha.gupta1@ibm.com/Desktop/ToxicOnlineComments")
list.files()
trainS <- read.csv("train.csv")
testS <- read.csv("test.csv")

library(tm)
  data = trainS$comment_text
  n = nrow(trainS)
  s = 1
  corpus = Corpus(VectorSource(data[s:n]))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  lb = 0.1 * n
  ub = 0.9 * n
  dtm = DocumentTermMatrix(corpus, control = list(bounds = list(global = c(lb, ub))))
  inspect(dtm)
  findFreqTerms(dtm)
  m = as.matrix(dtm)
  ##add more features
  library(stringr)
  wordcount <- str_count(data[s:n], pattern = " ") #accuracy goes up
  m = cbind(m, wordcount)
  
  library(caret)
  y=as.factor(trainS$toxic)
  trainIndex <- createDataPartition(trainS$toxic, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  yTrain <- as.factor(trainS$toxic[ trainIndex])
  yTest  <- as.factor(trainS$toxic[-trainIndex])
  xTrain <- data.frame(m[trainIndex,])
  xTest <- data.frame(m[-trainIndex,])
  
  fitControl <- trainControl(method = "cv",number = 5)
  
  data_corpus <- cbind(yTrain, xTrain)
  #SMOTE to balance dataset (optional)
  library(DMwR)
  set.seed(9560)
  smote_train <- SMOTE(yTrain ~ ., data  = data_corpus)                         
  
  glmnet_fit_smote <- train(yTrain~., data = smote_train, method = "glmnet", trControl = fitControl)
  test_class_pred <- predict(glmnet_fit_smote, smote_train,type = "prob")
  
  library(pROC)
  auc <- roc(smote_train$yTrain, test_class_pred$`1`)
  print(auc)
  plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
  
  test_class_pred <- predict(glmnet_fit_smote, x)
  out <- data.frame(prediction=test_class_pred,actual=smote_train$yTrain)
  table(out)
  
  glmnet_fit <- train(yTrain~., data = data_corpus, method = "glmnet", trControl = fitControl)
  test_class_pred <- predict(glmnet_fit, xTrain,type = "prob")
  auc <- roc(data_corpus$yTrain, test_class_pred$`1`)
  print(auc)
  plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
  
  test_class_pred <- predict(glmnet_fit, xTrain)
  out <- data.frame(prediction=test_class_pred,actual=yTrain)
  table(out)

  #confusionMatrix(data = imbal_test$pred, reference = imbal_test$UserClass,positive = "yes", mode = "prec_recall")
  
  treebag_fit_smote <- train(yTrain~., data = smote_train, method = "treebag", trControl = fitControl)
  x=smote_train[,!(names(smote_train) %in% "yTrain")]
  test_class_pred <- predict(treebag_fit_smote, x)
  out <- data.frame(prediction=test_class_pred,actual=smote_train$yTrain)
  table(out)
  test_class_pred <- predict(treebag_fit_smote, smote_train,type = "prob")
  auc <- roc(smote_train$yTrain, test_class_pred$`1`)
  print(auc)
  plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
  
  
 treebag_fit <- train(yTrain~., data = data_corpus, method = "treebag", trControl = fitControl)
  test_class_pred <- predict(treebag_fit, xTrain,type = "prob")
  auc <- roc(data_corpus$yTrain, test_class_pred$`1`)
  print(auc)
  plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
  
  test_class_pred <- predict(treebag_fit, xTrain)
  out <- data.frame(prediction=test_class_pred,actual=yTrain)
  table(out)
  
#test data
data = testS$comment_text
n = nrow(testS)
s = 1
corpus = Corpus(VectorSource(data[s:n]))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtmtest = DocumentTermMatrix(corpus, control = list(dictionary = Terms(dtm)))
mtest = as.matrix(dtmtest)
##add more features
library(stringr)
wordcount <- str_count(data[s:n], pattern = " ") #accuracy goes up
mtest = cbind(mtest, wordcount)


