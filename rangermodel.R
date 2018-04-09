ranger <- function (smote_train,test_data) {
  set.seed(1234)
  trainData <- smote_train[sample(nrow(smote_train)),]
  testingData <- test_data
  
  # set label name and predictors
  labelName <- 'yTrain'
  predictors <- names(trainData)[names(trainData) != labelName]
  
  # create a caret control object to control the number of cross-validations performed
  myControl <- trainControl(method='cv', number=5,classProbs = TRUE,summaryFunction = mnLogLoss)
  
  #colnames(trainData)=make.names(colnames(trainData))
  levels(trainData$yTrain) <- c("zero", "one")
  # train the ranger model
  model_ranger <- train(trainData[,predictors], as.factor(trainData[,labelName]), method='ranger', trControl=myControl)
  
  # get predictions 
  preds <- predict(object=model_ranger, testingData[,predictors],type = "prob")$one
  
  return(preds)
}

