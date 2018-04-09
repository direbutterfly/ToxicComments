ensemble <- function (smote_train,test_data) {
# shuffle and split the data into three parts
set.seed(1234)
smote_train <- smote_train[sample(nrow(smote_train)),]
split <- floor(nrow(smote_train)/2)
ensembleData <- smote_train[0:split,]
blenderData <- smote_train[(split+1):nrow(smote_train),]
testingData <- test_data

# set label name and predictors
labelName <- 'yTrain'
predictors <- names(ensembleData)[names(ensembleData) != labelName]

# create a caret control object to control the number of cross-validations performed
myControl <- trainControl(method='cv', number=3, returnResamp='none')

# train all the ensemble models with ensembleData
model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=myControl)
model_logitboost <- train(ensembleData[,predictors], ensembleData[,labelName], method='LogitBoost', trControl=myControl)
model_C5.0 <- train(ensembleData[,predictors], ensembleData[,labelName], method='C5.0', trControl=myControl)

# get predictions for each ensemble model for two last data sets
# and add them back to themselves
blenderData$LB_PROB <- predict(object=model_logitboost, blenderData[,predictors])
blenderData$C5_PROB <- predict(object=model_C5.0, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
testingData$LB_PROB <- predict(object=model_logitboost, testingData[,predictors])
testingData$C5_PROB <- predict(object=model_C5.0,testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])

# run a final model to blend all the probabilities together
predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='treebag', trControl=myControl)

# See final prediction and AUC of blended ensemble
preds <- predict(object=final_blender_model, testingData[,predictors],type = "prob")$`1`

return(preds)
}

