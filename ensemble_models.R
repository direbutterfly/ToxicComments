# shuffle and split the data into three parts
set.seed(1234)
smote_train <- smote_train[sample(nrow(smote_train)),]
split <- floor(nrow(smote_train)/3)
ensembleData <- smote_train[0:split,]
blenderData <- smote_train[(split+1):(split*2),]
testingData <- smote_train[(split*2+1):nrow(smote_train),]

# set label name and predictors
labelName <- 'yTrain'
predictors <- names(ensembleData)[names(ensembleData) != labelName]

# create a caret control object to control the number of cross-validations performed
myControl <- trainControl(method='cv', number=3, returnResamp='none')

# train all the ensemble models with ensembleData
model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl)
model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rpart', trControl=myControl)
model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=myControl)

# get predictions for each ensemble model for two last data sets
# and add them back to themselves
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])

# run a final model to blend all the probabilities together
predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)

# See final prediction and AUC of blended ensemble
preds <- predict(object=final_blender_model, testingData[,predictors])

