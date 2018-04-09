library(pROC)
#treebag with smote - 0.81
#rpart 0.68/0.62
#logitboost smote - 0.7144
#adbag - 0.68 /0.50
#glmboost 0.67/0.66
#lssvmpoly - error
#randomGLM - error

###try ranger - pure learning

fitControl <- trainControl(method = "cv",number = 3)
ranger_fit_smote <- train(yTrain~., data = smote_train, method = "ranger", trControl = fitControl)
x=smote_train[,!(names(smote_train) %in% "yTrain")]
test_class_pred <- predict(ranger_fit_smote, x)
out <- data.frame(prediction=test_class_pred,actual=smote_train$yTrain)
table(out)
test_class_pred <- predict(ranger_fit_smote, smote_train,type = "prob")
auc <- roc(smote_train$yTrain, test_class_pred$`1`)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))


ranger_fit <- train(yTrain~., data = data_corpus, method = "ranger", trControl = fitControl)
test_class_pred <- predict(ranger_fit, xTrain,type = "prob")
auc <- roc(data_corpus$yTrain, test_class_pred$`1`)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))

test_class_pred <- predict(ranger_fit, xTrain)
out <- data.frame(prediction=test_class_pred,actual=yTrain)
table(out)

test_final_ranger <- predict(ranger_fit_smote, mtest)
