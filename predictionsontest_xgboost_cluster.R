library(DMwR) #for SMOTE
library(caret) #for classification models
library(clue)
library(SciencesPo)
library(xgboost)
set.seed(9560) #for SMOTE - same distributions time and again
#NOTE TO SELF: threat and identity hate are super duper biased datasets

#add a feature using cluster analysis
cl_pred <- function(inputdata,outputdata) {
norm_eucl <- function(m) apply(m, MARGIN=2, FUN=function(X) (X - min(X))/diff(range(X)))

m_norm <- norm_eucl(inputdata[,-1])
#m_binary = ifelse(inputdata[,-1]>0,1,0)
#m_new = cbind(m_norm,m_binary)

op_m_norm <- norm_eucl(outputdata)
#op_m_binary = ifelse(outputdata>0,1,0)
#op_m_new = cbind(op_m_norm,op_m_binary)

cl <- kmeans(m_norm, 40)
clusters = Dummify(x = cl$cluster)

predict <- cl_predict(cl,op_m_norm)
op_clusters = Dummify(x = as.vector(predict))
inputdata1 <- cbind(inputdata,clusters)
outputdata1 <- cbind(outputdata,op_clusters)

return (list( "ip" = inputdata1,"op"  = outputdata1))
}

xgb <- function(inputdata,outputdata,nrounds, eta) {
  trainDATA=data.matrix(inputdata[,-1])
  trainLABEL=data.matrix(inputdata[,1])
  testDATA=data.matrix(outputdata)
  model <- xgboost(data = trainDATA, label = trainLABEL,
                   nrounds = nrounds, objective = "binary:logistic",eta=eta,eval_metric="logloss")
  pred = predict(model, testDATA)
  return(pred)
}

#create the training data set for each class of prediction
#apply SMOTE to balance the class representation
toxic_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$toxic),m))
s_toxic_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$severe_toxic),m))
obs_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$obscene),m))
threat_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$threat),m))
insult_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$insult),m))
identityhate_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$identity_hate),m))

#write.csv(toxic_data,"toxic_data.csv",row.names=FALSE)
#write.csv(s_toxic_data,"s_toxic_data.csv",row.names=FALSE)
#write.csv(obs_data,"obs_data.csv",row.names=FALSE)
#write.csv(threat_data,"threat_data.csv",row.names=FALSE)
#write.csv(insult_data,"insult_data.csv",row.names=FALSE)
#write.csv(identityhate_data,"identityhate_data.csv",row.names=FALSE)

#text data with cluster prediction
toxic_test <- cl_pred(toxic_data,mtest)
s_toxic_test <- cl_pred(s_toxic_data,mtest)
obs_test <- cl_pred(obs_data,mtest)
threat_test <- cl_pred(threat_data,mtest)
insult_test <- cl_pred(insult_data,mtest)
identityhate_test <- cl_pred(identityhate_data,mtest)

#text data with cluster prediction
toxic_test <- xgb(toxic_data,mtest)
s_toxic_test <- xgb(s_toxic_data,mtest)
obs_test <- xgb(obs_data,mtest)
threat_test <- xgb(threat_data,mtest)
insult_test <- xgb(insult_data,mtest)
identityhate_test <- xgb(identityhate_data,mtest)

#xgboost model predictions on test data
toxic_pred = xgb(toxic_test$ip,toxic_test$op,200,0.5)
s_toxic_pred = xgb(s_toxic_test$ip,s_toxic_test$op,2000,0.001)
obs_pred = xgb(obs_test$ip,obs_test$op,50,0.8)
threat_pred = xgb(threat_test$ip,threat_test$op,1000,0.01)
insult_pred = xgb(insult_test$ip,insult_test$op,100,1)
identity_hate_pred = xgb(identityhate_test$ip,identityhate_test$op,90,0.8)


#code for final submission on kaggle
SUBMISSION = read.csv("sample_submission.csv")
SUBMISSION[,2]=toxic_pred
SUBMISSION[,3]=s_toxic_pred 
SUBMISSION[,4]=obs_pred
SUBMISSION[,5]=threat_pred
SUBMISSION[,6]=insult_pred
SUBMISSION[,7]=identity_hate_pred

#write.csv(SUBMISSION,"format_test_submission.csv",row.names=FALSE)
#write.csv(SUBMISSION,"first_submission.csv",row.names=FALSE) #random attempt at ensemble
#write.csv(SUBMISSION,"second_submission.csv",row.names=FALSE) #random attempt at ranger
#write.csv(SUBMISSION,"third_submission.csv",row.names=FALSE) #changing criteria to logloss with ranger
#write.csv(SUBMISSION,"fourth_submission.csv",row.names=FALSE) #dtm using words of both test and train data, also changed word limits from 10-90% to 5-95%
write.csv(SUBMISSION,"fifth_submission.csv",row.names=FALSE) #xgboosting with cluster feature

#SUBMISSION = read.csv("format_test_submission.csv")
