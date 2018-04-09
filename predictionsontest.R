library(DMwR) #for SMOTE
library(caret) #for classification models
set.seed(9560) #for SMOTE - same distributions time and again
#NOTE TO SELF: threat and identity hate are super duper biased datasets

#to test the code on smaller datasets
#trainS = trainS[1:1000,]
#m=m[1:1000,]

#create the training data set for each class of prediction
#apply SMOTE to balance the class representation
toxic_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$toxic),m))
s_toxic_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$severe_toxic),m))
obs_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$obscene),m))
threat_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$threat),m))
insult_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$insult),m))
identityhate_data = SMOTE(yTrain ~ ., data  = cbind.data.frame(yTrain = as.factor(trainS$identity_hate),m))

#ensemble model predictions on test data
toxic_pred = ensemble(toxic_data,mtest)
s_toxic_pred = ensemble(s_toxic_data,mtest)
obs_pred = ensemble(obs_data,mtest)
threat_pred = ensemble(threat_data,mtest)
insult_pred = ensemble(insult_data,mtest)
identity_hate_pred = ensemble(identityhate_data,mtest)

#ranger model predictions on test data
toxic_pred = ranger(toxic_data,mtest)
s_toxic_pred = ranger(s_toxic_data,mtest)
obs_pred = ranger(obs_data,mtest)
threat_pred = ranger(threat_data,mtest)
insult_pred = ranger(insult_data,mtest)
identity_hate_pred = ranger(identityhate_data,mtest)

#add a new feature using the prediction class from xgboost

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
write.csv(SUBMISSION,"fourth_submission.csv",row.names=FALSE) #dtm using words of both test and train data, also changed word limits from 10-90% to 5-95%

#SUBMISSION = read.csv("format_test_submission.csv")
