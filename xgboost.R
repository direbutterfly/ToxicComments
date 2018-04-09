library(DMwR)
trainS$classtemp = paste(trainS$toxic,trainS$severe_toxic,trainS$obscene,
                         trainS$threat,trainS$insult,trainS$identity_hate,sep="")
trainS$classtemp=as.factor(trainS$classtemp)
temp = as.data.frame(table(trainS$classtemp))
temp = temp[order(-temp$Freq),]
temp$label = c(0:(nrow(temp)-1))
colnames(temp)[1]="classtemp"
trainS = merge(trainS,temp[,c("label","classtemp")],by="classtemp")
trainS$classtemp = NULL
colnames(trainS)[ncol(trainS)]="classtemp"

maxLevel = function(df,col){
  freqtable = as.data.frame(table(eval(parse(text=paste(df,"$", col, sep = "")))))
  freqtable = freqtable[order(-freqtable$Freq),]
  return (c(as.numeric(as.character(freqtable[1,1])),as.numeric(as.character(freqtable[2,1]))))
}

m=as.data.frame(m)
data  = cbind.data.frame(yTrain = as.factor(trainS$classtemp),m)

#dataNEW=data[data$yTrain %in% c(as.numeric(as.character(maxLevel("data","yTrain")))),]

datatemp = data[data$yTrain %in% maxLevel("data","yTrain"),]
datatemp$yTrain = factor(datatemp$yTrain)
dataNEW=SMOTE(yTrain ~ ., data  = datatemp)

#smote for each class
for (i in 2:40){
  for (j in unique(dataNEW$yTrain)) {
datatemp = rbind(dataNEW[dataNEW$yTrain %in% c(j),],data[data$yTrain %in% c(i),])
datatemp$yTrain = factor(datatemp$yTrain)
datatemp = SMOTE(yTrain ~ ., data  = datatemp)
dataNEW = rbind(dataNEW,datatemp[datatemp$yTrain %in% c(i,j),])
print(j) }
print(i)
print(table(dataNEW$yTrain))
}

library(SciencesPo)
clusters = Dummify(x = s_toxic_data$cluster)
toxic_data$cluster = NULL 
toxic_data = cbind(toxic_data,clusters)
  
m1=s_toxic_test$ip
trainIndex <- createDataPartition(m1$yTrain, p = 1.0, 
                                  list = FALSE, 
                                  times = 1)
trainLABEL <- m1$yTrain[trainIndex]
testLABEL <- m1$yTrain[-trainIndex]

trainDATA <- m1[trainIndex,]
testDATA <- m1[-trainIndex,]
trainDATA$yTrain=testDATA$yTrain=NULL

#library(xgboost)
trainDATA=data.matrix(trainDATA)
trainLABEL=data.matrix(trainLABEL)
testDATA=data.matrix(testDATA)
#model <- xgboost(data = trainDATA, label = trainLABEL,
#                 nrounds = 20, objective = "multi:softprob",eta=0.1,early_stopping_rounds = 2,
#                 num_class=41,eval_metric="mlogloss")
model <- xgb.cv(data = trainDATA, label = trainLABEL,
                 nrounds = 200, objective = "binary:logistic",eta=0.5,eval_metric="logloss",nfold=5)

model <- xgb.cv(data = trainDATA, label = trainLABEL,
                nrounds = 2000, objective = "binary:logistic",eta=0.001,eval_metric="logloss",nfold=5)

#performance on train data
pred = predict(model, trainDATA)
#out=as.data.frame(matrix(pred, ncol = 41, byrow = T))
#error_rate
sum(pred == trainLABEL) / length(trainLABEL)

#performance on test data
pred = predict(model, testDATA)
#a=data.frame(pred=pred,actual=testLABEL)
sum(pred == testLABEL) / length(testLABEL)

auc <- roc(as.factor(testLABEL),preds)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
