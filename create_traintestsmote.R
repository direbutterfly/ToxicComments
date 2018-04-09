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