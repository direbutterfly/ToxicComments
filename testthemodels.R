#test the model performance

toxic_index <- createDataPartition(toxic_data$yTrain, p = .8, list = FALSE, times = 1)
s_toxic_index <- createDataPartition(s_toxic_data$yTrain, p = .8, list = FALSE, times = 1)
obs_index <- createDataPartition(obs_data$yTrain, p = .8, list = FALSE, times = 1)
threat_index <- createDataPartition(threat_data$yTrain, p = .8, list = FALSE, times = 1)
insult_index <- createDataPartition(insult_data$yTrain, p = .8, list = FALSE, times = 1)
identityhate_index <- createDataPartition(identityhate_data$yTrain, p = .8, list = FALSE, times = 1)

toxic_pred = ranger(toxic_data[toxic_index,],toxic_data[-toxic_index,])
s_toxic_pred = ranger(s_toxic_data[s_toxic_index,],s_toxic_data[-s_toxic_index,])
obs_pred = ranger(obs_data[obs_index,],obs_data[-obs_index,])
threat_pred = ranger(threat_data[threat_index,],threat_data[-threat_index,])
insult_pred = ranger(insult_data[insult_index,],insult_data[-insult_index,])
identityhate_pred = ranger(identityhate_data[identityhate_index,],identityhate_data[-identityhate_index,])

toxic_cm = confusionMatrix(data = ifelse(toxic_pred>0.5,1,0),reference = toxic_data[-toxic_index,]$yTrain)
s_toxic_cm = confusionMatrix(data = ifelse(s_toxic_pred>0.5,1,0),reference = s_toxic_data[-s_toxic_index,]$yTrain)
obs_cm = confusionMatrix(data = ifelse(obs_pred>0.5,1,0),reference = obs_data[-obs_index,]$yTrain)
threat_cm = confusionMatrix(data = ifelse(threat_pred>0.5,1,0),reference = threat_data[-threat_index,]$yTrain)
insult_cm = confusionMatrix(data = ifelse(insult_pred>0.5,1,0),reference = insult_data[-insult_index,]$yTrain)
identityhate_cm = confusionMatrix(data = ifelse(identityhate_pred>0.5,1,0),reference = identityhate_data[-identityhate_index,]$yTrain)