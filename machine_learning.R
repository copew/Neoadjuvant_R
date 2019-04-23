#load libraries
library(readr)
library(caret)
library(pROC)
#load the features dataframe

#load training list
training_list <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/training_list.csv", col_names = F)
training_list <- data.frame(training_list)
colnames(transneo_imageID)[2] <- "Trial_ID"
colnames(training_list)[2] <- "Trial_ID"
merged_features_trialID <- merge(merged_features, transneo_imageID, by="ImageID")

training_df <- merge(training_list, merged_features_trialID, by="Trial_ID")
training_df <- training_df[-2]
training_df <- merge(training_df, transneo_rcb_only, by="Trial_ID")
#remove NA from rcb
row_na <- which(is.na(training_df$RCB))
training_df <- training_df[-row_na, ]
training_df <- data.frame(training_df)

testing_df <- merged_features_trialID[-which(merged_features_trialID$Trial_ID %in% training_df$Trial_ID), ]
testing_df <- merge(testing_df, transneo_rcb_only, by="Trial_ID")
row_na <- which(is.na(testing_df$RCB))
testing_df <- testing_df[-row_na, ]
training_df_nona <- na.omit(training_df)
#set seed
set.seed(825)

fitControl <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs = T, savePredictions = TRUE) #, summaryFunction = twoClassSummary)
response_fit <- train(RCB ~ ., data=training_df_nona[, c(5:42,101, 54:91)], method="rf", trControl=fitControl)
#this is done with all the features......

# Random Forest 
# 
# 95 samples
# 76 predictors
# 2 classes: 'pCR', 'RD' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 86, 86, 85, 85, 86, 85, ... 
# Resampling results across tuning parameters:
#   
#   mtry  ROC        Sens       Spec     
# 2    0.5535516  0.0700000  0.9692857
# 39    0.6071627  0.2750000  0.8878571
# 76    0.6097619  0.2966667  0.8769048
# 
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 76.




testing_df$pcr_rd <- ifelse(testing_df$RCB=="pCR", "pCR", "RD")
training_df$pcr_rd <- ifelse(training_df$RCB=="pCR", "pCR", "RD")

fitControl <- trainControl(method="repeatedcv", number=10, repeats=10)
response_fit <- train(pcr_rd ~ ., data=training_df[, c(3:21, 23)], method="rf", trControl=fitControl)

response_fit
ml_result <- predict(response_fit, newdata=testing_df[,3:21])
result_table <- cbind(testing_df[,c(1, 23)], ml_result)

#this is not working because buffer and in are treated separtely
#so i'm separating them and them merge

training_df_in <- training_df[which(training_df$location=="in"), ]
training_df_buffer <- training_df[which(training_df$location=="buffer"),]
training_df_new <- merge(training_df_in, training_df_buffer, by=c("ImageID", "Trial_ID", "RCB", "pcr_rd"))

fitControl <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs = T, savePredictions = TRUE, summaryFunction = twoClassSummary)
response_fit <- train(pcr_rd ~ ., data=training_df_new[, c(4, 6:23, 25:42)], method="rf", trControl=fitControl, metric="ROC")

response_fit_svm <- train(pcr_rd ~ ., data=training_df_new[, c(4, 6:23, 25:42)], method="svmLinear", trControl=fitControl, metric="ROC")
#ok this doesn't work


