## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load-libraries, message=FALSE, warning=FALSE-------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(dslabs)
library(data.table)
library(ggplot2)
library(plotly)
library(outliers)
library(qcc)
library(mctest)
library(ppcor)
library(car)
library(psych)
library(ggthemes)
library(corrplot)
library(DAAG)
library(GGally)
library(caret)
library(psych)
library(ggpubr)
library(tree)
library(randomForest)
library(vip)
library(rsample)
library(knitr)


## -------------------------------------------------------------------------------------------------------------------

data <- read.table("breast-cancer-wisconsin.data.txt", 
                   header = FALSE, 
                   stringsAsFactors = FALSE,
                   sep = ",", 
                   dec = ".")
head(data)


## -------------------------------------------------------------------------------------------------------------------
#transform response classes to 0 and 1:
data$V11[data$V11==2] <- 0
data$V11[data$V11==4] <- 1

#set value types
data <- transform(data,
                  #ID Number
                  V1=as.integer(data$V1),
                  V2=as.integer(data$V2),
                  V3=as.integer(data$V3),
                  V4=as.integer(data$V4),
                  V5=as.integer(data$V5),
                  V6=as.integer(data$V6),
                  V7=as.integer(data$V7),
                  V8=as.integer(data$V8),
                  V9=as.integer(data$V9),
                  V10=as.integer(data$V10),
                  V11=as.factor(data$V11))

#change column names
colnames(data) <- c("ID", "Clump_Thickness", "Uniformity_Size", "Uniformity_Shape",
                 "Marginal_Adhesion", "Single_Epith_Size", "Bare_Nuclei", "Bland_Chromatin",
                 "Normal_Nucleoli", "Mitoses", "Class")
summary(data)


## -------------------------------------------------------------------------------------------------------------------
na<-data[is.na(data$Bare_Nuclei),]
na


## -------------------------------------------------------------------------------------------------------------------
nrow(na)/nrow(data)*100


## -------------------------------------------------------------------------------------------------------------------
#create a new data set
data_mean_imp <- data

#imputation
data_mean_imp$Bare_Nuclei[is.na(data_mean_imp$Bare_Nuclei)] <- round(mean(data_mean_imp$Bare_Nuclei, na.rm=T))
data_mean_imp[24:41,]



## -------------------------------------------------------------------------------------------------------------------
#create indicators of missing or imputing vals data
Scales <- c(rep("Before (excluding NA's)", 10), rep("After Imputation", 10))

#use our 1-10 values as factors twcie to compare both sets
Values <- as.factor(rep(names(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])), 2))

#calculate how often those values appear
Count <- c(as.numeric(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])),
          as.numeric(table(data_mean_imp$Bare_Nuclei)))

#use all these values as a data frame for the plot 
bar_nuclei <- data.frame(Scales, Values, Count)

#finally, build the plot 
ggplot(bar_nuclei, aes(Values, Count, fill = Scales)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.title = element_blank())+
  ggtitle('Bare_nuclei Before and After Mode Imputation')


## -------------------------------------------------------------------------------------------------------------------
#mean before imputation
mean(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])

#mean after imputation
mean(data_mean_imp$Bare_Nuclei)



## -------------------------------------------------------------------------------------------------------------------
#values other than the missing ones in the column:
val <- unique(data$Bare_Nuclei[!is.na(data$Bare_Nuclei)])   

#Mode of Bare_nuclei
mode_barenuclei <- val[which.max(tabulate(match(data$Bare_Nuclei, val)))]
mode_barenuclei


## -------------------------------------------------------------------------------------------------------------------
#duplicate the data set
data_mode_imp <- data

#imputation
data_mode_imp$Bare_Nuclei[is.na(data_mode_imp$Bare_Nuclei)] <- mode_barenuclei
data_mode_imp[24:41,]


## -------------------------------------------------------------------------------------------------------------------
#create indicators of missing or imputing vals data
Scales <- c(rep("Before (excluding NA's)", 10), rep("After Imputation", 10))

#use our 1-10 values as factors twcie to compare both sets
Values <- as.factor(rep(names(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])), 2))

#calculate how often those values appear
Count <- c(as.numeric(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])),
          as.numeric(table(data_mode_imp$Bare_Nuclei)))

#use all these values as a data frame for the plot 
bar_nuclei <- data.frame(Scales, Values, Count)

#finally, build the plot 
ggplot(bar_nuclei, aes(Values, Count, fill = Scales)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.title = element_blank())+
  ggtitle('Bare_nuclei Before and After Mode Imputation')


## -------------------------------------------------------------------------------------------------------------------
#get numbers of rows with NA:
narows <- which(is.na(data$Bare_Nuclei)==T, arr.ind=T)

#create new df without response variable and without NA rows
data_mod <- data[-narows,2:10]
head(data_mod)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#5 fold cross validation with 10 repetitions
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
#use bare_nuclei as the response variable
reg <- train(Bare_Nuclei~.,
                     data=data_mod,
                     trControl=data_ctrl,
                     method="lm")
summary(reg)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
stepwise <- train(Bare_Nuclei~.,
                     data=data_mod,
                     trControl=data_ctrl,
                     method="lmStepAIC",
                     direction = "backward", trace=F)
summary(stepwise)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)

model <- train(Bare_Nuclei ~ Clump_Thickness + Uniformity_Shape + Marginal_Adhesion + Bland_Chromatin,
                     data=data_mod,
                     trControl=data_ctrl,
                     method="lm")
summary(model)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
bare_nuclei_pred <- predict(model, newdata=data[narows,])
bare_nuclei_pred


## -------------------------------------------------------------------------------------------------------------------
data_reg_imp <- data
data_reg_imp[narows,]$Bare_Nuclei <- round(bare_nuclei_pred)
summary(data_reg_imp$Bare_Nuclei)


## -------------------------------------------------------------------------------------------------------------------
#create indicators of missing or imputing vals data
Scales <- c(rep("Before (excluding NA's)", 10), rep("After Imputation", 10))

#use our 1-10 values as factors twcie to compare both sets
Values <- as.factor(rep(names(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])), 2))

#calculate how often those values appear
Count <- c(as.numeric(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])),
          as.numeric(table(data_reg_imp$Bare_Nuclei)))

#use all these values as a data frame for the plot 
bar_nuclei <- data.frame(Scales, Values, Count)

#finally, build the plot 
ggplot(bar_nuclei, aes(Values, Count, fill = Scales)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.title = element_blank())+
  ggtitle('Bare_nuclei Before and After Regression Imputation')


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
pert <- rnorm(nrow(data[narows,]), 
              mean=bare_nuclei_pred,
              sd=sd(bare_nuclei_pred))
pert


## -------------------------------------------------------------------------------------------------------------------
pert <- abs(round(pert))
pert



## -------------------------------------------------------------------------------------------------------------------
summary(pert)


## -------------------------------------------------------------------------------------------------------------------
pert[which.min(pert)]=1
pert


## -------------------------------------------------------------------------------------------------------------------
data_pert_imp <- data
data_pert_imp[narows,]$Bare_Nuclei <- pert
summary(data_pert_imp$Bare_Nuclei)


## -------------------------------------------------------------------------------------------------------------------
#create indicators of missing or imputing vals data
Scales <- c(rep("Before (excluding NA's)", 10), rep("After Imputation", 10))

#use our 1-10 values as factors twcie to compare both sets
Values <- as.factor(rep(names(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])), 2))

#calculate how often those values appear
Count <- c(as.numeric(table(data$Bare_Nuclei[is.na(data$Bare_Nuclei) == FALSE])),
          as.numeric(table(data_pert_imp$Bare_Nuclei)))

#use all these values as a data frame for the plot 
bar_nuclei <- data.frame(Scales, Values, Count)

#finally, build the plot 
ggplot(bar_nuclei, aes(Values, Count, fill = Scales)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.title = element_blank())+
  ggtitle('Bare_nuclei Before and After Regression with Perturbation Imputation')


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#find the number of rows for test set
index <- round(nrow(data_mean_imp)*0.25,digits=0)

#randomly sample the rows which will go to the test set (20%)
test_sample <- sample(1:nrow(data_mean_imp), index)

#create the test set - exclude id column
mean_test <- data_mean_imp[test_sample,2:11]
head(mean_test)

#create the train set
mean_train <- data_mean_imp[-test_sample,2:11]
head(mean_train)

#CHECK PROPORTIONS:

#whole set
prop.table(table(data_mean_imp$Class))

#test set
prop.table(table(mean_test$Class))

#train set
prop.table(table(mean_train$Class))


## -------------------------------------------------------------------------------------------------------------------

#create the test set - exclude id column
mode_test <- data_mode_imp[test_sample,2:11]
head(mode_test)

#create the train set
mode_train <- data_mode_imp[-test_sample,2:11]
head(mode_train)



## -------------------------------------------------------------------------------------------------------------------

#create the test set - exclude id column
reg_test <- data_reg_imp[test_sample,2:11]
head(reg_test)

#create the train set
reg_train <- data_reg_imp[-test_sample,2:11]
head(reg_train)



## -------------------------------------------------------------------------------------------------------------------

#create the test set - exclude id column
pert_test <- data_pert_imp[test_sample,2:11]
head(pert_test)

#create the train set
pert_train <- data_pert_imp[-test_sample,2:11]
head(pert_train)



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
mean_knn <- train(as.factor(Class)~.,#Class - response values
                      mean_train, 
                      method = "knn",
                      preProcess = c("center", "scale"), #standardize the data
                      tuneGrid = data.frame(k = c(1:50)), #use k values from 1 to 50
                      trControl = trainControl(
                        method = "repeatedcv", #repeated cross validation
                        number = 5, #5 k-folds
                        repeats = 10)) #repeating cross-validation 10 times, each time the folds are split in a different way

mean_knn


## -------------------------------------------------------------------------------------------------------------------
plot(mean_knn)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
mode_knn <- train(as.factor(Class)~.,
                      mode_train, 
                      method = "knn",
                      preProcess = c("center", "scale"), 
                      tuneGrid = data.frame(k = c(1:50)), 
                      trControl = trainControl(
                        method = "repeatedcv", 
                        number = 5, 
                        repeats = 10)) 

mode_knn


## -------------------------------------------------------------------------------------------------------------------
plot(mode_knn)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
reg_knn <- train(as.factor(Class)~.,
                      reg_train, 
                      method = "knn",
                      preProcess = c("center", "scale"), 
                      tuneGrid = data.frame(k = c(1:50)), 
                      trControl = trainControl(
                        method = "repeatedcv", 
                        number = 5, 
                        repeats = 10)) 

reg_knn


## -------------------------------------------------------------------------------------------------------------------
plot(reg_knn)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
pert_knn <- train(as.factor(Class)~.,
                      pert_train, 
                      method = "knn",
                      preProcess = c("center", "scale"), 
                      tuneGrid = data.frame(k = c(1:50)), 
                      trControl = trainControl(
                        method = "repeatedcv", 
                        number = 5, 
                        repeats = 10)) 

pert_knn


## -------------------------------------------------------------------------------------------------------------------
plot(pert_knn)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_mean <- predict(mean_knn,mean_test)

#confusion matrix
cm_mean <- table(Actual=mean_test$Class, Predicted=predict_mean)
cm_mean

#calculate accuracy
accuracy_mean <- sum(predict_mean == mean_test[,10])/nrow(mean_test)
accuracy_mean


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_mode <- predict(mode_knn,mode_test)

#confusion matrix
cm_mode <- table(Actual=mode_test$Class, Predicted=predict_mode)
cm_mode

#calculate accuracy
accuracy_mode <- sum(predict_mode == mode_test[,10])/nrow(mode_test)
accuracy_mode


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_reg <- predict(reg_knn,reg_test)

#confusion matrix
cm_reg <- table(Actual=reg_test$Class, Predicted=predict_reg)
cm_reg

#calculate accuracy
accuracy_reg <- sum(predict_reg == reg_test[,10])/nrow(reg_test)
accuracy_reg


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_pert <- predict(pert_knn,pert_test)

#confusion matrix
cm_pert <- table(Actual=pert_test$Class, Predicted=predict_pert)
cm_pert

#calculate accuracy
accuracy_pert <- sum(predict_pert == pert_test[,10])/nrow(pert_test)
accuracy_pert


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
mean_svm <- train(as.factor(Class)~.,
                          mean_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = seq.int(0.01, 100, length = 20)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
mean_svm




## -------------------------------------------------------------------------------------------------------------------
plot(mean_svm)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
mode_svm <- train(as.factor(Class)~.,
                          mode_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = seq.int(0.01, 100, length = 20)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
mode_svm




## -------------------------------------------------------------------------------------------------------------------
plot(mode_svm)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
reg_svm <- train(as.factor(Class)~.,
                          reg_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = seq.int(0.01, 100, length = 20)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
reg_svm




## -------------------------------------------------------------------------------------------------------------------
plot(reg_svm)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
pert_svm <- train(as.factor(Class)~.,
                          pert_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = seq.int(0.01, 100, length = 20)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
pert_svm




## -------------------------------------------------------------------------------------------------------------------
plot(pert_svm)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_mean_svm <- predict(mean_svm,mean_test)

#confusion matrix
cm_mean_svm <- table(Actual=mean_test$Class, Predicted=predict_mean_svm)
cm_mean_svm

#calculate accuracy
accuracy_mean_svm <- sum(predict_mean_svm == mean_test[,10])/nrow(mean_test)
accuracy_mean_svm


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_mode_svm <- predict(mode_svm,mode_test)

#confusion matrix
cm_mode_svm <- table(Actual=mode_test$Class, Predicted=predict_mode_svm)
cm_mode_svm

#calculate accuracy
accuracy_mode_svm <- sum(predict_mode_svm == mode_test[,10])/nrow(mode_test)
accuracy_mode_svm


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_reg_svm <- predict(reg_svm,reg_test)

#confusion matrix
cm_reg_svm <- table(Actual=reg_test$Class, Predicted=predict_reg_svm)
cm_reg_svm

#calculate accuracy
accuracy_reg_svm <- sum(predict_reg_svm == reg_test[,10])/nrow(reg_test)
accuracy_reg_svm


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_pert_svm <- predict(pert_svm,pert_test)

#confusion matrix
cm_pert_svm <- table(Actual=pert_test$Class, Predicted=predict_pert_svm)
cm_pert_svm

#calculate accuracy
accuracy_pert_svm <- sum(predict_pert_svm == pert_test[,10])/nrow(pert_test)
accuracy_pert_svm


## -------------------------------------------------------------------------------------------------------------------
rem_data <- data[-narows,]
rem_data[23:30,]


## -------------------------------------------------------------------------------------------------------------------
set.seed(3)

#find the number of rows for test set
index <- round(nrow(rem_data)*0.25,digits=0)

#randomly sample the rows which will go to the test set (20%)
test_sample <- sample(1:nrow(rem_data), index)

#create the test set - exclude id column
rem_test <- rem_data[test_sample,-1]
head(rem_test)

#create the train set
rem_train <- rem_data[-test_sample,-1]
head(rem_train)

#CHECK PROPORTIONS:

#whole set
prop.table(table(rem_data$Class))

#test set
prop.table(table(rem_test$Class))

#train set
prop.table(table(rem_train$Class))


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
rem_knn <- train(as.factor(Class)~ .,
                      rem_train, 
                      method = "knn",
                      preProcess = c("center", "scale"), 
                      tuneGrid = data.frame(k = c(1:50)), 
                      trControl = trainControl(
                        method = "repeatedcv",
                        number = 5, 
                        repeats = 10)) 

rem_knn


## -------------------------------------------------------------------------------------------------------------------
plot(rem_knn)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_rem <- predict(rem_knn,rem_test)

#confusion matrix
cm_rem <- table(Actual=rem_test$Class, Predicted=predict_rem)
cm_rem

#calculate accuracy
accuracy_rem <- sum(predict_rem == rem_test[,10])/nrow(rem_test)
accuracy_rem


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
rem_svm <- train(as.factor(Class)~.,
                          rem_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = seq.int(0.01, 100, length = 20)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
rem_svm




## -------------------------------------------------------------------------------------------------------------------
plot(rem_svm)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_rem_svm <- predict(rem_svm,rem_test)

#confusion matrix
cm_rem_svm <- table(Actual=rem_test$Class, Predicted=predict_rem_svm)
cm_rem_svm

#calculate accuracy
accuracy_rem_svm <- sum(predict_rem_svm == rem_test[,10])/nrow(rem_test)
accuracy_rem_svm


## -------------------------------------------------------------------------------------------------------------------
binary <- data
binary$Missing[is.na(data$Bare_Nuclei)==T] <- 0
binary$Missing[is.na(data$Bare_Nuclei)==F] <- 1

binary[23:30,]


## -------------------------------------------------------------------------------------------------------------------
#0 if NA
binary$Interact[is.na(data$Bare_Nuclei)==T] <- 0

#normal value if not NA
binary$Interact[is.na(data$Bare_Nuclei)==F] <- as.integer(data[-narows,]$Bare_Nuclei)

binary[23:30,]


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#find the number of rows for test set
index <- round(nrow(binary)*0.25,digits=0)

#randomly sample the rows which will go to the test set (20%)
test_sample <- sample(1:nrow(binary), index)

#create the test set - exclude id column
bin_test <- binary[test_sample,-1]
head(bin_test)

#create the train set
bin_train <- binary[-test_sample,-1]
head(bin_train)

#CHECK PROPORTIONS:

#whole set
prop.table(table(binary$Class))

#test set
prop.table(table(bin_test$Class))

#train set
prop.table(table(bin_train$Class))


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
bin_knn <- train(as.factor(Class)~ Clump_Thickness + Uniformity_Size + Uniformity_Shape + Marginal_Adhesion + Single_Epith_Size + Bland_Chromatin + Normal_Nucleoli + Mitoses + Interact,
                      bin_train, 
                      method = "knn",
                      preProcess = c("center", "scale"), 
                      tuneGrid = data.frame(k = c(1:50)), 
                      trControl = trainControl(
                        method = "repeatedcv",
                        number = 5, 
                        repeats = 10)) 

bin_knn


## -------------------------------------------------------------------------------------------------------------------
plot(bin_knn)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_bin <- predict(bin_knn,bin_test)

#confusion matrix
cm_bin <- table(Actual=bin_test$Class, Predicted=predict_bin)
cm_bin

#calculate accuracy
accuracy_bin <- sum(predict_bin == bin_test[,10])/nrow(bin_test)
accuracy_bin


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
bin_svm <- train(as.factor(Class)~ Clump_Thickness + Uniformity_Size + Uniformity_Shape + Marginal_Adhesion + Single_Epith_Size + Bland_Chromatin + Normal_Nucleoli + Mitoses + Interact,
                          bin_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = seq.int(0.01, 100, length = 20)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
bin_svm




## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
bin_svm <- train(as.factor(Class)~ Clump_Thickness + Uniformity_Size + Uniformity_Shape + Marginal_Adhesion + Single_Epith_Size + Bland_Chromatin + Normal_Nucleoli + Mitoses + Interact,
                          bin_train, 
                          method = "svmLinear", 
                          preProcess = c("center", "scale"), 
                          tuneGrid = expand.grid(C = c(0.01, 0.1, 0.5, 1, 2.5, 5)),
                          trControl = trainControl(
                            method = "repeatedcv", 
                            number = 5, 
                            repeats = 10)) 
bin_svm


## -------------------------------------------------------------------------------------------------------------------
plot(bin_svm)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#make the prediction
predict_bin_svm <- predict(bin_svm,bin_test)

#confusion matrix
cm_bin_svm <- table(Actual=bin_test$Class, Predicted=predict_bin_svm)
cm_bin_svm

#calculate accuracy
accuracy_bin_svm <- sum(predict_bin_svm == bin_test[,10])/nrow(bin_test)
accuracy_bin_svm


## -------------------------------------------------------------------------------------------------------------------
# Define the data
knn <- tibble::tribble(
    ~Type, ~Best_k, ~Accuracy_Train_CV, ~Accuracy_Test,
    "Mean Imputation",       3,           "97.14%",       "95.43%",
    "Mode Imputation",       3,           "97.16%",       "95.43%",
    "Regression Imputation",       3,           "97.16%",       "95.43%",
    "Regression with Perturbation Imputation",       3,           "97.16%",       "95.43%",
    "Removal of Missing Values",       9,           "97.19%",       "95.32%",
    "Binary Variable with Interaction Factor",       3,           "97.18%",       "95.43%"
)

# Use knitr's kable function to generate the table
knitr::kable(knn, digits = 3, row.names = FALSE, align = 'c')



## -------------------------------------------------------------------------------------------------------------------
# svm tibble
svm <- tibble::tribble(
  ~Type, ~Best_C, ~Accuracy_Train_CV, ~Accuracy_Test,
  "Mean Imputation", 0.01,  "97.12%", "96.00%",
  "Mode Imputation", 0.01,  "97.12%", "96.00%",
  "Regression Imputation", 0.01,  "97.12%", "96.57%",
  "Regression with Perturbation Imputation", 0.01,  "97.14%", "96.00%",
  "Removal of Missing Values", 0.01,  "97.23%", "96.49%",
  "Binary Variable with Interaction Factor", 1,  "97.00%", "96.57%"
)

# Display the table
kable(svm, digits = 3, row.names = FALSE, align = "c")


