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
library(ROCR)


## -------------------------------------------------------------------------------------------------------------------
data <- read.table("uscrime.txt", 
                   header = TRUE, 
                   stringsAsFactors = FALSE,
                   sep = "", 
                   dec = ".")
head(data)


## -------------------------------------------------------------------------------------------------------------------
test_data <- data.frame(M = 14.0,
                        So = 0, 
                        Ed = 10.0, 
                        Po1 = 12.0, 
                        Po2 = 15.5,
                        LF = 0.640, 
                        M.F = 94.0, 
                        Pop = 150, 
                        NW = 1.1, 
                        U1 = 0.120, 
                        U2 = 3.6, 
                        Wealth = 3200, 
                        Ineq = 20.1, 
                        Prob = 0.040,
                        Time = 39.0)
test_data


## -------------------------------------------------------------------------------------------------------------------
describe.by(data)


## -------------------------------------------------------------------------------------------------------------------
#melt data for easier visualization
melted<-melt(data)
#boxplots
box_plots <- ggplot(melted,
                    aes(x=factor(variable), y=value))+
              geom_boxplot(alpha=.5, fill="skyblue")+
              facet_wrap(~variable, ncol=8, scale="free")+
              theme_fivethirtyeight()

    
box_plots
#distribution plots with density
hist_plots <- ggplot(melted,
                    aes(x=value))+
              geom_histogram(aes(y=..density..), colour="black", fill="white")+
              geom_density(alpha=.3, color="skyblue", fill="skyblue")+
              facet_wrap(~variable, scale="free")+
              theme_fivethirtyeight()
hist_plots


## -------------------------------------------------------------------------------------------------------------------
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(data[,-16])
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(data)
corrplot(cor(data[,-16]), 
         method='pie', 
         type="upper", 
         #order="hclust",
         p.mat = p.mat, 
         sig.level = 0.1,
         insig = "blank")


## -------------------------------------------------------------------------------------------------------------------
set.seed(111)

tree1 <- tree(Crime~.,
              data=data)
summary(tree1)


## -------------------------------------------------------------------------------------------------------------------
plot(tree1)
text(tree1)
title(main = "Unpruned Regression Tree (1)")


## -------------------------------------------------------------------------------------------------------------------
tree1$frame


## -------------------------------------------------------------------------------------------------------------------
set.seed(111)
tree_ctrl <- tree.control(nobs=nrow(data), 
                          mincut=3, 
                          mindev=0.01)
tree2 <- tree(Crime~.,
                   data=data,
                   control=tree_ctrl
                   )
summary(tree2)



## -------------------------------------------------------------------------------------------------------------------
plot(tree2)
text(tree2)
title(main = "Unpruned Regression Tree (2)")


## -------------------------------------------------------------------------------------------------------------------
tree2$frame


## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
tree1_cv <- cv.tree(tree1)
tree1_cv


## -------------------------------------------------------------------------------------------------------------------
plot(tree1_cv$size, sqrt(tree1_cv$dev / nrow(data)), type = "b",
     xlab = "Tree Size", ylab = "CV-RMSE")


## -------------------------------------------------------------------------------------------------------------------
set.seed(111)
tree2_cv <- cv.tree(tree2)
tree2_cv
plot(tree2_cv$size, sqrt(tree2_cv$dev / nrow(data)), type = "b",
     xlab = "Tree Size", ylab = "CV-RMSE")


## -------------------------------------------------------------------------------------------------------------------
#prune
tree1_pruned <- prune.tree(tree1, best=6)
summary(tree1_pruned)
#plot
plot(tree1_pruned)
text(tree1_pruned)
title(main = "Pruned Regression Tree (1)")


## -------------------------------------------------------------------------------------------------------------------
#prune
tree2_pruned <- prune.tree(tree2, best=6)
summary(tree2_pruned)
#plot
plot(tree2_pruned)
text(tree2_pruned)
title(main = "Pruned Regression Tree (2)")


## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
#predictions
tree1pred <- predict(tree1)

#plot predicted vs actual Crime values
plot(data$Crime, tree1pred, xlab="Actual Values", ylab="Predicted values")
abline(0,1)


## -------------------------------------------------------------------------------------------------------------------
plot(data$Crime, scale(tree1pred - data$Crime),  xlab="Actual values", ylab="Standardized Residuals")
abline(0,0)


## -------------------------------------------------------------------------------------------------------------------
#Calculate SSres
tree1_ssr <- sum((tree1pred-data$Crime)^2)
tree1_ssr
#SSTot
tree1_SStot <- sum((data$Crime - mean(data$Crime))^2)
tree1_SStot
#R2
tree1_r2 <- 1 - tree1_ssr/tree1_SStot
tree1_r2


## -------------------------------------------------------------------------------------------------------------------
#sst for comparison
tree1_SStot
#values for each node of tree without CV
prune.tree(tree1)$dev
#and for the CV model
tree1_cv$dev



## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
#predictions
tree2pred <- predict(tree2)

#plot predicted vs actual Crime values
plot(data$Crime, tree2pred, xlab="Actual Values", ylab="Predicted values")
abline(0,1)


## -------------------------------------------------------------------------------------------------------------------
plot(data$Crime, scale(tree2pred - data$Crime),  xlab="Actual values", ylab="Standardized Residuals")
abline(0,0)


## -------------------------------------------------------------------------------------------------------------------
#Calculate SSres
tree2_ssr <- sum((tree2pred-data$Crime)^2)
tree2_ssr
#SSTot
tree2_SStot <- sum((data$Crime - mean(data$Crime))^2)
tree2_SStot
#R2
tree2_r2 <- 1 - tree2_ssr/tree2_SStot
tree2_r2


## -------------------------------------------------------------------------------------------------------------------
#sst for comparison
tree2_SStot
#values for each node of tree without CV
prune.tree(tree2)$dev
#and for the CV model
tree2_cv$dev


## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
#predictions
tree1pruned_pred <- predict(tree1_pruned)

#plot predicted vs actual Crime values
plot(data$Crime, tree1pruned_pred, xlab="Actual Values", ylab="Predicted values")
abline(0,1)

#residuals plot
plot(data$Crime, scale(tree1pruned_pred - data$Crime),  xlab="Actual values", ylab="Standardized Residuals")
abline(0,0)

#Calculate SSres
tree1pruned_ssr <- sum((tree1pruned_pred-data$Crime)^2)
tree1pruned_ssr
#SSTot
tree1pruned_SStot <- sum((data$Crime - mean(data$Crime))^2)
tree1pruned_SStot
#R2
tree1pruned_r2 <- 1 - tree1pruned_ssr/tree1pruned_SStot
tree1pruned_r2

#sst for comparison
tree1pruned_SStot
#values for each node of tree without CV
prune.tree(tree1_pruned)$dev
#cross-validate:
tree1pruned_cv <- cv.tree(tree1_pruned)
tree1pruned_cv$dev


## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
#predictions
tree2pruned_pred <- predict(tree2_pruned)

#plot predicted vs actual Crime values
plot(data$Crime, tree2pruned_pred, xlab="Actual Values", ylab="Predicted values")
abline(0,1)

#residuals plot
plot(data$Crime, scale(tree2pruned_pred - data$Crime),  xlab="Actual values", ylab="Standardized Residuals")
abline(0,0)

#Calculate SSres
tree2pruned_ssr <- sum((tree2pruned_pred-data$Crime)^2)
tree2pruned_ssr
#SSTot
tree2pruned_SStot <- sum((data$Crime - mean(data$Crime))^2)
tree2pruned_SStot
#R2
tree2pruned_r2 <- 1 - tree2pruned_ssr/tree2pruned_SStot
tree2pruned_r2

#sst for comparison
tree2pruned_SStot
#values for each node of tree without CV
prune.tree(tree2_pruned)$dev
#cross-validate:
tree2pruned_cv <- cv.tree(tree2_pruned)
tree2pruned_cv$dev


## -------------------------------------------------------------------------------------------------------------------
#prune
final_tree <- prune.tree(tree1,best=2)
#plot
plot(final_tree)
text(final_tree)
title(main = "Pruned Regression Tree")


## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
#predictions
tree_pred <- predict(final_tree)

#Calculate SSres
tree_ssr <- sum((tree_pred-data$Crime)^2)

#SSTot
tree_SStot <- sum((data$Crime - mean(data$Crime))^2)

#R2
tree_r2 <- 1 - tree_ssr/tree_SStot
tree_r2

#sst for comparison
tree_SStot
#values for each node of tree without CV
prune.tree(final_tree)$dev
#cross-validate:
tree_cv <- cv.tree(final_tree)
tree_cv$dev


## -------------------------------------------------------------------------------------------------------------------
final_tree$frame


## -------------------------------------------------------------------------------------------------------------------
#get the data for leaf 1
d1 <- data[which(final_tree$where==2),]
head(d1)


## -------------------------------------------------------------------------------------------------------------------
#p values
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(d1[,-16])
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(mtcars)

#corrplot
corrplot(cor(d1[,-16]), 
         method='pie', 
         type="upper", 
         #order="hclust",
         p.mat = p.mat, 
         sig.level = 0.1,
         insig = "blank")


## -------------------------------------------------------------------------------------------------------------------
#l1m1 - leaf 1 model 1
l1m1 <- lm(Crime~., data=d1)
summary(l1m1)


## -------------------------------------------------------------------------------------------------------------------
l1m2 <- lm(Crime~Ed+Pop+Prob+Time, data=d1)
summary(l1m2)


## -------------------------------------------------------------------------------------------------------------------
l1m3 <- lm(Crime~Pop+Time, data=d1)
summary(l1m3)


## -------------------------------------------------------------------------------------------------------------------
l1m4 <- lm(Crime~Pop, data=d1)
summary(l1m4)


## -------------------------------------------------------------------------------------------------------------------
l1m4cv <- cv.lm(d1, l1m4, m=nrow(d1), plotit=F, printit=F)
#R2
1 - attr(l1m4cv,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)


## -------------------------------------------------------------------------------------------------------------------
l1pca <- prcomp(~., data=d1[,-16], scale=T)
screeplot(l1pca)


## -------------------------------------------------------------------------------------------------------------------
pca1m1 <- lm(Crime~., data=as.data.frame(cbind(Crime=d1[,16], l1pca$x[,1:4])))
summary(pca1m1)


## -------------------------------------------------------------------------------------------------------------------
pca1m2 <- lm(Crime~., data=as.data.frame(cbind(Crime=d1[,16], l1pca$x[,2])))
summary(pca1m2,2)


## -------------------------------------------------------------------------------------------------------------------
pca1cv <- cv.lm(as.data.frame(cbind(Crime=d1[,16], l1pca$x[,2])),pca1m2,m=nrow(d1), plotit=F, printit = F)

#calculate R2

1 - attr(pca1cv,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)



## -------------------------------------------------------------------------------------------------------------------
#getting rid of 'e'values in output
options("scipen"=100, "digits"=4)
#getting coefs
int_sc <- summary(pca1m2)$coefficients[1]
coefs1_sc <- summary(pca1m2)$coefficients[2]
a_sc <- l1pca$rotation[,2]
#unscaling
unsc_coefs <- a_sc/l1pca$scale
unsc_coefs
unsc_int <-int_sc - sum(a_sc*l1pca$center/l1pca$scale)
unsc_int


## -------------------------------------------------------------------------------------------------------------------
#get the data for leaf 2
d2 <- data[which(final_tree$where==3),]
head(d2)


## -------------------------------------------------------------------------------------------------------------------
#p values
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(d2[,-16])
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(mtcars)

#corrplot
corrplot(cor(d2[,-16]), 
         method='pie', 
         type="upper", 
         #order="hclust",
         p.mat = p.mat, 
         sig.level = 0.1,
         insig = "blank")


## -------------------------------------------------------------------------------------------------------------------
#l1m1 - leaf 1 model 1
l2m1 <- lm(Crime~., data=d2)
summary(l2m1)


## -------------------------------------------------------------------------------------------------------------------
l2m2 <- lm(Crime~Ineq+Po1, data=d2)
summary(l2m2)


## -------------------------------------------------------------------------------------------------------------------
l2m2cv <- cv.lm(d2, l2m2, m=nrow(d2), plotit=F, printit=F)
#R2
1 - attr(l2m2cv,"ms")*nrow(d2)/sum((d2$Crime - mean(d2$Crime))^2)


## -------------------------------------------------------------------------------------------------------------------
l2pca <- prcomp(~., data=d2[,-16], scale=T)
screeplot(l2pca)


## -------------------------------------------------------------------------------------------------------------------
pca2m1 <- lm(Crime~., data=as.data.frame(cbind(Crime=d2[,16], l2pca$x[,1:5])))
summary(pca2m1)


## -------------------------------------------------------------------------------------------------------------------
pca2m2 <- lm(Crime~., data=as.data.frame(cbind(Crime=d2[,16], l2pca$x[,5])))
summary(pca2m2,2)


## -------------------------------------------------------------------------------------------------------------------
pca2cv <- cv.lm(as.data.frame(cbind(Crime=d2[,16], l2pca$x[,5])),pca2m2,m=nrow(d2), plotit=F, printit = F)

#calculate R2

1 - attr(pca2cv,"ms")*nrow(d2)/sum((d2$Crime - mean(d2$Crime))^2)



## -------------------------------------------------------------------------------------------------------------------
#getting rid of 'e'values in output
options("scipen"=100, "digits"=4)
#getting coefs
int_sc <- summary(pca2m2)$coefficients[1]
coefs1_sc <- summary(pca2m2)$coefficients[2]
a_sc <- l2pca$rotation[,5] #fifth PC
#unscaling
unsc_coefs <- a_sc/l2pca$scale
unsc_coefs
unsc_int <-int_sc - sum(a_sc*l2pca$center/l2pca$scale)
unsc_int


## -------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2)) 
plot(tree1)
text(tree1)
title(main = "Unpruned Regression Tree (1)")
plot(tree2)
text(tree2)
title(main = "Unpruned Regression Tree (2)")


## -------------------------------------------------------------------------------------------------------------------
plot(final_tree)
text(final_tree)
title(main = "Pruned Regression Tree")


## -------------------------------------------------------------------------------------------------------------------
head(data)


## -------------------------------------------------------------------------------------------------------------------
# Since So is a binary variable, set it as factor for better model building
data <- transform(data, So=as.factor(data[,2]))
summary(data)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
ctrl <- trainControl(method='LOOCV')

#RMSE quality metric since its a regression, not classification
metric <- 'RMSE'

#p/3 for regression models
mtry <- (ncol(data)/3)

tgrd <- expand.grid(.mtry=mtry)

rf_basic <- train(Crime~.,
                    data=data,
                    method='rf',
                    metric=metric,
                    tuneGrid=tgrd,
                    trControl=ctrl,
                    #verbose to get more text description on the model
                    verbose=T)

print(rf_basic)

rf_basic$finalModel

varImp(rf_basic)


## -------------------------------------------------------------------------------------------------------------------

#Residual sum of squares:
yhat_rf_basic <- rf_basic$pred[,1]
ssres <- sum((yhat_rf_basic-data$Crime)^2)

#Total sum of squares:
sstot <- sum((data$Crime - mean(data$Crime))^2)

#Plot predicted vs actual:
plot(data$Crime, yhat_rf_basic, xlab='Actual Values', ylab='Predicted Values', main='Predicted vs Actual Crime Values, rf_basic')
abline(0,1)

#Plot residuals
plot(data$Crime, scale(yhat_rf_basic - data$Crime), xlab='Actual Values', ylab='Standardized Residuals', main='Residuals, rf_basic')
abline(0,0)

#Plot Variable improtance
vip::vip(rf_basic, num_features = 15)


## -------------------------------------------------------------------------------------------------------------------
set.seed(11)
control <- trainControl(method='LOOCV', 
                        search='random')

#grow 500 trees
ntree <- 500

rf_randsearch <- train(Crime ~ .,
                   data = data,
                   method = 'rf',
                   metric = metric,
                   tuneLength  = 15, 
                   trControl = control,
                   verbose=T)
print(rf_randsearch)

rf_randsearch$finalModel

varImp(rf_randsearch)



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#Residual sum of squares:
yhat_rf_randsearch <- predict(rf_randsearch)
ssres <- sum((yhat_rf_randsearch-data$Crime)^2)

#plot the model
plot(rf_randsearch)

#Plot predicted vs actual:
plot(data$Crime, yhat_rf_randsearch, xlab='Actual Values', ylab='Predicted Values', main='Predicted vs Actual Crime Values, rf_randsearch')
abline(0,1)

#Plot residuals
plot(data$Crime, scale(yhat_rf_randsearch - data$Crime), xlab='Actual Values', ylab='Standardized Residuals', main='Residuals, rf_randsearch')
abline(0,0)

#Plot Variable improtance
vip::vip(rf_randsearch, num_features = 15)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
control <- trainControl(method='LOOCV', 
                        search='grid')

tunegrid <- expand.grid(.mtry=(1:15))

rf_grid <- train(Crime ~ .,
                 data = data,
                 method = 'rf',
                 metric = metric,
                 tuneGrid = tunegrid,
                 verbose=T)
print(rf_grid)

rf_grid$finalModel

varImp(rf_grid)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#Residual sum of squares:
yhat_rf_grid <- predict(rf_grid)
ssres <- sum((yhat_rf_grid-data$Crime)^2)

#plot the model
plot(rf_grid)

#Plot predicted vs actual:
plot(data$Crime, yhat_rf_grid, xlab='Actual Values', ylab='Predicted Values', main='Predicted vs Actual Crime Values, rf_randsearch')
abline(0,1)

#Plot residuals
plot(data$Crime, scale(yhat_rf_grid - data$Crime), xlab='Actual Values', ylab='Standardized Residuals', main='Residuals, rf_randsearch')
abline(0,0)

#Plot Variable improtance
vip::vip(rf_grid, num_features = 15)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
ctrl <- trainControl(method='LOOCV')

bag <- train(
  Crime~.,
  data=data,
  method='treebag',
  trControl=ctrl,
  metric='RMSE', importance=T
)
print(bag)
bag$finalModel
varImp(bag)



## -------------------------------------------------------------------------------------------------------------------
#Residual sum of squares:
yhat_bag <- bag$pred[,1]
ssres <- sum((yhat_bag-data$Crime)^2)


#Plot predicted vs actual:
plot(data$Crime, yhat_bag, xlab='Actual Values', ylab='Predicted Values', main='Predicted vs Actual Crime Values, rf_randsearch')
abline(0,1)

#Plot residuals
plot(data$Crime, scale(yhat_bag - data$Crime), xlab='Actual Values', ylab='Standardized Residuals', main='Residuals, rf_randsearch')
abline(0,0)

#Plot Variable improtance
vip::vip(bag, num_features = 15)


## -------------------------------------------------------------------------------------------------------------------
rf_grid$results[4,]


## -------------------------------------------------------------------------------------------------------------------
#split based on Po1
leaves<-split(data, data$Po1<7.65)
#df for each leaf
#leaf 1 - TRUE vals from split
df1<- as.data.frame(leaves[2])
#leaf 2 - FALSE vals from split
df2<- as.data.frame(leaves[1])
  

#yhat from the model for each leaf
#leaf 1 prediction
yhat1<-yhat_rf_grid[data$Po1<7.65]
#leaf 2 prediction
yhat2<-yhat_rf_grid[data$Po1>=7.65]

#ssres and sstot for each leaf
#leaf 1
ssres1 <- sum((yhat1-df1[,16])^2)
sstot1 <- sum((df1[,16]-mean(df1[,16]))^2)
#leaf 2
ssres2 <- sum((yhat2-df2[,16])^2)
sstot2 <- sum((df2[,16]-mean(df2[,16]))^2)

#R2 for each leaf
#leaf 1
r2_leaf1 <- 1 - ssres1/sstot1
r2_leaf1
#leaf 2
r2_leaf2 <- 1 - ssres2/sstot2
r2_leaf2



## -------------------------------------------------------------------------------------------------------------------
data <- read.table("germancredit.txt", 
                   header = FALSE, 
                   stringsAsFactors = FALSE,
                   sep = "", 
                   dec = ".")
head(data)


## -------------------------------------------------------------------------------------------------------------------
#1 and 2 in last column to 0 and 1
data$V21[data$V21==1] <- 0
data$V21[data$V21==2] <- 1

data <- transform(data,
                  V1=as.factor(data$V1),
                  V2=as.integer(data$V2),
                  V3=as.factor(data$V3),
                  V4=as.factor(data$V4),
                  V5=as.integer(data$V5),
                  V6=as.factor(data$V6),
                  V7=as.factor(data$V7),
                  V8=as.integer(data$V8),
                  V9=as.factor(data$V9),
                  V10=as.factor(data$V10),
                  V11=as.integer(data$V11),
                  V12=as.factor(data$V12),
                  V13=as.integer(data$V13),
                  V14=as.factor(data$V14),
                  V15=as.factor(data$V15),
                  V16=as.integer(data$V16),
                  V17=as.factor(data$V17),
                  V18=as.integer(data$V18),
                  V19=as.factor(data$V19),
                  V20=as.factor(data$V20),
                  #response is a factor! 0-good risk 1-bad
                  V21=as.factor(data$V21))
                 
colnames(data) <- c('Checking_Account', 'Duration_months', 'Credit_History', 'Credit_Purpose', 'Credit_Amount', 'Savings', 'Employment_Present','Installment_to_Income', 'Status_and_Sex', 'Debtors_Guarantors', 'Residence_Since', 'Property', 'Age', 'Other_Installments', 'Housing', 'Existing_Credits_at_Bank', 'Job', 'People_Liable', 'Telephone', 'Foreign_Worker', 'Credit_Risk')
summary(data)

dim(data)


## -------------------------------------------------------------------------------------------------------------------
is.null(data)


## -------------------------------------------------------------------------------------------------------------------
p1=ggplot(data, aes(x=Checking_Account))+geom_bar()+theme_stata()
p2=ggplot(data, aes(x=Credit_History))+geom_bar()+theme_stata()
p3=ggplot(data, aes(x=Credit_Purpose))+geom_bar()+theme_stata()
p4=ggplot(data, aes(x=Savings))+geom_bar()+theme_stata()
p5=ggplot(data, aes(x=Employment_Present))+geom_bar()+theme_stata()
p6=ggplot(data, aes(x=Status_and_Sex))+geom_bar()+theme_stata()
p7=ggplot(data, aes(x=Debtors_Guarantors))+geom_bar()+theme_stata()
p8=ggplot(data, aes(x=Property))+geom_bar()+theme_stata()
p9=ggplot(data, aes(x=Other_Installments))+geom_bar()+theme_stata()
p10=ggplot(data, aes(x=Housing))+geom_bar()+theme_stata()
p11=ggplot(data, aes(x=Job))+geom_bar()+theme_stata()
p12=ggplot(data, aes(x=Telephone))+geom_bar()+theme_stata()
p13=ggplot(data, aes(x=Foreign_Worker))+geom_bar()+theme_stata()
p14=ggplot(data, aes(x=Credit_Risk))+geom_bar()+theme_stata()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)


## -------------------------------------------------------------------------------------------------------------------
t<-table(data$Credit_Risk)
t
round(t[1]/(t[1]+t[2])*100,2)
round(t[2]/(t[1]+t[2])*100,2)


## -------------------------------------------------------------------------------------------------------------------
melted <- melt(data)[,15:16]
hist_plots <- ggplot(melted,
                    aes(x=value))+
              geom_histogram(aes(y=..density..), colour="black", fill="white")+
              geom_density(alpha=.3, color="skyblue", fill="skyblue")+
              facet_wrap(~variable, scale="free")+
              theme_fivethirtyeight()
hist_plots


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

#split
data_split <- initial_split(data, prop = .7, strata = "Credit_Risk")
data_train <- training(data_split)
data_test  <- testing(data_split)

#check proportions
t_train <- table(data_train$Credit_Risk)
round(t_train[1]/(t_train[1]+t_train[2])*100,2)
round(t_train[2]/(t_train[1]+t_train[2])*100,2)

t_test <- table(data_test$Credit_Risk)
round(t_test[1]/(t_test[1]+t_test[2])*100,2)
round(t_test[2]/(t_test[1]+t_test[2])*100,2)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

logreg1 <- train(Credit_Risk~.,
                   data=data_train,
                   method='glm',
                 #use logit since we have 0/1 as response - natural log link
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(logreg1)
logreg1$results



## -------------------------------------------------------------------------------------------------------------------
par(mar = c(3,8,3,3))
vif1<-car::vif(logreg1$finalModel)
as.table(vif1)

barplot(vif1,
        main="VIF Values (model with all parameters)",
        horiz=TRUE,
        col="lightblue",
        las=2,
        cex.names=0.3,
        )
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

logreg2 <- train(Credit_Risk~. -Job,
                   data=data_train,
                   method='glm',
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(logreg2)
logreg2$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

logreg3 <- train(Credit_Risk~. - Residence_Since - Housing - Existing_Credits_at_Bank - Job,
                   data=data_train,
                   method='glm',
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(logreg3)
logreg3$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

logreg4 <- train(Credit_Risk~. - Residence_Since - Housing - Existing_Credits_at_Bank - Job - Property,
                   data=data_train,
                   method='glm',
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(logreg4)
logreg4$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

logreg5 <- train(Credit_Risk~. - Residence_Since - Housing - Existing_Credits_at_Bank - Job - Property - Status_and_Sex,
                   data=data_train,
                   method='glm',
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(logreg5)
logreg5$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

logreg6 <- train(Credit_Risk~. - Residence_Since - Housing - Existing_Credits_at_Bank - Job - Property - Status_and_Sex - People_Liable,
                   data=data_train,
                   method='glm',
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(logreg6)
logreg6$results


## -------------------------------------------------------------------------------------------------------------------
#Checking Account A13, A14
data_train$Checking_AccountA13[data_train$Checking_Account == 'A13'] <-1
data_train$Checking_AccountA13[data_train$Checking_Account != 'A13'] <-0
data_train$Checking_AccountA14[data_train$Checking_Account == 'A14'] <-1
data_train$Checking_AccountA14[data_train$Checking_Account != 'A14'] <-0

#Credit History A32, A33, A34
data_train$Credit_HistoryA32[data_train$Credit_History == 'A32'] <-1
data_train$Credit_HistoryA32[data_train$Credit_History != 'A32'] <-0
data_train$Credit_HistoryA33[data_train$Credit_History == 'A33'] <-1
data_train$Credit_HistoryA33[data_train$Credit_History != 'A33'] <-0
data_train$Credit_HistoryA34[data_train$Credit_History == 'A34'] <-1
data_train$Credit_HistoryA34[data_train$Credit_History != 'A34'] <-0

#Credit Purpose A41, A42, A43, A49
data_train$Credit_PurposeA41[data_train$Credit_Purpose == 'A41'] <-1
data_train$Credit_PurposeA41[data_train$Credit_Purpose != 'A41'] <-0
data_train$Credit_PurposeA42[data_train$Credit_Purpose == 'A42'] <-1
data_train$Credit_PurposeA42[data_train$Credit_Purpose != 'A42'] <-0
data_train$Credit_PurposeA43[data_train$Credit_Purpose == 'A43'] <-1
data_train$Credit_PurposeA43[data_train$Credit_Purpose != 'A43'] <-0
data_train$Credit_PurposeA49[data_train$Credit_Purpose == 'A49'] <-1
data_train$Credit_PurposeA49[data_train$Credit_Purpose != 'A49'] <-0

#Savings A64, A65
data_train$SavingsA64[data_train$Savings == 'A64'] <-1
data_train$SavingsA64[data_train$Savings != 'A64'] <-0
data_train$SavingsA65[data_train$Savings == 'A65'] <-1
data_train$SavingsA65[data_train$Savings != 'A65'] <-0

#Employment A74
data_train$Employment_PresentA74[data_train$Employment_Present == 'A74'] <-1
data_train$Employment_PresentA74[data_train$Employment_Present != 'A74'] <-0

#Debtors_Guarantors A103
data_train$Debtors_GuarantorsA103[data_train$Debtors_Guarantors == 'A103'] <-1
data_train$Debtors_GuarantorsA103[data_train$Debtors_Guarantors != 'A103'] <-0

#Other Installments A143
data_train$Other_InstallmentsA143[data_train$Other_Installments == 'A143'] <-1
data_train$Other_InstallmentsA143[data_train$Other_Installments != 'A143'] <-0

#Telephone A192
data_train$TelephoneA192[data_train$Telephone == 'A192'] <-1
data_train$TelephoneA192[data_train$Telephone != 'A192'] <-0

#Foreign_Worker A202
data_train$Foreign_WorkerA202[data_train$Foreign_Worker == 'A202'] <-1
data_train$Foreign_WorkerA202[data_train$Foreign_Worker != 'A202'] <-0




## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

final <- train(Credit_Risk~ Checking_AccountA13 + Checking_AccountA14 + Duration_months + Credit_HistoryA32 + Credit_HistoryA33 + Credit_HistoryA34 +  Credit_PurposeA41 + Credit_PurposeA42 + Credit_PurposeA43 +Credit_PurposeA49 + Credit_Amount + SavingsA64 + SavingsA65 + Employment_PresentA74 + Installment_to_Income + Debtors_GuarantorsA103 + Age + Other_InstallmentsA143 + TelephoneA192 + Foreign_WorkerA202,
                   data=data_train,
                   method='glm',
                   family=binomial(link = "logit"),
                   trControl = trainControl(method='repeatedcv', number=10, repeats=10))
                  
summary(final)
final$results



## -------------------------------------------------------------------------------------------------------------------
par(mar = c(3,8,3,3))
vif<-car::vif(final$finalModel)
as.table(vif)

barplot(vif,
        main="VIF Values (model with significant parameters)",
        horiz=TRUE,
        col="lightblue",
        las=2,
        cex.names=0.7,
        )
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
#Checking Account A13, A14
data_test$Checking_AccountA13[data_test$Checking_Account == 'A13'] <-1
data_test$Checking_AccountA13[data_test$Checking_Account != 'A13'] <-0
data_test$Checking_AccountA14[data_test$Checking_Account == 'A14'] <-1
data_test$Checking_AccountA14[data_test$Checking_Account != 'A14'] <-0

#Credit History A32, A33, A34
data_test$Credit_HistoryA32[data_test$Credit_History == 'A32'] <-1
data_test$Credit_HistoryA32[data_test$Credit_History != 'A32'] <-0
data_test$Credit_HistoryA33[data_test$Credit_History == 'A33'] <-1
data_test$Credit_HistoryA33[data_test$Credit_History != 'A33'] <-0
data_test$Credit_HistoryA34[data_test$Credit_History == 'A34'] <-1
data_test$Credit_HistoryA34[data_test$Credit_History != 'A34'] <-0

#Credit Purpose A41, A42, A43, A49
data_test$Credit_PurposeA41[data_test$Credit_Purpose == 'A41'] <-1
data_test$Credit_PurposeA41[data_test$Credit_Purpose != 'A41'] <-0
data_test$Credit_PurposeA42[data_test$Credit_Purpose == 'A42'] <-1
data_test$Credit_PurposeA42[data_test$Credit_Purpose != 'A42'] <-0
data_test$Credit_PurposeA43[data_test$Credit_Purpose == 'A43'] <-1
data_test$Credit_PurposeA43[data_test$Credit_Purpose != 'A43'] <-0
data_test$Credit_PurposeA49[data_test$Credit_Purpose == 'A49'] <-1
data_test$Credit_PurposeA49[data_test$Credit_Purpose != 'A49'] <-0

#Savings A64, A65
data_test$SavingsA64[data_test$Savings == 'A64'] <-1
data_test$SavingsA64[data_test$Savings != 'A64'] <-0
data_test$SavingsA65[data_test$Savings == 'A65'] <-1
data_test$SavingsA65[data_test$Savings != 'A65'] <-0

#Employment A74
data_test$Employment_PresentA74[data_test$Employment_Present == 'A74'] <-1
data_test$Employment_PresentA74[data_test$Employment_Present != 'A74'] <-0

#Debtors_Guarantors A103
data_test$Debtors_GuarantorsA103[data_test$Debtors_Guarantors == 'A103'] <-1
data_test$Debtors_GuarantorsA103[data_test$Debtors_Guarantors != 'A103'] <-0

#Other Installments A143
data_test$Other_InstallmentsA143[data_test$Other_Installments == 'A143'] <-1
data_test$Other_InstallmentsA143[data_test$Other_Installments != 'A143'] <-0

#Telephone A192
data_test$TelephoneA192[data_test$Telephone == 'A192'] <-1
data_test$TelephoneA192[data_test$Telephone != 'A192'] <-0

#Foreign_Worker A202
data_test$Foreign_WorkerA202[data_test$Foreign_Worker == 'A202'] <-1
data_test$Foreign_WorkerA202[data_test$Foreign_Worker != 'A202'] <-0


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)

prediction <- predict(final, newdata=data_test, type='prob')


prediction_round <- factor(as.integer(prediction[,2]>0.5))
prediction_round



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
cm <- confusionMatrix(
  data = prediction_round, 
  reference = relevel(data_test$Credit_Risk, ref = "0")
)
cm



## -------------------------------------------------------------------------------------------------------------------
#misclassification error
round((cm$table[1,2]+cm$table[2,1])/sum(cm$table)*100,2)


## -------------------------------------------------------------------------------------------------------------------
library(ROCR)

# predicted probabilities

prob_final <- predict(final, data_test, type = "prob")[,2]

# AUC metrics
perffinal <- prediction(prob_final, data_test$Credit_Risk) %>%
  performance(measure = "tpr", x.measure = "fpr")

# ROC curve
plot(perffinal, col = "blue", main='ROC TP/FP', lwd=2)
abline(0,1)




## -------------------------------------------------------------------------------------------------------------------
library(pROC)

roccurve <- roc(response=data_test$Credit_Risk,predictor=as.integer(prediction[,2]>0.5),
                levels = c(0, 1), 
                direction = "<",
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
                
sens.ci <- ci.se(roccurve)
plot(sens.ci, type="shape", col="lightblue")                
plot(sens.ci, type="bars")               



## -------------------------------------------------------------------------------------------------------------------
auc <- list()
acc <- list()
thresh <- seq(0,1, by=0.01)
for (i in thresh){
  acci=confusionMatrix(data = factor(as.integer(prediction[,2]>i)), 
                        reference = relevel(data_test$Credit_Risk, ref = "0"))$overall[1]
  auci=roc(response=data_test$Credit_Risk,predictor=as.integer(prediction[,2]>i),
           levels = c(0, 1),
           direction = "<")$auc  
  acc<-append(acc,as.numeric(acci))
  auc<-append(auc,as.numeric(auci))
}

thresholds<-data.frame(unlist(thresh), round(unlist(auc),2), round(unlist(acc),2))
colnames(thresholds)<-c('Threshold', 'AUC', 'Accuracy')
head(thresholds)


## -------------------------------------------------------------------------------------------------------------------
colors<-c('AUC'='steelblue', 'Accuracy'='darkgreen')
ggplot(thresholds, aes(x=Threshold))+
  geom_line(aes(y=AUC, color='AUC'), size=1.2)+
  geom_line(aes(y=Accuracy, color='Accuracy'), size=1.2)+
  scale_color_manual(values=colors)


## -------------------------------------------------------------------------------------------------------------------
thresholds[thresholds$AUC==thresholds$Accuracy,]


## -------------------------------------------------------------------------------------------------------------------
thresholds[thresholds$AUC==max(thresholds$AUC),]


## -------------------------------------------------------------------------------------------------------------------
thresholds[thresholds$Accuracy==max(thresholds$Accuracy),]



## -------------------------------------------------------------------------------------------------------------------
losses<-list()
thresh <- seq(0,1, by=0.01)
for (i in thresh){
  confm = confusionMatrix(data = factor(as.integer(prediction[,2]>i)), 
                        reference = relevel(data_test$Credit_Risk, ref = "0"))
  losses <- append(losses, confm$table[1,2]*5+confm$table[2,1])
}

losses <- data.frame(unlist(thresh), unlist(losses))
colnames(losses) <- c('Threshold', 'Loss')
head(losses)


## -------------------------------------------------------------------------------------------------------------------
ggplot(losses, aes(x=Threshold, y=Loss))+
  geom_point(color='skyblue', size=2)+
  geom_point(data= losses[losses$Loss==min(losses$Loss),], color='darkgreen', size=3)+
  geom_point(data= losses[losses$Loss==max(losses$Loss),], color='red', size=3)+
  labs(title='Loss - Threshold')+
  theme_stata()


## -------------------------------------------------------------------------------------------------------------------
losses[losses$Loss==min(losses$Loss),]


## -------------------------------------------------------------------------------------------------------------------
thresholds[thresholds$Threshold==0.17,]

