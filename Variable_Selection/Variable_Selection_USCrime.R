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
library(rsample)
library(glmnet)


## -------------------------------------------------------------------------------------------------------------------

data <- read.table("uscrime.txt", 
                   header = TRUE, 
                   stringsAsFactors = FALSE,
                   sep = "", 
                   dec = ".")
head(data)



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
#scale except So (column 2) and Crime (response)
scaled <- as.data.frame(scale(data[,-c(2,16)]))
#add So and response back
scaled <- cbind(So=data[,2], scaled, Crime=data[,16])
head(scaled)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
stepwise <- train(Crime~.,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lmStepAIC",
                     direction = "backward", trace=F)
summary(stepwise)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

m1 <- train(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
m1$results
summary(m1)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

m2 <- train(Crime ~ M + Ed + Po1 + U1 + U2 + Ineq + Prob,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
m2$results
summary(m2)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

m3 <- train(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
m3$results
summary(m3)



## -------------------------------------------------------------------------------------------------------------------
m3$finalModel


## -------------------------------------------------------------------------------------------------------------------
pca <- prcomp(data[,-16], 
              scale. = TRUE 
              )
summary(pca)


## -------------------------------------------------------------------------------------------------------------------
explained_var <- pca$sdev^2/sum(pca$sdev^2)
explained_var_round<-round(explained_var,4)
#plot explained variance by each PC:

qplot(c(1:15), explained_var_round,) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Proportion of Variance Explained") +
  ggtitle("Proportion of Variance explained by each Principal Component") +
  ylim(0, 1) +
  geom_text(aes(label=explained_var_round*100),vjust=-2)+
  theme_stata()

cumsum_var <- cumsum(explained_var)
cumsum_var_round <- round(cumsum_var,4)
#plot cumulative explained variance by each PC:

qplot(c(1:15), cumsum_var_round,) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Cumulative Proportion of Variance Explained") +
  ggtitle("Cumulative Proportion of Variance explained by each Principal Component") +
  ylim(0, 1) +
  geom_text(aes(label=cumsum_var_round*100),vjust=2)+
  theme_stata()

screeplot(pca, npcs=15)


## -------------------------------------------------------------------------------------------------------------------
pca_data <- as.data.frame(cbind(pca$x, Crime=data[,16]))
head(pca_data)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
stepwise_pca <- train(Crime~.,
                     data=pca_data,
                     trControl=data_ctrl,
                     method="lmStepAIC",
                     direction = "backward", trace=F)
summary(stepwise_pca)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

m1_pca <- train(Crime ~ PC1 + PC2 + PC4 + PC5 + PC6 + PC7 + PC12 + PC14 + PC15,
                     data=pca_data,
                     trControl=data_ctrl,
                     method="lm")
summary(m1_pca)
m1_pca$results



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")
#removing PC6 and PC15
m2_pca <- train(Crime ~ PC1 + PC2 + PC4 + PC5 + PC7 + PC12 + PC14 ,
                     data=pca_data,
                     trControl=data_ctrl,
                     method="lm")
summary(m2_pca)
m2_pca$results


## -------------------------------------------------------------------------------------------------------------------
y <- data.matrix(scaled$Crime)
head(y)
x <- data.matrix(scaled[,-16])
head(x)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
lasso <- cv.glmnet(x, y, #matrices
                   alpha=1,
                   nfolds=5, #5-fold CV
                   type.measure='mse', #use MSE to find best lambda
                   family='gaussian') #in r refers to normal distribution 
                    
best.lambda <- lasso$lambda.min
best.lambda

plot(lasso)



## -------------------------------------------------------------------------------------------------------------------
coefs_minlambda <- coef(lasso, s=best.lambda)
coefs_minlambda


## -------------------------------------------------------------------------------------------------------------------
lambda.1se <-lasso$lambda.1se
lambda.1se
coefs_1selambda <- coef(lasso, s=lambda.1se)
coefs_1selambda


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

lasso1 <- train(Crime ~ So + M + Ed + Po1 + LF + M.F + NW + U2 + Ineq + Prob,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
summary(lasso1)
lasso1$results


## -------------------------------------------------------------------------------------------------------------------
summary(m3)
m3$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

lasso2 <- train(Crime ~ M + Ed + Po1 + M.F + Ineq + Prob,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
summary(lasso2)
lasso2$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

lasso3 <- train(Crime ~ M + Ed + Po1 + Ineq + Prob,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
summary(lasso3)
lasso3$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#matrices
y.pca <- data.matrix(pca_data$Crime)
x.pca <- data.matrix(pca_data[,-16])

#LASSO
lasso.pca <- cv.glmnet(x.pca, y.pca, #matrices
                   alpha=1,
                   nfolds=5, #5-fold CV
                   type.measure='mse', #use MSE to find best lambda
                   family='gaussian') #in r refers to normal distribution 
                    
best.lambda.pca <- lasso.pca$lambda.min
best.lambda.pca

plot(lasso.pca)


## -------------------------------------------------------------------------------------------------------------------
coef(lasso.pca, s=best.lambda.pca)


## -------------------------------------------------------------------------------------------------------------------
lambda.1se.pca <-lasso.pca$lambda.1se
lambda.1se.pca
coef(lasso.pca, s=lambda.1se.pca)



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

lassopca1 <- train(Crime ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC10 + PC12 + PC13 + PC14 + PC15,
                     data=pca_data,
                     trControl=data_ctrl,
                     method="lm")
summary(lassopca1)
lassopca1$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

lassopca2 <- train(Crime ~ PC1 + PC2 + PC4 + PC5 + PC7 + PC12 + PC14,
                     data=pca_data,
                     trControl=data_ctrl,
                     method="lm")
summary(lassopca2)
lassopca2$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

lassopca3 <- train(Crime ~ PC1 + PC2 + PC4 + PC5 + PC6 + PC7 + PC12 + PC14 + PC15,
                     data=pca_data,
                     trControl=data_ctrl,
                     method="lm")
summary(lassopca3)
lassopca3$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
r2list<-c()
alphalist <- seq(0,1,by=0.05)
for (i in alphalist){
  
  elasticnetcv = cv.glmnet(x, y, alpha=i,nfolds = 5,type.measure="mse",family="gaussian") 
  
  r2list = cbind(r2list,
                elasticnetcv$glmnet.fit$dev.ratio[which(elasticnetcv$glmnet.fit$lambda == elasticnetcv$lambda.min)])}

#merge results in a data frame
alpha_r2 <- data.table(alphalist, t(r2list))
colnames(alpha_r2) <- c('alpha', 'R2')
alpha_r2


## -------------------------------------------------------------------------------------------------------------------
alpha_r2[which.max(alpha_r2$R2)]


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#elastic net
elasticnetcv <- cv.glmnet(x, y, alpha=0.3, nfolds = 5,type.measure="mse",family="gaussian") 
#get coefs
coef(elasticnetcv, s=elasticnetcv$lambda.min)
coef(elasticnetcv, s=elasticnetcv$lambda.1se)


## -------------------------------------------------------------------------------------------------------------------
plot(elasticnetcv)


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

net1 <- train(Crime ~ .-Time,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
summary(net1)
net1$results


## -------------------------------------------------------------------------------------------------------------------
summary(m3)
m3$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#leave-one-out CV
data_ctrl <- trainControl(method = "LOOCV")

net2 <- train(Crime ~ .- Time - Pop - U1 - Wealth ,
                     data=scaled,
                     trControl=data_ctrl,
                     method="lm")
summary(net2)
net2$results


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
r2list<-c()
alphalist <- seq(0,1,by=0.05)
for (i in alphalist){
  
  elasticnetcvpca = cv.glmnet(x.pca, y.pca, alpha=i,nfolds = 5,type.measure="mse",family="gaussian") 
  
  r2list = cbind(r2list,
                elasticnetcvpca$glmnet.fit$dev.ratio[which(elasticnetcvpca$glmnet.fit$lambda == elasticnetcvpca$lambda.min)])}

#merge results in a data frame
alpha_r2_pca <- data.table(alphalist, t(r2list))
colnames(alpha_r2_pca) <- c('alpha', 'R2')
alpha_r2_pca


## -------------------------------------------------------------------------------------------------------------------
alpha_r2_pca[which.max(alpha_r2_pca$R2)]


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
#elastic net
elasticnetcvpca <- cv.glmnet(x.pca, y.pca, alpha=0.55, nfolds = 5,type.measure="mse",family="gaussian") 
#get coefs
coef(elasticnetcvpca, s=elasticnetcvpca$lambda.min)
coef(elasticnetcvpca, s=elasticnetcvpca$lambda.1se)


## -------------------------------------------------------------------------------------------------------------------
plot(elasticnetcvpca)


## -------------------------------------------------------------------------------------------------------------------
summary(lassopca1)
lassopca1$results


## -------------------------------------------------------------------------------------------------------------------
summary(lassopca2)
lassopca2$results


## -------------------------------------------------------------------------------------------------------------------
summary(lassopca3)
lassopca3$results

