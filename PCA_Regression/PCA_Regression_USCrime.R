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
corrplot(cor(data[,-16]), method='pie', type="upper", order="hclust")


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
p.mat <- cor.mtest(mtcars)
head(p.mat[, 1:5])


## -------------------------------------------------------------------------------------------------------------------
corrplot(cor(data[,-16]), 
         method='pie', 
         type="upper", 
         #order="hclust",
         p.mat = p.mat, 
         sig.level = 0.1,
         insig = "blank")


## -------------------------------------------------------------------------------------------------------------------
cm <- as.table(round(cor(data[,-16]),1))
cm


## -------------------------------------------------------------------------------------------------------------------

ggpairs(data[,-16], upper = list(continuous = wrap("cor", size = 2)))
#changed font size so that values would fit


## -------------------------------------------------------------------------------------------------------------------
pca <- prcomp(data[,-16], 
              scale. = TRUE 
              )
pca


## -------------------------------------------------------------------------------------------------------------------
summary(pca)


## -------------------------------------------------------------------------------------------------------------------
head(pca$x)


## -------------------------------------------------------------------------------------------------------------------
head(pca$rotation)


## -------------------------------------------------------------------------------------------------------------------
screeplot(pca, npcs=15)
#display all PCs


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


## -------------------------------------------------------------------------------------------------------------------
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



## -------------------------------------------------------------------------------------------------------------------
#attach Crime values to the first 4 PCs
pc4 <- cbind(pca$x[,1:4], Crime=data[,16])
#check the result
head(as.data.frame(pc4))


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
pc4_model <- train(Crime~.,
                     data=as.data.frame(pc4),
                     trControl=data_ctrl,
                     method="lm")
pc4_model
install.packages('rpart.plot')



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
test_data_scaled <- data.frame(predict(pca,test_data))
test_data_scaled


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
pc4_prediction <- predict(pc4_model,test_data_scaled)
ceiling(pc4_prediction)


## -------------------------------------------------------------------------------------------------------------------
#check all coefficients:
summary(pc4_model)$coefficients
#extract the intercept:
intercept_scaled4 <- summary(pc4_model)$coefficients[1]
intercept_scaled4
#extract the coefficients
coefs_scaled4 <- summary(pc4_model)$coefficients[2:5]
coefs_scaled4


## -------------------------------------------------------------------------------------------------------------------
#rotation component of PCA:
pca$rotation[,1:4]
#alphas:
a4 <- pca$rotation[,1:4] %*% coefs_scaled4
#transpose:
t(a4)


## -------------------------------------------------------------------------------------------------------------------
a_unscaled4 <- a4/pca$scale
a_unscaled4


## -------------------------------------------------------------------------------------------------------------------
intercept_unscaled4 <- intercept_scaled4 - sum(a4*pca$center/pca$scale)
intercept_unscaled4


## -------------------------------------------------------------------------------------------------------------------
estimated_vals4 <- as.matrix(data[,1:15]) %*% a_unscaled4 + intercept_unscaled4
estimated_vals4


## -------------------------------------------------------------------------------------------------------------------
#attach Crime values to the first 5 PCs
pc5 <- cbind(pca$x[,1:5], Crime=data[,16])
#check the result
head(as.data.frame(pc5))


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
pc5_model <- train(Crime~.,
                     data=as.data.frame(pc5),
                     trControl=data_ctrl,
                     method="lm")
pc5_model



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
pc5_prediction <- predict(pc5_model,test_data_scaled)
#round
ceiling(pc5_prediction)


## -------------------------------------------------------------------------------------------------------------------
#check all coefficients:
summary(pc5_model)$coefficients
#extract the intercept:
intercept_scaled5 <- summary(pc5_model)$coefficients[1]
intercept_scaled5
#extract the coefficients
coefs_scaled5 <- summary(pc5_model)$coefficients[2:6]
coefs_scaled5


## -------------------------------------------------------------------------------------------------------------------
#rotation component of PCA:
pca$rotation[,1:5]
#alphas:
a5 <- pca$rotation[,1:5] %*% coefs_scaled5
#transpose:
t(a5)


## -------------------------------------------------------------------------------------------------------------------
#getting rid of 'e'values in output
options("scipen"=100, "digits"=4)
#unscaling
a_unscaled5 <- a5/pca$scale
a_unscaled5


## -------------------------------------------------------------------------------------------------------------------
intercept_unscaled5 <- intercept_scaled5 - sum(a5*pca$center/pca$scale)
intercept_unscaled5


## -------------------------------------------------------------------------------------------------------------------
estimated_vals5 <- as.matrix(data[,1:15]) %*% a_unscaled5 + intercept_unscaled5
estimated_vals5


## -------------------------------------------------------------------------------------------------------------------
#attach Crime values to the first 5 PCs
pc6 <- cbind(pca$x[,1:6], Crime=data[,16])
#check the result
head(as.data.frame(pc6))


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)
pc6_model <- train(Crime~.,
                     data=as.data.frame(pc6),
                     trControl=data_ctrl,
                     method="lm")
pc6_model



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
pc6_prediction <- predict(pc6_model,test_data_scaled)
#round
ceiling(pc6_prediction)


## -------------------------------------------------------------------------------------------------------------------
#check all coefficients:
summary(pc6_model)$coefficients
#extract the intercept:
intercept_scaled6 <- summary(pc6_model)$coefficients[1]
intercept_scaled6
#extract the coefficients
coefs_scaled6 <- summary(pc6_model)$coefficients[2:7]
coefs_scaled6


## -------------------------------------------------------------------------------------------------------------------
#rotation component of PCA:
pca$rotation[,1:6]
#alphas:
a6 <- pca$rotation[,1:6] %*% coefs_scaled6
#transpose:
t(a6)


## -------------------------------------------------------------------------------------------------------------------
#getting rid of 'e'values in output
options("scipen"=100, "digits"=4)
#unscaling
a_unscaled6 <- a6/pca$scale
a_unscaled6


## -------------------------------------------------------------------------------------------------------------------
intercept_unscaled6 <- intercept_scaled6 - sum(a6*pca$center/pca$scale)
intercept_unscaled6


## -------------------------------------------------------------------------------------------------------------------
estimated_vals6 <- as.matrix(data[,1:15]) %*% a_unscaled6 + intercept_unscaled6
estimated_vals6


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)

r2 <- numeric(15)
rmse <- numeric(15)
mae <- numeric(15)
prediction <- numeric(15)
  
for (i in 1:15) {
  pcomps <- pca$x[,1:i]  #extract the first i PCs
  pc_i <- cbind(Crime=data[,16],pcomps)  #add crime variable to the set
  model <- train(Crime~.,
                     data=as.data.frame(pc_i),
                     trControl=data_ctrl,
                     method="lm")
  r2[i] <- unlist(model$results[3]) #extract R squared
  rmse[i] <- unlist(model$results[2]) #extract RMSE
  mae[i] <- unlist(model$results[4]) #extract RMSE
  prediction[i] <- ceiling(predict(model,test_data_scaled))
}
#compare results
models <- data.frame(PC=c(1:15),R2=r2, RMSE=rmse, MAE=mae, Prediction=prediction)
models


## -------------------------------------------------------------------------------------------------------------------
#r2
lab<-round(models$R2*100,1)
qplot(c(1:15), models$R2) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("R-squared") +
  ggtitle("R-squared for cross-validated models") +
  ylim(0, 1) +
  geom_text(aes(label=lab),vjust=-1)+
  theme_stata()
#RMSE
qplot(c(1:15), models$RMSE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Root Mean Square Error") +
  ggtitle("Root Mean Square Error for cross-validated models") +
  theme_stata()
#MAE
qplot(c(1:15), models$MAE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Mean Average Error") +
  ggtitle("Mean Average Error for cross-validated models") +
  theme_stata()
#Prediction
qplot(c(1:15), models$Prediction) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Crimes per 100.000 residents") +
  ggtitle("Predicted Crime values") +
  geom_text(aes(label=models$Prediction),vjust=-1)+
  theme_stata()



## -------------------------------------------------------------------------------------------------------------------
models[5,]


## -------------------------------------------------------------------------------------------------------------------
data1 <- data[,-2]
head(data1)


## -------------------------------------------------------------------------------------------------------------------
pca_1 <- prcomp(data1[,-15], 
              scale. = TRUE 
              )
pca_1


## -------------------------------------------------------------------------------------------------------------------
screeplot(pca_1)


## -------------------------------------------------------------------------------------------------------------------
explained_var1 <- pca_1$sdev^2/sum(pca_1$sdev^2)
explained_var_round1<-round(explained_var1,4)
#plot explained variance by each PC:

qplot(c(1:14), explained_var_round1,) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Proportion of Variance Explained") +
  ggtitle("Proportion of Variance explained by each Principal Component") +
  ylim(0, 1) +
  geom_text(aes(label=explained_var_round1*100),vjust=-2)+
  theme_stata()


## -------------------------------------------------------------------------------------------------------------------
cumsum_var1 <- cumsum(explained_var1)
cumsum_var_round1 <- round(cumsum_var1,4)
#plot cumulative explained variance by each PC:

qplot(c(1:14), cumsum_var_round1,) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Cumulative Proportion of Variance Explained") +
  ggtitle("Cumulative Proportion of Variance explained by each Principal Component") +
  ylim(0, 1) +
  geom_text(aes(label=cumsum_var_round1*100),vjust=2)+
  theme_stata()


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats=10)

r2_rem <- numeric(14)
rmse_rem <- numeric(14)
mae_rem <- numeric(14)
prediction_rem <- numeric(14)
  
for (i in 1:14) {
  pcomps <- pca_1$x[,1:i]  #extract the first i PCs
  pc_i <- cbind(Crime=data1[,15],pcomps)  #add crime variable to the set
  model <- train(Crime~.,
                     data=as.data.frame(pc_i),
                     trControl=data_ctrl,
                     method="lm")
  r2_rem[i] <- unlist(model$results[3]) #extract R squared
  rmse_rem[i] <- unlist(model$results[2]) #extract RMSE
  mae_rem[i] <- unlist(model$results[4]) #extract RMSE
  prediction_rem[i] <- ceiling(predict(model,test_data_scaled))
}
#compare results
models_rem <- data.frame(PC=c(1:14),R2=r2_rem, RMSE=rmse_rem, MAE=mae_rem, Prediction=prediction_rem)
models_rem


## -------------------------------------------------------------------------------------------------------------------
#r2
lab<-round(models_rem$R2*100,1)
qplot(c(1:14), models_rem$R2) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("R-squared") +
  ggtitle("R-squared for cross-validated models") +
  ylim(0, 1) +
  geom_text(aes(label=lab),vjust=-1)+
  theme_stata()
#RMSE
qplot(c(1:14), models_rem$RMSE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Root Mean Square Error") +
  ggtitle("Root Mean Square Error for cross-validated models") +
  theme_stata()
#MAE
qplot(c(1:14), models_rem$MAE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Mean Average Error") +
  ggtitle("Mean Average Error for cross-validated models") +
  theme_stata()
#Prediction
qplot(c(1:14), models_rem$Prediction) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Crimes per 100.000 residents") +
  ggtitle("Predicted Crime values") +
  geom_text(aes(label=models_rem$Prediction),vjust=-1)+
  theme_stata()

