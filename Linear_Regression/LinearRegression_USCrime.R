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


## -------------------------------------------------------------------------------------------------------------------
data <- read.table("uscrime.txt", 
                   header = TRUE, 
                   stringsAsFactors = FALSE,
                   sep = "", 
                   dec = ".")
head(data)


## -------------------------------------------------------------------------------------------------------------------

is.null(data)


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


## -------------------------------------------------------------------------------------------------------------------
hist_plots <- ggplot(melted,
                    aes(x=value))+
              geom_histogram(aes(y=..density..), colour="black", fill="white")+
              geom_density(alpha=.3, color="skyblue", fill="skyblue")+
              facet_wrap(~variable, scale="free")+
              theme_fivethirtyeight()
hist_plots


## -------------------------------------------------------------------------------------------------------------------
corrplot(cor(data[,-16]), method='pie', type="upper")


## -------------------------------------------------------------------------------------------------------------------
cm <- as.table(round(cor(data[,-16]),1))
cm


## -------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
#regression:
model_1 <- lm(Crime ~ ., data=data)
#get model summary
summary(model_1, vif=TRUE)
#plot results
plot(model_1)


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
pred_model_1 <- predict(model_1, test_data)
pred_model_1


## -------------------------------------------------------------------------------------------------------------------
cm


## -------------------------------------------------------------------------------------------------------------------
#Overall Multicollinearity Diagnostics Measures
omcdiag(model_1)


## -------------------------------------------------------------------------------------------------------------------
#Individual Multicollinearity Diagnostic Measures
#locate where mc is
imcdiag(model_1)


## -------------------------------------------------------------------------------------------------------------------
vif_1 <- vif(model_1)
barplot(vif_1,
        main="VIF Values (model with all parameters)",
        horiz=TRUE,
        col="lightblue",
        las=2)
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
pcor(data[,-16], method = "pearson")


## -------------------------------------------------------------------------------------------------------------------
#regression:
model_2 <- lm(Crime ~ .-Po2, data=data)
#get model summary
summary(model_2)
#Check VIF
as.table(round(vif(model_2),2))
#check prediction
pred_model_2 <- predict(model_2, test_data)
pred_model_2


## -------------------------------------------------------------------------------------------------------------------
vif_2 <- vif(model_2)
barplot(vif_2,
        main="VIF Values (-Po2)",
        horiz=TRUE,
        col="lightblue",
        las=2)
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
#regression:
model_3 <- lm(Crime ~ .-Po2 -Wealth, data=data)
#get model summary
summary(model_3)
#Check VIF
as.table(round(vif(model_3),2))
#check prediction
pred_model_3 <- predict(model_3, test_data)
pred_model_3


## -------------------------------------------------------------------------------------------------------------------
vif_3 <- vif(model_3)
barplot(vif_3,
        main="VIF Values (-Po2, -Wealth)",
        horiz=TRUE,
        col="lightblue",
        las=2)
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
#regression:
model_4 <- lm(Crime ~ .-Po2 -Wealth -U1 , data=data)
#get model summary
summary(model_4)
#Check VIF
as.table(round(vif(model_4),2))
#check prediction
pred_model_4 <- predict(model_4, test_data)
pred_model_4


## -------------------------------------------------------------------------------------------------------------------
vif_4 <- vif(model_4)
barplot(vif_4,
        main="VIF Values (-Po2 - Wealth - U1)",
        horiz=TRUE,
        col="lightblue",
        las=2)
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
#regression:
model_final <- lm(Crime ~ M+Ed+Po1+U2+Ineq+Prob , data=data)
#get model summary
summary(model_final)
#Check VIF
as.table(round(vif(model_final),2))
#check prediction
pred_model_final <- predict(model_final, test_data)
pred_model_final
#plot results
plot(model_final)


## -------------------------------------------------------------------------------------------------------------------
vif_final <- vif(model_final)
barplot(vif_final,
        main="VIF Values (Final Model)",
        horiz=TRUE,
        col="lightblue",
        las=2)
abline(v=5, lwd=3, lty=2, col="orange")
abline(v=10, lwd=3, lty=2, col="darkred")


## -------------------------------------------------------------------------------------------------------------------
#Overall Multicollinearity Diagnostics Measures
omcdiag(model_final)


## -------------------------------------------------------------------------------------------------------------------
#Individual Multicollinearity Diagnostic Measures
#locate where mc is
imcdiag(model_final)


## -------------------------------------------------------------------------------------------------------------------
cv_final <- cv.lm(data,
                  model_final,
                  m=5) #m is the number of folds
cv_final


## -------------------------------------------------------------------------------------------------------------------
#sum of squared errors (residuals)
sseres_cv_final <- nrow(data)*attr(cv_final,"ms")
sseres_cv_final

#sum of squares total
sst <- sum((data$Crime - mean(data$Crime))^2)
sst

#R-squared
R2_cv_final <- 1-sseres_cv_final/sst
R2_cv_final

#adjusted R-squared
R2_adj_cv_final <- 1 - (((1-R2_cv_final)*(nrow(data)-1))/(nrow(data)-6-1)) #6 predictors
R2_adj_cv_final


## -------------------------------------------------------------------------------------------------------------------
cv_model1 <- cv.lm(data,
                  model_1,
                  m=5) #m is the number of folds
cv_model1


## -------------------------------------------------------------------------------------------------------------------
#sum of squared errors (residuals)
sseres_cv_model1 <- nrow(data)*attr(cv_model1,"ms")
sseres_cv_model1

#R-squared
R2_cv_model1 <- 1-sseres_cv_model1/sst
R2_cv_model1

#adjusted R-squared
R2_adj_cv_model1 <- 1 - (((1-R2_cv_model1)*(nrow(data)-1))/(nrow(data)-6-1))
R2_adj_cv_model1

