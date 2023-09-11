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
summary(data[ncol(data)])


## -------------------------------------------------------------------------------------------------------------------
#since we're learning about crime data - extract it to a variable

crime <- data$Crime
crime


## -------------------------------------------------------------------------------------------------------------------
#Histogram
hist(crime,
     main = "Crimes Histogram",
     xlab = "Crimes per 100 000 people",
     col = "lightblue",
     probability = TRUE)
#add norm distr curbv
curve(dnorm(x, 
            mean=mean(crime), 
            sd=sd(crime)), 
      add=TRUE,
      col = "purple", 
      lwd = 3)


## -------------------------------------------------------------------------------------------------------------------
shapiro.test(crime)


## -------------------------------------------------------------------------------------------------------------------
qqnorm(crime)
qqline(crime)


## -------------------------------------------------------------------------------------------------------------------
plot_ly(y=crime, 
        type="box", 
        quartilemethod="inclusive",
        fillcolor="lightblue",
        boxpoints="outliers",
        #color for outliers
        marker=list(color="darkred")) %>%
  layout(title="Number of Crimes per 100 000 people",
         xaxis=list(title="Crimes"),
         yaxis=list(title="Number of commited crimes"))


## -------------------------------------------------------------------------------------------------------------------
#for left tail 
grubbs.test(crime, 
            type=10, 
            opposite = TRUE, #?
            )


## -------------------------------------------------------------------------------------------------------------------
#check max values (RIGHT  TAIL)
grubbs.test(crime, 
            type=10,#test for one outlier on one tail
            opposite = FALSE #specify tail = right (max vals)
            )


## -------------------------------------------------------------------------------------------------------------------
#New ds without the outlier on the right
crime.1 <- crime[-which.max(crime)]


## -------------------------------------------------------------------------------------------------------------------
#see graphs to see if anythings changed
qqnorm(crime.1)
qqline(crime.1)


## -------------------------------------------------------------------------------------------------------------------
shapiro.test(crime.1)


## -------------------------------------------------------------------------------------------------------------------
#just in case check for left tail (min)
grubbs.test(crime.1, 
            type=10, 
            opposite = TRUE, #?
)


## -------------------------------------------------------------------------------------------------------------------
grubbs.test(crime.1, 
            type=10,#test for one outlier on one tail
            opposite = FALSE #specify tail = right (max vals)
)


## -------------------------------------------------------------------------------------------------------------------
crime.2 <- crime.1[-which.max(crime.1)]


## -------------------------------------------------------------------------------------------------------------------
qqnorm(crime.2)
qqline(crime.2)


## -------------------------------------------------------------------------------------------------------------------
shapiro.test(crime.2)


## -------------------------------------------------------------------------------------------------------------------
hist(crime.2,
     main = "Crimes Histogram",
     xlab = "Crimes per 100 000 people",
     col = "lightblue",
     probability = TRUE)
#add norm distr curve
curve(dnorm(x, 
            mean=mean(crime.2), 
            sd=sd(crime.2)), 
      add=TRUE,
      col = "purple", 
      lwd = 3)


## -------------------------------------------------------------------------------------------------------------------
#just in case check for left tail (min dps)
grubbs.test(crime.2, 
            type=10, 
            opposite = TRUE, #?
)


## -------------------------------------------------------------------------------------------------------------------
grubbs.test(crime.2, 
            type=10,#test for one outlier on one tail
            opposite = FALSE #specify tail = right (max vals)
)



## -------------------------------------------------------------------------------------------------------------------
plot_ly(y=crime.2, 
        type="box", 
        quartilemethod="inclusive",
        fillcolor="lightblue",
        boxpoints="outliers",
        #color for outliers
        marker=list(color="darkred")) %>%
  layout(title="Number of Crimes per 100 000 people (outliers removed)",
         xaxis=list(title="Crimes"),
         yaxis=list(title="Number of commited crimes"))


## -------------------------------------------------------------------------------------------------------------------
after_removal <- data.table(
  c("minimum", "lower-hinge", "median", "upper-hinge", "maximum"),
  fivenum(crime), 
  fivenum(crime.2))
colnames(after_removal) <- c("fivenum", "Initial data", "Data w/o 2 outliers")
after_removal

