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



## -------------------------------------------------------------------------------------------------------------------
data <- read.table("temps.txt", 
                   header = TRUE, 
                   stringsAsFactors = FALSE,
                   sep = "", 
                   dec = ".")

#rename to exclude X from att names
data <- data %>% rename_at(vars(starts_with("X")), funs(str_replace(., "X", "")))

data[,"DAY"] <- as.Date(data[,"DAY"], "%d-%B")
#leave only month and day
data[,"DAY"] <- format(data[,"DAY"],"%m.%d")
head(data)


## -------------------------------------------------------------------------------------------------------------------
boxplot(data[-1],
        xlab="Year",
        ylab="°F",
        col="lightblue",
        border="darkblue")
title(main="July-October Temperatures in Atlanta (1996-2015")


## -------------------------------------------------------------------------------------------------------------------
#There are two critical inputs we must give the function — frequency and start.

#Turn col data into one vector
data_vector <- as.vector(unlist(data[,2:ncol(data)]))

#Turn data to ts object
#we have 123 days' temp for each year - we will have 'seasons' from 1996 each lasting 123 days

datats <- ts(data_vector,start=1996,frequency=nrow(data))
head(datats)



## -------------------------------------------------------------------------------------------------------------------
plot(datats, main="Temperatures in Atlanta (07.01-10.31, 1996-2015)", xlab="Year", ylab="Temperature")


## -------------------------------------------------------------------------------------------------------------------
ggplot(as.data.frame(datats),aes(x=c(1:2460),y=x))+
  geom_line(aes(x=c(1:2460),y=x),color="darkblue")+
  geom_smooth(method="loess",color="green")


## -------------------------------------------------------------------------------------------------------------------
components_dts <- decompose(datats)
plot(components_dts)


## -------------------------------------------------------------------------------------------------------------------


#trend
min(na.omit(components_dts$trend))
max(na.omit(components_dts$trend))


#seasonal 
min(na.omit(components_dts$seasonal))
max(na.omit(components_dts$seasonal))


#random 
min(na.omit(components_dts$random))
max(na.omit(components_dts$random))


## -------------------------------------------------------------------------------------------------------------------
#Decomposition (multiplicative)
components_dts_mult <- decompose(datats, type="mult")
plot(components_dts_mult)


## -------------------------------------------------------------------------------------------------------------------
#trend 
min(na.omit(components_dts_mult$trend))
max(na.omit(components_dts_mult$trend))


#seasonal 
min(na.omit(components_dts_mult$seasonal))
max(na.omit(components_dts_mult$seasonal))


#random 
min(na.omit(components_dts_mult$random))
max(na.omit(components_dts_mult$random))


## -------------------------------------------------------------------------------------------------------------------
#single - no trend no season
HW1 <- HoltWinters(datats, 
                     beta=FALSE, #model without trend 
                     gamma=FALSE) #not considering seasonality 
                     
HW1 


## -------------------------------------------------------------------------------------------------------------------
HW2 <- HoltWinters(datats, 
                   gamma=FALSE) #now we have trend value in the M but still no season
HW2


## -------------------------------------------------------------------------------------------------------------------
HW3_a <- HoltWinters(datats)
HW3_a


## -------------------------------------------------------------------------------------------------------------------
plot(HW3_a$coefficients[3:length(HW3_a$coefficients)], 
     type="o",
     col="lightblue", 
     lty=2, 
     lwd=1, 
     main="Seasonal Factors (Additive)", 
     xlab="S", 
     ylab="Value")
points(HW3_a$coefficients[3:length(HW3_a$coefficients)], 
       col="darkblue", 
       pch=19, cex=1)
abline(h=0)


## -------------------------------------------------------------------------------------------------------------------
HW3_m <- HoltWinters(datats, seasonal = "multiplicative")
HW3_m


## -------------------------------------------------------------------------------------------------------------------
plot(HW3_m$coefficients[3:length(HW3_m$coefficients)], 
     type="o",
     col="lightblue", 
     lty=2, 
     lwd=1, 
     main="Seasonal Factors (Multiplicative)", 
     xlab="S", 
     ylab="Value")
points(HW3_m$coefficients[3:length(HW3_m$coefficients)], 
       col="darkblue", 
       pch=19, cex=1)
abline(h=1)


## -------------------------------------------------------------------------------------------------------------------
es_models <- data.frame(
  alpha=c(HW1$alpha,HW2$alpha,HW3_a$alpha,HW3_m$alpha),
  beta=c(NA,HW2$beta,HW3_a$beta,HW3_m$beta),
  gamma=c(NA,NA,HW3_a$gamma,HW3_m$gamma),
  Baseline_Estimate=c(HW1$coefficients[1],HW2$coefficients[1],HW3_a$coefficients[1],HW3_m$coefficients[1]),
  Trend_Estimate=c(HW1$coefficients[2],HW2$coefficients[2],HW3_a$coefficients[2],HW3_m$coefficients[2]))

rownames(es_models) <- c('Single', 
                         'Double', 
                         'Triple (Additive)',
                         'Triple (Multiplicative)')

es_models


## -------------------------------------------------------------------------------------------------------------------
plot(fitted(HW3_a), main="Decomposition (Additive)")
plot(fitted(HW3_m), main="Decomposition (Multiplicative)")


## -------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(HW3_a, main="Additive", xlab="Year", ylab="Observed/Fitted")

plot(HW3_m, main="Multiplicative", xlab="Year", ylab="Observed/Fitted")


## -------------------------------------------------------------------------------------------------------------------
print('SSE Additive')
HW3_a$SSE
print('SSE Multiplicative')
HW3_m$SSE


## -------------------------------------------------------------------------------------------------------------------
head(HW3_a$fitted)


## -------------------------------------------------------------------------------------------------------------------
#put seasonal factors in the matrix
season_factors <- matrix(HW3_a$fitted[,4],nrow=123)
dim(season_factors)
head(season_factors)


## -------------------------------------------------------------------------------------------------------------------
#rename
colnames(season_factors) <- c(1997:2015)
rownames(season_factors) <- data$DAY
#check
head(season_factors)
tail(season_factors)


## -------------------------------------------------------------------------------------------------------------------
sf_avg<-data.frame(rowMeans(season_factors, n = 19)) 
colnames(sf_avg)<-'Avg_seasonal_factor'
head(sf_avg)


## -------------------------------------------------------------------------------------------------------------------
#ggplot(sf_avg, aes(x=1:123, y=Avg_seasonal_factor))+
  #geom_line()+
  #geom_point()

plot_ly(data=sf_avg, x=1:123, y=~Avg_seasonal_factor, type='scatter', mode='lines+markers', fill='tozeroy', fillcolor='lightblue')



## -------------------------------------------------------------------------------------------------------------------
models_cusum <- vector(mode="list", length=ncol(season_factors))
violat_cusum <- vector(mode="list", length=ncol(season_factors))


## -------------------------------------------------------------------------------------------------------------------
#mean and stdev of seasonal factors for each year
#use July temps 
mean_seasons <- vector(mode="list", length=0)
stdev_seasons <- vector(mode="list", length=0)
for (i in 1:ncol(season_factors)){
  mean_seasons  <- append(mean_seasons,mean(season_factors[1:51,i]))
  stdev_seasons <- append(stdev_seasons,sd(season_factors[1:51,i]))                     
  
}
unlist(mean_seasons)
unlist(stdev_seasons)


## -------------------------------------------------------------------------------------------------------------------
dec_int <- 4 #4 st deviations for T
se_shift <- 2 #2 instead of 1, since shift = 2C. SO 1st deviation for C


## -------------------------------------------------------------------------------------------------------------------

for (y in 1:ncol(season_factors)) {
  #cusum model for each year
  models_cusum[[y]] <- cusum(season_factors[,y], #column of the year in loop
                             center = unlist(mean_seasons)[y],
                             std.dev = unlist(stdev_seasons)[y], #use st dev for the particular year
                             decision.interval = dec_int, #T value, 5 st dev-s
                             se.shift = se_shift, #shift value, 2C, 2
                             plot = TRUE,
                             title = colnames(season_factors)[y]) #add plot for each year's cusum
  #cusum violation for each year - moment when fall came
  #use the model we built above
  #$use lower violation - the first day that violated temp - the 1st day of falL!
  violat_cusum[[y]] <- models_cusum[[y]]$violations$lower}


## -------------------------------------------------------------------------------------------------------------------
#vector to store first days of fall
first_days <- vector(mode="list", length=ncol(season_factors))

#add to that vector first days 
for (y in 1:ncol(season_factors)){
  first_days[y] <- min(violat_cusum[[y]])
}
#day index of first day of fall
first_days <- unlist(first_days)

#turn indexes to days
fall_dates<- vector(mode="list", length=0)
for (x in 1:19){
  i=0
  d<-first_days[x]
  fall_dates <- append(fall_dates, data[d,1], after=i)
  i=i+1
  }

fall_dates<-unlist(fall_dates)
fall_dates


## -------------------------------------------------------------------------------------------------------------------
year_list <- 1997:2015
year_list <- as.list(year_list)
date_year <- do.call(rbind, Map(data.frame, Year=year_list, FallStart=fall_dates))
date_year


## -------------------------------------------------------------------------------------------------------------------
results_plot<- ggplot(data = date_year, aes(x = Year, y = FallStart, group=1)) +
  geom_line(linetype="dashed", color="darkblue") +
  geom_hline(yintercept = 2, color="lightblue")+ #compare to 09.21 for 2015
  geom_point(color="blue")+
  labs(title="Fall Start Dates in Atlanta (1997-2015)", y="Date")

results_plot



## -------------------------------------------------------------------------------------------------------------------
fall_dates_hw3 <- c('09.25','08.16','08.15','09.05','09.28','09.01','08.14','09.18','09.09','10.07','08.10','09.10','08.31','09.02','09.06','09.22','08.15','09.25','09.02')
year_list1 <- 1997:2015
year_list1 <- as.list(year_list)
date_year1 <- do.call(rbind, Map(data.frame, Year=year_list, FallStart=fall_dates_hw3))
date_year1


## -------------------------------------------------------------------------------------------------------------------
previous<-ggplot(data = date_year1, aes(x = Year, y = FallStart, group=1)) +
  geom_line(linetype="dashed", color="darkgreen") +
  geom_point(color="green")+
  labs(title="No Exp.Sm. - Fall Start Dates", y="Date")
previous

