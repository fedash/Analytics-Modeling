# Regression Tree and Risk Assessment Models

## Objective
This task involves building and analyzing different types of predictive models for two different datasets. The first part uses crime data to build regression tree and random forest models, and the second part uses German credit data to build a logistic regression model for credit risk assessment.

### Part 1
Building and analyzing (a) a regression tree model and (b) a random forest model using the crime data set.

### Part 2
Using logistic regression to build a predictive model to identify good or bad credit risks in German credit data.

## Methodology

### Part 1
- **Data Source**: `uscrime.txt`
- **Statistical Models**: Regression Tree and Random Forest
- **Programming Language**: R

### Part 2
- **Data Source**: `germancredit.txt`
- **Statistical Models**: Logistic Regression
- **Programming Language**: R

## Packages and Libraries Used

### Common across both parts
- `dplyr`: For data manipulation
- `tidyverse`: For an integrated approach to data manipulation and visualization
- `ggplot2`: For data visualization
- `data.table`: For fast data manipulation

### Specific to Part 1
- `tree`: For regression trees
- `rpart`: For recursive partitioning
- `randomForest`: For random forest models

### Specific to Part 2
- `glm`: For generalized linear models
- `binomial`: For logistic regression with binomial distribution
