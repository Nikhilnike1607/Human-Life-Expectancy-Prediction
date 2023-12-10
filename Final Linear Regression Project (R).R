# Contents
# 1. Importing The Dataset
# 2. Checking And Treating The Null Values
# 3. Outliers Detection and Treatment
# 4. Exploratory Data Analysis (EDA)
# 5. Correlation Check
# 6. Creating Dummies
# 7. Train-Test Split And Scaling 
# 8. Model Building
# 9. Model Testing
# 10. Conclusion / Summary

# Packages
library(corrplot)
library(fastDummies)
library(caret)
library(data.table)
library(dplyr)
library(car)
library(lmtest)

# 1. Importing The Dataset
df <- read.csv('Life Expectancy Data.csv', header = TRUE)

# Checking the shape of the data (Rows x Columns)
dim(df)

# Checking if all the data types are in the correct format
str(df)
# We need to change the data type of 'Year' from int to chr
df$Year <- as.character(df$Year)

# Checking the summary of the dataset just for our reference.
summary(df)

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 2. Checking And Treating The Null Values
colSums(is.na(df))
colMeans(is.na(df)) * 100

# Treating the null values (Median for continuous) for 'Life.expectancy', 'Adult.Mortality', 'Alcohol','GDP', 'Hepatitis.B', 'BMI', 'Polio', 'Total.expenditure', 'Diphtheria', 'Population', 'thinness..1.19.years', 'thinness.5.9.years', 'Income.composition.of.resources','Schooling' columns
for (i in c('Life.expectancy', 'Adult.Mortality', 'Alcohol', 'GDP', 'Hepatitis.B', 'BMI', 'Polio', 'Total.expenditure', 'Diphtheria', 'Population', 'thinness..1.19.years', 'thinness.5.9.years', 'Income.composition.of.resources', 'Schooling')) {
  df[[i]][is.na(df[[i]])] <- median(df[[i]], na.rm = TRUE)
  cat(df[[i]])
}

# Checking the treated null values again 
colSums(is.na(df))

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 3. Outliers Detection And Treatment
# Tukey's Fences - Outliers Detection
# Checking the number of outliers in all the columns
cont_cols <- colnames(df)[!(colnames(df) %in% c('Country', 'Year', 'Status'))]
for (i in cont_cols) {
  cat(i, length(boxplot.stats(df[[i]])$out), '\n')
}
# We won't be treating any of the outliers for now because some of the outliers are valid data points that have important information to convey. We don't want to face data loss.

plot(df[['HIV.AIDS']], df[['Life.expectancy']], xlab = 'HIV AIDS', ylab = 'Life Expectancy')

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 4. Exploratory Data Analysis (EDA)
# Let's regress all the continuous independent variables against the target variable (Life.expectancy)

# A. Adult.Mortality vs Life.expectancy
plot(df[['Adult.Mortality']], df[['Life.expectancy']], xlab = 'Adult Mortality', ylab = 'Life Expectancy')
# Slight Statistical relation. As the mortality rate increases, life expectancy decreases, which is obvious.

# B. infant.deaths vs Life.expectancy
plot(df[['infant.deaths']], df[['Life.expectancy']], xlab = 'Infant Deaths', ylab = 'Life Expectancy')
# Very light/no statistical relation

# C. Alcohol vs Life.expectancy
plot(df[['Alcohol']], df[['Life.expectancy']], xlab = 'Alcohol', ylab = 'Life Expectancy')
# No statistical relation

# D. percentage.expenditure vs Life.expectancy
plot(df[['percentage.expenditure']], df[['Life.expectancy']], xlab = '% Expenditure', ylab = 'Life Expectancy')
# Slight statistical relation. As the expenditure of the people increases, it is indirectly increasing Life Expectancy. This can be due to expenditure on high-quality nutrients and medications, which is actually increasing the life expectancy of the person.

# E. Hepatitis.B vs Life.expectancy
plot(df[['Hepatitis.B']], df[['Life.expectancy']], xlab = 'Hepatitis B', ylab = 'Life Expectancy')
# No statistical relation

# F. Measles vs Life.expectancy
plot(df[['Measles']], df[['Life.expectancy']], xlab = 'Measles', ylab = 'Life Expectancy')
# No statistical relation

# G. BMI vs Life.expectancy
plot(df[['BMI']], df[['Life.expectancy']], xlab = 'BMI', ylab = 'Life Expectancy')
# Very low/no statistical relation

# H. under.five.deaths vs Life.expectancy
plot(df[['under.five.deaths']], df[['Life.expectancy']], xlab = 'Under Five Deaths', ylab = 'Life Expectancy')
# Very low/No statistical relation

# I. Polio vs Life.expectancy
plot(df[['Polio']], df[['Life.expectancy']], xlab = 'Polio', ylab = 'Life Expectancy')
# No statistical relation

# J. Total.expenditure vs Life.expectancy
plot(df[['Total.expenditure']], df[['Life.expectancy']], xlab = 'Total Expenditure', ylab = 'Life Expectancy')
# No statistical relation

# K. Diphtheria vs Life.expectancy
plot(df[['Diphtheria']], df[['Life.expectancy']], xlab = 'Diphtheria', ylab = 'Life Expectancy')
# No statistical relation

# L. HIV.AIDS vs Life.expectancy
plot(df[['HIV.AIDS']], df[['Life.expectancy']], xlab = 'HIV AIDS', ylab = 'Life Expectancy')
# Slight statistical relation. As the presence of HIV AIDS increases, life expectancy has been gradually decreasing from the graph.

# M. GDP vs Life.expectancy
plot(df[['GDP']], df[['Life.expectancy']], xlab = 'GDP', ylab = 'Life Expectancy')
# Slight statistical relation. As the GDP of the country increases, the life expectancy of that country is also gradually increasing.

# N. Population vs Life.expectancy
plot(df[['Population']], df[['Life.expectancy']], xlab = 'Population', ylab = 'Life Expectancy')
# No statistical relation

# O. thinness..1.19.years vs Life.expectancy
plot(df[['thinness..1.19.years']], df[['Life.expectancy']], xlab = 'Thinness 1-19 Yrs', ylab = 'Life Expectancy')
# No statistical relation

# P. thinness.5.9.years vs Life.expectancy
plot(df[['thinness.5.9.years']], df[['Life.expectancy']], xlab = 'Thinness 5-9 Yrs', ylab = 'Life Expectancy')
# No statistical relation

# Q. Income.composition.of.resources vs Life.expectancy
plot(df[['Income.composition.of.resources']], df[['Life.expectancy']], xlab = 'Income Composition of Resources', ylab = 'Life Expectancy')
# Slight statistical linear relation. As the Human Development Index score of the country increases, the Life Expectancy of the people is also increasing.

# R. Schooling vs Life.expectancy
plot(df[['Schooling']], df[['Life.expectancy']], xlab = 'Schooling', ylab = 'Life Expectancy')
# Slight statistical linear relation. As the schooling of the population increases, it is indirectly influencing the life expectancy of the population with positive growth.

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 5. Correlation Check
# Checking the correlation between the variables
# Let's see the correlation matrix here
cor_matrix <- cor(df[, cont_cols])
corrplot(cor_matrix, method = 'color')
cor_matrix <- as.data.frame(cor_matrix)
cor_matrix[['Cols']] <- names(cor_matrix)
cor_matrix <- cor_matrix[order(-cor_matrix[['Life.expectancy']]), ][c('Cols', 'Life.expectancy')]
cor_matrix

# Variables that are highly correlated with the target variable are:
# 1. Schooling (0.713)
# 2. Adult.Mortality (-0.696)
# 3. Income.composition.of.resources (0.688)
# 4. BMI (0.557)
# 5. HIV.AIDS (-0.557)
# Apart from this, we could also see some multicollinearity between the variables, but we shall eliminate them in the feature elimination process 

colSums(is.na(df))

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 6. Creating Dummies
# Checking the binary columns before performing the Dummy columns creation for categorical columns
for (i in colnames(df)) {
  cat(i, length(unique(df[[i]])), '\n')
} 

# Only the 'Status' variable has binary values, so excluding that we shall create dummies for variables 'Country' and 'Year'.
df <- dummy_cols(df, select_columns = c('Country', 'Year'), remove_first_dummy = TRUE)
df <- subset(df, select = -c(Country, Year))

# Checking the dimension of the data after creating the dummies
dim(df)

# Converting the categories of the binary column 'Status' to 1's and 0's
df[['Status']] <- ifelse(df[['Status']] == 'Developing', 1, 0)

# Checking the data types of all the columns before building the linear regression model
str(df)

# Converting all the data types of all the columns to numeric
for (i in names(df)) {
  df[[i]] <- as.numeric(df[[i]])
}
str(df)

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 7. Train-Test Split And Scaling
# Train and Test split with stratification (And Scaling)
set.seed(100)
index <- createDataPartition(df[['Life.expectancy']], p = 0.8, list = FALSE, times = 1)
train <- df[index,]
# Scaling train data
Life.expectancy <- train[['Life.expectancy']]
x_train <- data.frame(subset(train, select = -c(Life.expectancy)))
scaling <- preProcess(x_train, method = c("range"))
x_train <- predict(scaling, x_train)
train <- data.frame(cbind(x_train, Life.expectancy))
# In the test data, we are separating the target variable from test (x_test) before we make the predictions.
y_test <- df[-index,][['Life.expectancy']]
x_test <- data.frame(subset(df[-index,], select = -c(Life.expectancy)))
# Scaling test data
x_test <- predict(scaling, x_test)
# Checking the train and test data dimensions
dim(train)
length(y_test)
dim(x_test)

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 8. Model Building
# Model 1
# Building the multiple linear regression model with all the available variables
model_1 <- lm(Life.expectancy ~., data = train)
summary(model_1)
# By adding all the variables, we can see that both R-squared and adjusted R-squared are high (~96% due to many variables), F-statistic is high, and p-value is low (<0.05).
# However, there are variables whose p-values are more than 0.05 and show that they are not significant to be in the model. So let's use RFE & backward elimination method and keep removing them to build the most efficient model.

# Automating the feature elimination method
for (i in c(1:300)) {
  df_p <- na.omit(data.frame(summary(model_1)[['coefficients']]))
  df_p <- df_p %>%
    tibble::rownames_to_column(var = "new_index")
  colnames(df_p) <- c('Columns', 'Coefficients', 'Std_error', 't_value', 'p_value')
  if (max(df_p[['p_value']]) > 0.05) {
    remove_col <- df_p[['Columns']][which(df_p[['p_value']] == max(df_p[['p_value']]))]
    train <- train[, -which(names(train) == remove_col)]
    model_1 <- lm(Life.expectancy ~., data = train)
  } else {
    break
  }
}
df_p
summary(model_1)

# We can see that there are some columns that don't have values for Coefficients, Std error, and p-values etc. Thus let's drop the columns 'Country_San.Marino' and 'Country_Palau'
train <- subset(train, select = -c(Country_San.Marino, Country_Palau))

# Building the Model 2
model_2 <- lm(Life.expectancy ~., data = train)
summary(model_2)
# Variance Inflation Factor (Checking the Multicollinearity)
data.frame(vif(model_2))
# VIF values less than 5 are in the acceptable range. Let's drop the variables that have VIF score more than 5.

# Building the Model 3 (Dropping Multicollinear column 'infant.deaths')
train <- subset(train, select = -c(infant.deaths))
model_3 <- lm(Life.expectancy ~., data = train)
summary(model_3)
# Variance Inflation Factor (Checking the Multicollinearity)
data.frame(vif(model_3))
# 'Status' column has the highest VIF score now, but it is an important variable so we cannot drop it. Let's try dropping 'GDP' and see.

# Building the Model 4 (Dropping Multicollinear column 'GDP')
train <- subset(train, select = -c(GDP))
model_4 <- lm(Life.expectancy ~., data = train)
summary(model_4)
# Variance Inflation Factor (Checking for Multicollinearity)
data.frame(vif(model_4))
# Still, the values of the VIF of Status and some other columns are not reduced much. Let's drop the Status column itself and see the R-squared value we get in the model.
# After testing the model without the Status variable, the R-squared significantly dropped. Status was one of the variables with high coefficients, influencing the target variable a lot. Hence, we shall keep it.

# Final feature elimination based on the P-value
for (i in c(1:300)){
  df_p = na.omit(data.frame(summary(model_4)[['coefficients']]))
  df_p <- df_p %>%
    tibble::rownames_to_column(var = "new_index")
  colnames(df_p) = c('Columns', 'Coefficients', 'Std_error', 't_value', 'p_value')
  if (max(df_p[['p_value']]) > 0.05){
    remove_col = df_p[['Columns']][which(df_p[['p_value']] == max(df_p[['p_value']]))]
    train=train[, -which(names(train) == remove_col)]
    model_4 = lm(Life.expectancy ~., data = train)
  }
  else{
    break
  }
}
df_p
summary(model_4)
data.frame(vif(model_4))
# Variables with high VIF scores are 'Status' (14.07), 'thinness.5.9.years' (5.65), 'Schooling'(Schooling). Now let's see their coefficient values (influence) with respect to the target.
# 1. Status - -22.9141
# 2. thinness.5.9.years - 3.3821
# 3. Schooling - 3.9126
# Slight influence on the target variable is there, so let's keep them in the model.

# Final Model (model_4)
final_model = model_4

# Confidence interval check for the final model
confint(final_model, level=0.95)

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 9. Model Testing
# Prediction on the test data
x_test = subset(x_test, select=names(train)[names(train)!='Life.expectancy'])
y_test_pred=predict(final_model, newdata=x_test)
cor(y_test, y_test_pred)^2
cor(train[['Life.expectancy']], fitted(final_model))^2
# R2 score of training was 0.9599 and that of the test was 0.9492. The difference is 0.0107, which is the loss of prediction accuracy value. Anything <0.02 is acceptable, but let's do statistical analysis to test the significance of the model.

# Model Evaluation (Assumptions Check)
# Normality Check (Residual Analysis)
plot(density(resid(model_1)), main = 'Residual Error Distribution', xlab = 'Error', col = 'blue', lwd = 4)
# We can see that the error terms are normally distributed.

# Let's see Residuals vs y_train_pred (Predicted values of train data)
plot(y=resid(model_1), x=fitted(model_1), main='Residuals vs y_train_pred (Predicted values of train data)', xlab='Residuals', ylab='y_train_pred', col='blue')
abline(h = 0, col = 'red', lty = 2, lwd=3)
# Residuals are randomly scattered and centered towards zero.

# Homoscedasticity and Linearity Check (Train Data)
plot(y=train[['Life.expectancy']], x=fitted(model_1), main='Homoscedasticity and Linearity between y_train and y_train_pred', col='blue', xlab='y_train', ylab='y_train_pred')
abline(lm(train[['Life.expectancy']]~fitted(model_1)), col = 'red', lwd = 2)
# There is a linear relationship between y_train and y_train_pred.

# Homoscedasticity and Linearity Check (Test Data)
plot(y=y_test, x=y_test_pred, main='Homoscedasticity and Linearity between y_test and y_test_pred', col='blue', xlab='y_test', ylab='y_test_pred')
abline(lm(y_test~y_test_pred), col = 'red', lwd = 2)

# Hypothesis testing (F - test)
# From the summary of the model
summary(final_model)
# Since the p-value is below the significance level 0.05 (alpha), we reject the null hypothesis and conclude that at least one predictor variable is significant in explaining the variability in the response variable (Life.expectancy). Hence, the model is significant.

# Lack of fit test (F statistic) - Linearity Relationship
# Let's calculate the reduced model (SLR model with only the target column and Intercept)
reduced_model = lm(Life.expectancy~1, data=train)
lack_of_fit_statistic <- (sum((residuals(final_model))^2) - sum(residuals(reduced_model)^2)) / (length(residuals(final_model)) - length(coefficients(final_model)))
df_numerator <- length(coefficients(final_model)) - length(coefficients(reduced_model))
df_denominator <- length(residuals(final_model)) - length(coefficients(final_model))
p_value_lack_of_fit <- pf(lack_of_fit_statistic, df_numerator, df_denominator, lower.tail = FALSE)
# Printing the results
cat("Lack-of-Fit Test Statistic:", lack_of_fit_statistic, "\n")
cat("Degrees of Freedom (numerator):", df_numerator, "\n")
cat("Degrees of Freedom (denominator):", df_denominator, "\n")
cat("P-value:", p_value_lack_of_fit, "\n")
# Since we have a p-value of 1, there is no chance to reject the null hypothesis, and we conclude that there is a strong linear relationship with the predictors and the target variable.

# Breusch-Pagan Test (Homoscedasticity)
bp_test = bptest(final_model)
bp_test
# Since the p-value is less than 0.05, we reject the null hypothesis and conclude that there is evidence of heteroscedasticity. This we also saw in the diagnostic plot that there were dependencies seen between the residuals but they are slightly ignorable.

# Shapiro-Wilk test (Normality Test)
norm_test = shapiro.test(residuals(final_model))
norm_test
# Since the p-value is less than 0.05, we reject the null hypothesis and conclude that the residuals are not normally distributed.

#***************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

# 10. Conclusion/Summary

# Finalized Model = Model 4 

# Top 7 variables influencing the Target variable (Life Expectancy) are:
sort(coef(model_1), decreasing = TRUE)
# 1. infant.deaths (140.7829)
# 2. under.five.deaths(-139.9208)
# 3. Country_France (23.5337)
# 4. Country_Canada (22.4182)
# 5. Country_Israel (22.4007)
# 6. Country_Israel (22.1800)
# 7. Status (22.0440)