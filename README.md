# Predicting_Fuel_Efficiency-A-Linear-Regression-Analysis-
#Title: Predicting Fuel Efficiency (MPG) in Cars: A Linear Regression Analysis
#Name:  Anush Kumar Bolla
#Id : 00877066


#Data Exploration:Importing the data set and loading the data set

data("mtcars")
data = data.frame(mtcars)
View(data)
summary(data)
str(data)

#Variable Selection: HP, WT and AM

#Ans:

#I Have Considered Hp (Horsepower), Wt (Weight) and am (Transmission) as the independent variables (predictors).
#I considered weight as on of the independent variables because the vehicles with more weight are tend to have low MPG.
#I considered Horsepower as another independent variable because the vehicles with the more HP are tend to consume more fuel than less HP vehicles.
#I considered transmission as one more independent variable to assess the fuel efficiency as manual cars are more fuel efficient than

#Data Split: Splitting the data into training_set and test_set to evaluate the model performance

library(caTools)
set.seed(123)
data_split <- sample.split(data$mpg,SplitRatio = 0.7)
summary(data_split)
training_set <- subset(data, data_split == TRUE)
View(training_set)
test_set <- subset(data, data_split == FALSE)
View(test_set)

#Linear Regression Model: Fitting the simple linear regression model

lin_reg <- lm(formula = mpg ~ wt+hp+am, data = training_set)
View(lin_reg)
summary(lin_reg)


#Visualization: Using GGPLOT - Training_set

library(ggplot2)
ggplot(data = training_set, aes(x = hp+wt+am, y = mpg)) +
  geom_point(col = 'blue') +
  geom_line(aes(x = training_set$hp+wt+am, y = predict(lin_reg, new_data = training_set)), col = 'red') +
  ggtitle("MPG Vs Hp+Wt+am (Training_Set)")

#Visualization: Using GGPLOT - Test_set

test_set$y_pred <- predict(lin_reg, newdata = test_set)
ggplot(data = test_set, aes(x = hp + wt + am, y = mpg)) +
  geom_point(col = 'blue') +
  geom_line(aes(x = hp + wt + am, y = y_pred), color = 'red') +
  ggtitle("MPG Vs Hp+Wt+am (Test_Set)")



#Model Evaluation: Predicting the test_set results

y_pred <- predict(lin_reg, newdata = test_set)
View(y_pred)
summary(y_pred)


#Comments:

#Model shows the relationship between the Independent variables (Predictors) and the dependent variable(Response Variable).
#Hp, Wt, am are the independent variables and mpg is the dependent variable.
#The negative coefficients of wt and hp shows that an increase in these variables will have an impact on MPG, which mean MPG decreases with the increase in both wt and hp.
#Positive coefficient of transmission says that the automatic cars are tend to be more fuel efficient than the manual transmission of cars,but the model suggest as it is not statistically significant, because the p-value is greater than 0.05.
#Multiple R-Squared is standing at 0.83 which means that the model explains 83% of the variability in MPG.
#Overall the above model is suggested to be a good fit and significant enough.
















