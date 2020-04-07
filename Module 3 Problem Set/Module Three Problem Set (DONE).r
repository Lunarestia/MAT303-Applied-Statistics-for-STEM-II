
economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Print the first six rows
print("head")
head(economic, 6)

plot(economic$gdp, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and GDP",
     xlab = "GDP", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

plot(economic$unemployment, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and Unemployment",
     xlab = "Unemployment", ylab = "Wage Growth",
     col="red", 
     pch = 19, frame = FALSE)

# Create the second order regression model and print the statistics
model1 <- lm(wage_growth ~ gdp + I(gdp^2), data=economic)
summary(model1)

newdata <- data.frame(gdp=1.70)

print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Create the second order regression model and print the statistics
model2 <- lm(wage_growth ~ unemployment + gdp + unemployment:gdp + I(unemployment^2) + gdp + I(gdp^2) , data=economic)
summary(model2)

newdata <- data.frame(unemployment=8.09, gdp=1.70)

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Create the second order regression model and print the statistics
model3 <- lm(wage_growth ~ unemployment + economy + unemployment:economy + I(unemployment^2) + I(unemployment^2):economy, data=economic)
summary(model3)

newdata <- data.frame(unemployment=8.09, economy='recession')

print("prediction interval")
prediction_pred_int <- predict(model3, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model3, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Total number of rows in the data set
print("Number of rows")
nrow(economic)

# Total number of columns in the data set
print("Number of columns")
ncol(economic)

plot(economic$inflation, economic$wage_growth, 
     main = "Scatterplot of Wage Growth and Inflation",
     xlab = "Inflation", ylab = "Wage Growth",
     col="red", 
     pch = 19)

# Quadratic (Second Order) Model with One Quantitative Variable
print("Second Order regression model for model 1")
model1 <- lm(wage_growth ~ inflation + I(inflation^2), data=economic)
summary(model1)

newdata <- data.frame(inflation = 7.41)

# Prediction interval for model 1
print("prediction interval for model 1")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

# Prediction interval for model 1
print("confidence interval for model 1")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)

# Second Order regression model with two quantitative variables
print("Second Order regression model for model 2")
model2 <- lm(wage_growth ~ inflation + gdp + inflation:gdp + I(inflation^2) + I(gdp^2) , data=economic)
summary(model2)

newdata <- data.frame(inflation = 7.41, gdp = 9.59)

# Prediction interval for model 2
print("prediction interval for model 2")
prediction_pred_int <- predict(model2, newdata, interval = "predict", level = 0.95) 
round(prediction_pred_int, 4)

# Confidence interval for model 2
print("confidence interval for model 2")
prediction_conf_int <- predict(model2, newdata, interval = "confidence", level = 0.95) 
round(prediction_conf_int, 4)

# Second Order regression model with one quantitative and one qualitative variable 
print("Second Order regression model for model 3")
model3 <- lm(wage_growth ~ inflation + economy + inflation:economy + I(inflation^2) + I(inflation^2):economy, data = economic)
summary(model3)

newdata <- data.frame(inflation = 7.41, economy = 'no_recession')

# Prediction interval for model 3
print("prediction interval for model 3")
prediction_pred_int <- predict(model3, newdata, interval = "predict", level = 0.95) 
round(prediction_pred_int, 4)

# Confidence interval for model 3
print("confidence interval for model 3")
prediction_conf_int <- predict(model3, newdata, interval = "confidence", level = 0.95) 
round(prediction_conf_int, 4)
