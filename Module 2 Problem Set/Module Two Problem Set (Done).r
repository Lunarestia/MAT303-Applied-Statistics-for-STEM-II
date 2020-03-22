
# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})


# Print the first six rows
print("head")
head(mtcars2, 6)

myvars <- c("mpg","wt","drat")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the multiple regression model and print summary statistics. Note that this model includes the interaction term. 
model1 <- lm(mpg ~ wt + drat + wt:drat, data=mtcars_subset)
summary(model1)

# Subsetting data to only include the variables that are needed
myvars <- c("mpg","wt","drat","am")
mtcars_subset <- mtcars2[myvars]

# Create the model
model2 <- lm(mpg ~ wt + drat + wt:drat + am, data=mtcars_subset)
summary(model2)

# predicted values
print("fitted")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_90_int <- confint(model2, level=0.90) 
round(conf_90_int, 4)

newdata <- data.frame(wt=3.88, drat=3.05, am='1')

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})

# Subsetting data to only include the variables that are needed for model 1
myvars <- c("mpg","wt","drat","hp")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)

# Print the correlation matrix
print("correlation matrix for mpg, wt, drat & hp")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the model for weight, gear ratio, horsepower, interaction of weight & gear ratio, and interaction of weight & horsepower 
model1 <- lm(mpg ~ wt + drat + hp + wt:drat + wt:hp, data=mtcars_subset)
summary(model1)

# predicted values for model 1 variables
print("fitted values for model 1")
fitted_values <- fitted.values(model1) 
fitted_values

# residuals for model 1 variables
print("residuals for model 1")
residuals <- residuals(model1)
residuals

# residuals against fitted values for model 1 variables
plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values for model 1",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19)

# Q-Q plot for model 1
qqnorm(residuals, pch = 19, col="red")
qqline(residuals, col = "blue", lwd = 2)

# predictions with hypothetical values for model 1 variables
newdata <- data.frame(wt=2.965, drat=2.91, hp=210)

print("prediction interval for model 1 hypothetical values")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("confidence interval for model 1 hypothetical values")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)

# Subsetting data to only include the variables that are needed for model 2
myvars <- c("mpg","wt","cyl","hp", "cyl")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)

# Create the model for weight, horsepower, interaction of weight & horsepower, and number of cylinders
model2 <- lm(mpg ~ wt + hp + wt:hp + cyl, data=mtcars_subset)
summary(model2)

# predicted values for model 2 variables
print("fitted values for model 2")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals for model 2 variables
print("residuals for model 2")
residuals <- residuals(model2)
residuals

# residuals against fitted values for model 2 variables
plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values Model 2",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19)

# Q-Q plot for model 2
qqnorm(residuals, pch = 19, col="blue")
qqline(residuals, col = "red", lwd = 2)

# predictions with hypothetical values for model 2 variables
newdata <- data.frame(wt=2.965, cyl='6', hp=210)

print("prediction interval for model 2 hypothetical values")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("confidence interval for model 2 hypothetical values")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)
