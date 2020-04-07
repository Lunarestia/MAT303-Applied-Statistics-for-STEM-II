housing <- read.csv(file="housing.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Print the first six rows
print("head")
head(housing, 6)

plot(housing$sqft_living, housing$price, 
     main = "Scatterplot of price against living area in sq ft",
     xlab = "living area in sq ft", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$age, housing$price, 
     main = "Scatterplot of price against age of home",
     xlab = "age of home", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

myvars <- c("price","sqft_living", "age")
housing_subset <- housing[myvars]

# Print the correlation matrix
print("cor")
corr_matrix <- cor(housing_subset, method = "pearson")
round(corr_matrix, 4)


# Subsetting data to only include the variables that are needed
myvars <- c("price", "sqft_living", "grade", "bathrooms", "view")
housing_subset <- housing[myvars]

# Create the model
model1 <- lm(price ~ sqft_living + grade + bathrooms + view, data=housing_subset)
summary(model1)

# fitted values for model 1
fitted_values <- fitted.values(model1)

# residuals for model 1
residuals <- residuals(model1)

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(sqft_living=2150, grade=7, bathrooms=3, view='2')

# Prediction 1 for living area of 2,150sq ft, grade 7, 3 bathrooms and lake view
print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

newdata <- data.frame(sqft_living=2150, grade=7, bathrooms=3, view='0')

# Prediction 2 for living area of 2,150sq ft, grade 7, 3 bathrooms and road view
print("prediction interval")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

plot(housing$appliance_age, housing$price, 
     main = "Scatterplot of price against age of appliances",
     xlab = "age of appliances", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

plot(housing$crime, housing$price, 
     main = "Scatterplot of price against crime rate per 100,000 people",
     xlab = "crime rate", ylab = "price",
     col="red", 
     pch = 19, frame = FALSE)

# Subsetting data to only include the variables that are needed
print("Second Order Regression Model for Model 2")
myvars <- c("price", "appliance_age", "crime")
housing_subset <- housing[myvars]

# Create the model
model2 <- lm(price ~ appliance_age + crime + appliance_age:crime + I(appliance_age^2) + I(crime^2), data=housing_subset)
summary(model2)

# fitted values for model 2
fitted_values <- fitted.values(model2) 

# residuals for model 2
residuals <- residuals(model2)

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values for model 2",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

newdata <- data.frame(appliance_age=1, crime=81.02)

# Prediction for 1 year old appliances and a crime rate of 81.02 per 100,000 people
print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

newdata <- data.frame(appliance_age=15, crime=200.50)

# Prediction for 15 year old appliances and a crime rate of 200.50 per 100,000 people
print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int,4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int,4)

# Subsetting data to only include the variables that are needed
print("Nested Model for Model 2")
myvars <- c("price", "appliance_age", "crime")
housing_subset <- housing[myvars]

# Create the reduced model for model 2
model2_reduced <- lm(price ~ appliance_age + crime + appliance_age:crime, data=housing)
summary(model2_reduced)

# Perform the Nested Model F-test
anova(model2, model2_reduced)
