
# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
credit_default <- within(credit_default, {
   default <- factor(default)
   sex <- factor(sex)
   education <- factor(education)
   marriage <- factor(marriage)
   assets <- factor(assets)
   missed_payment <- factor(missed_payment)
})

print("head")
head(credit_default, 6)

# Create the complete model
logit <- glm(default ~ credit_utilize + education , data = credit_default, family = "binomial")

summary(logit)

conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

library(ResourceSelection)


print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('education', 'credit_utilize')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(pROC)

labels <- credit_default$default
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction: education is high school (education='1'), credit utilization is 40% (credit_utilize=0.40)")
newdata1 <- data.frame(education="1", credit_utilize=0.40)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)

# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors 
credit_default <- within(credit_default, {
   default <- factor(default)
   sex <- factor(sex)
   education <- factor(education)
   marriage <- factor(marriage)
   assets <- factor(assets)
   missed_payment <- factor(missed_payment)
})

# First 6 observations
head(credit_default,6)

# number of columns
print("Number of Columns")
ncol(credit_default)

# number of rows
print("Number of rows")
nrow(credit_default)

# Create the complete model
print("Model 1")
logit1 <- glm(default ~ credit_utilize + missed_payment, data = credit_default, family = "binomial")
summary(logit1)

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('credit_utilize', 'missed_payment')]
pred <- predict(logit1, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0')
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 1
print("Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 1")
hl = hoslem.test(logit1$y, fitted(logit1), g=50)
hl

# Receiver Operating Characteristic (ROC) Curve
print("Receiver Operating Characteristic (ROC) Curve")
library(pROC)

labels <- credit_default$default
predictions = logit1$fitted.values

roc <- roc(labels ~ predictions)

# Print Area under the Curve (AUC)
print("Area Under the Curve (AUC)")
round(auc(roc),4)

# Print ROC Curve
print("ROC Curve")

# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

# Prediction of credit default if missed payments and credit utilization is 32%
print("Prediction: has missed payments (missed_payment='1'), credit utilization is 32% (credit_utilize=0.32)")
newdata1 <- data.frame(missed_payment="1", credit_utilize=0.32)
prediction1 <- predict(logit1, newdata1, type='response')
round(prediction1, 4)

# Prediction of credit default if no missed payments and credit utilization is 32%
print("Prediction: has not missed payments (missed_payment='0'), credit utilization is 32% (credit_utilize=0.32)")
newdata2 <- data.frame(missed_payment="0", credit_utilize=0.32)
prediction2 <- predict(logit1, newdata2, type='response')
round(prediction2, 4)

# Create the second model
print("Model 2")
logit2 <- glm(default ~ credit_utilize + assets + education, data = credit_default, family = "binomial")
summary(logit2)

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('credit_utilize', 'assets', 'education')]
pred <- predict(logit2, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0')
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": ")

# print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 2
print("Hosmer-Lemeshow Goodness of Fit (GOF) Test for Model 2")
library(ResourceSelection)
hl = hoslem.test(logit2$y, fitted(logit2), g=50)
hl

# Receiver Operating Characteristic (ROC) Curve
print("Receiver Operating Characteristic (ROC) Curve")
library(pROC)

labels <- credit_default$default
predictions = logit2$fitted.values

roc <- roc(labels ~ predictions)

# Print Area under the Curve (AUC)
print("Area Under the Curve (AUC)")
round(auc(roc),4)

# Print ROC Curve
print("ROC Curve")

# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

# Prediction for credit default if credit utilization is 43%, owns car and house and has high school diploma
print("Prediction: credit utilization is 43% (credit_utilize=0.43), owns a car and a house (assets='3'), high school educated (education='1')")
newdata3 <- data.frame(credit_utilize=0.43, assets='3', education='1')
prediction1 <- predict(logit2, newdata3, type='response')
round(prediction1, 4)

# Prediction for credit default if credit utilization is 43%, owns car and house and has post graduate education
print("Prediction: credit utilization is 43% (credit_utilize=0.43), owns a car and a house (assets='3'), postgraduate (education='3')")
newdata4 <- data.frame(credit_utilize=0.43, assets='3', education='3')
prediction2 <- predict(logit2, newdata4, type='response')
round(prediction2, 4)
