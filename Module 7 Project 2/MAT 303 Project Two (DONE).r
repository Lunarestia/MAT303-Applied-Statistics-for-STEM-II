
install.packages("ResourceSelection")
install.packages("pROC")
install.packages("rpart.plot")

heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of variables")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

# Create the first model
print("Logistic regression model 1")
logit1 <- glm(target ~ age + trestbps + thalach, data = heart_data, family = "binomial")

summary(logit1)

library(ResourceSelection)

print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit1$y, fitted(logit1), g=50)
hl

# predict heart disease or no heart disease for the dataset using the model
default_model_data <- heart_data[c('age', 'trestbps', 'thalach')]
pred <- predict(logit1, newdata=default_model_data, type='response')

# if the predicted probability of heart disease is >=0.50 then predict heart disease (default='1'), otherwise predict no heart 
# disease (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# this creates the confusion matrix
conf.matrix <- table(heart_data$target, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(pROC)

labels <- heart_data$target
predictions = logit1$fitted.values

roc <- roc(labels ~ predictions)

# Print Area under the Curve (AUC)
print("Area Under the Curve (AUC)")
round(auc(roc),4)

# Print ROC Curve
print("ROC Curve")

# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

# Prediction of heart disease if age=50, resting blood pressure is 122, and max heart rate is 140
print("Prediction: age=50, trestbps=122, thalach=140")
newdata1 <- data.frame(age=50, trestbps=122, thalach=140)
round(predict(logit1, newdata1, type='response'), 4)

# Prediction of heart disease if age=50, resting blood pressure is 140, and max heart rate is 170
print("Prediction: age=50, trestbps=140, thalach=170")
newdata1 <- data.frame(age=50, trestbps=140, thalach=170)
round(predict(logit1, newdata1, type='response'), 4)

# Create the second model
logit2 <- glm(target ~ thalach + age + sex + exang + cp + I(age^2) + age:thalach, data = heart_data, family = "binomial")

summary(logit2)

library(ResourceSelection)

print("Hosmer-Lemeshow Goodness of Fit Test")
h2 = hoslem.test(logit2$y, fitted(logit2), g=50)
h2

# predict heart disease or no heart disease for the dataset using the model
default_model_data2 <- heart_data[c('thalach', 'age', 'sex', 'exang', 'cp')]
pred2 <- predict(logit2, newdata=default_model_data2, type='response')

# if the predicted probability of heart disease is >=0.50 then predict heart disease (default='1'), otherwise predict no heart 
# disease (default='0')
depvar_pred2 = as.factor(ifelse(pred2 >= 0.5, '1', '0'))

# this creates the confusion matrix
conf.matrix <- table(heart_data$target, depvar_pred2)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(pROC)

labels <- heart_data$target
predictions = logit2$fitted.values

roc <- roc(labels ~ predictions)

# Print Area under the Curve (AUC)
print("Area Under the Curve (AUC)")
round(auc(roc),4)

# Print ROC Curve
print("ROC Curve")

# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

# Prediction of heart disease if age=30, sex=male('1'), max heart rate=145, exang='1' and cp='0'
print("Prediction: age=30, sex='1', thalach=145, exang='1', cp='0'")
newdata2 <- data.frame(age=30, sex='1', thalach=145, exang='1', cp='0')
round(predict(logit2, newdata2, type='response'), 4)

# Prediction of heart disease if age=30, sex=male('1'), max heart rate=145, exang='0' and cp='1'
print("Prediction: age=30, sex='1', thalach=145, exang='0', cp='1'")
newdata2 <- data.frame(age=30, sex='1', thalach=145, exang='0', cp='1')
round(predict(logit2, newdata2, type='response'), 4)

set.seed(511038)

# partition the dataset into training and testing data
samp.size = floor(0.80*nrow(heart_data))

# training set
print("Number of rows for the Training set")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# testing set 
print("Number of rows for the Testing set")
test.data = heart_data[-train_ind,]
nrow(test.data)

library(randomForest)

# Checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=200, by=1)) {
    #print(i)
    
    trees <- c(trees, i)
    set.seed(511038)
    model_rf1 <- randomForest(target ~ age+sex+cp+trestbps+chol+restecg+exang+slope+ca, data=train.data, ntree = i)
    
    train.data.predict <- predict(model_rf1, train.data, type = "class")
    conf.matrix1 <- table(train.data$target, train.data.predict)
    train_error = 1-(sum(diag(conf.matrix1)))/sum(conf.matrix1)
    train <- c(train, train_error)
    
    test.data.predict <- predict(model_rf1, test.data, type = "class")
    conf.matrix2 <- table(test.data$target, test.data.predict)
    test_error = 1-(sum(diag(conf.matrix2)))/sum(conf.matrix2)
    test <- c(test, test_error)
}
 
plot(trees, train,type = "l",ylim=c(0,1),col = "red", xlab = "Number of Trees", ylab = "Classification Error")
lines(test, type = "l", col = "blue")
legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

set.seed(511038)

library(randomForest)
model_rf1 <- randomForest(target ~ age+sex+cp+trestbps+chol+restecg+exang+slope+ca, data=train.data, ntree = 20)

# Confusion Matrix
print("======================================================================================================================")
print('Confusion Matrix: TRAINING set based on Random Forest model built using 20 trees')
train.data.predict <- predict(model_rf1, train.data, type = "class")

# construct the confusion matrix
conf.matrix1 <- table(train.data$target, train.data.predict)[,c('0','1')]
rownames(conf.matrix1) <- paste("Actual", rownames(conf.matrix1), sep = ": ")
colnames(conf.matrix1) <- paste("Prediction", colnames(conf.matrix1), sep = ": ")

# print nicely formatted confusion matrix
format(conf.matrix1,justify="centre",digit=2)


print("======================================================================================================================")
print('Confusion Matrix: TESTING set based on Random Forest model built using 20 trees')
test.data.predict <- predict(model_rf1, test.data, type = "class")

# construct the confusion matrix
conf.matrix2 <- table(test.data$target, test.data.predict)[,c('0','1')]
rownames(conf.matrix2) <- paste("Actual", rownames(conf.matrix2), sep = ": ")
colnames(conf.matrix2) <- paste("Prediction", colnames(conf.matrix2), sep = ": ")

# print nicely formatted confusion matrix
format(conf.matrix2,justify="centre",digit=2)

library(randomForest)

# Root Mean Squared Error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}


# Checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=80, by=1)) {
    set.seed(511038)
    trees <- c(trees, i)
    model_rf2 <- randomForest(thalach ~ age+sex+cp+trestbps+chol+restecg+exang+slope+ca, data=train.data, ntree = i)
    
    pred <- predict(model_rf2, newdata=train.data, type='response')
    rmse_train <-  RMSE(pred, train.data$thalach)
    rmse_train
    train <- c(train, rmse_train)
    
    pred <- predict(model_rf2, newdata=test.data, type='response')
    rmse_test <-  RMSE(pred, test.data$thalach)
    test <- c(test, rmse_test)
}
 
plot(trees, train,type = "l",col = "red", ylim=c(0,50), xlab = "Number of Trees", ylab = "Root Mean Squared Error")
lines(test, type = "l", col = "blue")
legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

set.seed(511038)
model_rf2 <- randomForest(thalach ~ age+sex+cp+trestbps+chol+restecg+exang+slope+ca, data=train.data, ntree = 30)

# Root Mean Squared Error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}



print("======================================================================================================================")
print('Root Mean Squared Error: TRAINING set based on Random Forest model built using 25 tree')
pred <- predict(model_rf2, newdata=train.data, type='response')
round(RMSE(pred, train.data$thalach),4)


print("======================================================================================================================")
print('Root Mean Squared Error: TESTING set based on Random Forest model built using 25 tree')
pred <- predict(model_rf2, newdata=test.data, type='response')
round(RMSE(pred, test.data$thalach),4)
