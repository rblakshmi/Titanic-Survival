prediction_titanic <- function(){
#reading the file
train <- read.csv("C:/Users/lakshmi1/Desktop/dataset titanic/train.csv" , stringsAsFactors = TRUE )
test <- read.csv("C:/Users/lakshmi1/Desktop/dataset titanic/test.csv" , stringsAsFactors = TRUE )

#convert survived as factor
train$Survived <- as.factor(train$Survived)

#creating survived column and assingning null to it
test$Survived <- NA

#combining the two dataset so that it'll be easier to predict missing age values
full_data <- rbind(train , test)

#info--missing values in train (only age)
#info--missing values in test - age(86),fare(1)

#calculating missing FARE value in test
test$Fare[is.na(test$Fare)] <- mean(full_data$Fare[!is.na(full_data$Fare)])

#predicting the missing values of age
#model
age_train <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = full_data[!is.na(full_data$Age),], method = "anova")
#prediction
train$Age[is.na(train$Age)] <- predict(age_train , train[is.na(train$Age) ,])
test$Age[is.na(test$Age)] <- predict(age_train , test[is.na(test$Age) ,])

#predicting su rvival by svm
ctrl <- trainControl(method = "repeatedcv", repeats = 10)
set.seed(5000)

#model for predicting age
mod <- train(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare, data=train, method = "svmLinear", trControl = ctrl)

#prediction
survival <- predict(mod , test)
result <- data.frame(test$PassengerId , test$Name , survival)
return(result)
}

