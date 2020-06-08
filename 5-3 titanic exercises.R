library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# question 1
str(titanic_clean)
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, 
                                  times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]

nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

# question 2
set.seed(3)
y_hat <- sample(c(0,1), length(test_index), replace = TRUE) 
class(y_hat)
mean(y_hat == test_set$Survived)

# Question 3a
class(train_set$Survived)
train_set %>%
  filter(Sex == "female") %>% 
  mutate(Survived = as.numeric(as.character(Survived))) %>%
  pull(Survived) %>% 
  mean()

train_set %>%
  filter(Sex == "male") %>% 
  mutate(Survived = as.numeric(as.character(Survived))) %>%
  pull(Survived) %>% 
  mean()

# Answer code
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

# Question 3b
y_hat_sex <- ifelse(test_set$Sex == "female", 1 , 0) 
mean(y_hat == test_set$Survived)

# Question 4a
titanic_clean %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived ==1))

# Question 4b
y_hat_class <- ifelse(test_set$Pclass == 1, 1, 0)
mean(y_hat == test_set$Survived)

# Question 4C
titanic_clean %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived ==1))

# Question 4D
y_hat_sex_class <- ifelse(test_set$Sex == "female" &
                  test_set$Pclass %in% c(1, 2), 1, 0)
mean(y_hat == test_set$Survived)

# question 5a
confusionMatrix(data = factor(y_hat_sex), reference = factor(test_set$Survived))
confusionMatrix(data = factor(y_hat_class), reference = factor(test_set$Survived))
confusionMatrix(data = factor(y_hat_sex_class), reference = factor(test_set$Survived))

# question 6
F_meas(data = factor(y_hat_sex), reference = factor(test_set$Survived))
F_meas(data = factor(y_hat_class), reference = factor(test_set$Survived))
F_meas(data = factor(y_hat_sex_class), reference = factor(test_set$Survived))


# Question 7
set.seed(1)
# x_test <- factor(test_set$Survived)
# y_test <- test_set$Fare
# dat_test <- data.frame(Survived=factor(x_test), Fare = y_test)) 

train_lda <- train(Survived~Fare, 
                   method = "lda", data = train_set)
y_hat_lda <- predict(train_lda, test_set)
confusionMatrix(data = y_hat_lda, 
                reference = test_set$Survived)$overall["Accuracy"]

set.seed(1)
train_qda <- train(Survived~Fare, 
                   method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set)
confusionMatrix(data = y_hat_qda, 
                reference = test_set$Survived)$overall["Accuracy"]


# Question 8
# Age predictor only
set.seed(1)
train_glm <- train(Survived~Age, method = "glm", 
                   data = train_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
confusionMatrix(y_hat_glm, test_set$Survived)$overall["Accuracy"]

# Age Sex Class Fare predictors
set.seed(1)
train_glm <- train(Survived~Age+Sex+Pclass+Fare, method = "glm", 
                   data = train_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
confusionMatrix(y_hat_glm, test_set$Survived)$overall["Accuracy"]

# all predictors
set.seed(1)
train_glm <- train(Survived~., method = "glm", 
                   data = train_set)
y_hat_glm <- predict(train_glm, test_set, type = "raw")
confusionMatrix(y_hat_glm, test_set$Survived)$overall["Accuracy"]

# Question 9
set.seed(6)
train_knn <- train(Survived~., method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   data = train_set)
train_knn$bestTune

# question 9b
ggplot(train_knn)

# question 9c
y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(y_hat_knn, test_set$Survived)$overall["Accuracy"]
# answer code used this method
mean(y_hat_knn == test_set$Survived)

# Question 10
control <- trainControl(method = "cv", number = 10, p = 0.1)
set.seed(8)
train_knn_10 <- train(Survived~., method = "knn",
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      data = train_set, 
                      trControl = control)
y_hat_knn_10 <- predict(train_knn_10, test_set, type = "raw")
train_knn_10$bestTune
confusionMatrix(y_hat_knn_10, test_set$Survived)$overall["Accuracy"]

# Question 11
set.seed(10)
train_rpart_11 <- train(Survived ~ ., method = "rpart", 
                        tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), 
                        data = train_set)
train_rpart_11$bestTune
y_hat_rpart_11 <- predict(train_rpart_11, test_set, type = "raw")
confusionMatrix(y_hat_rpart_11, test_set$Survived)$overall["Accuracy"]


# Question 11B
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
train_rpart_11$finalModel
plot(train_rpart_11$finalModel)
text(train_rpart_11$finalModel, cex = 0.75)
fancyRpartPlot(train_rpart_11)

# question 11C
titanic_clean %>%
  group_by(Sex, Pclass, Age, Fare) %>%
  summarize(Survived = mean(Survived ==1)) %>%
  filter(Sex == "female", Pclass == 2)


# question 11C
titanic_clean %>%
  group_by(Sex, Pclass, Fare) %>%
  summarize(Survived = mean(Survived ==1)) %>%
  filter(Sex == "female", Pclass == 3, Fare >= 23.35)

# Question 12
set.seed(14)
train_rf_12 <- train(Survived ~ ., method = "rf", 
                     tuneGrid = data.frame(mtry = seq(1,7,1)), 
                     ntree = 100, data = train_set)
y_hat_rf_12 <- predict(train_rf_12, test_set, type = "raw")
train_rf_12$bestTune
confusionMatrix(y_hat_rf_12, test_set$Survived)$overall["Accuracy"]


imp <- varImp(train_rf_12)
imp 

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>%
  arrange(desc(importance))

# answer code
varImp(train_rf)    # first row
