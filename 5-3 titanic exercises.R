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
