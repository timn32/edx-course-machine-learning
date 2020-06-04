library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# question 1
str(x)
str(dat)

prop_female_inclass <- dat %>%
  filter(type == "inclass") %>%
  select(sex) %>%
  summarise(mean = mean(sex == "Female"))
prop_female_inclass
  
prop_female_online <- dat %>%
  filter(type == "online") %>%
  select(sex) %>%
  summarise(mean = mean(sex == "Female"))
prop_female_online

# question 2
y_hat <- ifelse(dat$type == "inclass", "Female", "Male")
mean(y_hat == dat$sex)

# answer code
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

# question 3
table(y_hat, y)

# question 4
sensitivity(data = y_hat, reference = y)

#answer code
library(caret)
sensitivity(y_hat, y)

# question 5
specificity(y_hat, y)

# question 6
mean(y == "Female")

# question 7
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

head(iris)
head(y)
class(y)
class(iris)

set.seed(2)    
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# question 8
cutoff <- seq(0,8, by = 0.1)

# sepal length
cutoff_s_length <- seq(4.9, 7.9, by = 0.1)
set.seed(2) 
accuracy_sepal_length <- map_dbl(cutoff_s_length, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_sepal_length)
best_cutoff_sepal_length <- cutoff_s_length[which.max(accuracy_sepal_length)]
best_cutoff_sepal_length

# sepal width
cutoff_s_width <- seq(2, 3.8, by = 0.1)
set.seed(2) 
accuracy_sepal_width <- map_dbl(cutoff_s_width, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_sepal_width)
best_cutoff_sepal_width <- cutoff_s_width[which.max(accuracy_sepal_width)]
best_cutoff_sepal_width


# petal length
cutoff_p_length <- seq(3, 6.9, by = 0.1)
set.seed(2) 
accuracy_petal_length <- map_dbl(cutoff_p_length, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_petal_length)
best_cutoff_petal_length <- cutoff_p_length[which.max(accuracy_petal_length)]
best_cutoff_petal_length

# petal width
cutoff_p_width <- seq(1, 2.5, by = 0.1)
set.seed(2) 
accuracy_petal_width <- map_dbl(cutoff_p_width, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy_petal_width)
best_cutoff_petal_width <- cutoff_p_width[which.max(accuracy_petal_width)]
best_cutoff_petal_width

# answer code
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

# question 9
y_hat <- ifelse(test$Petal.Length > best_cutoff_petal_length, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

# answer code
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# question 10

foo_10 <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions_10 <- apply(train[,-5],2,foo_10)
sapply(predictions_10,max)	


# question 11

library(caret)
data(iris)
iris <- iris[-which(iris$Species == 'setosa'), ]
y <- iris$Species
plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index, ]
train <- iris[-test_index,]

petal_length_range <- seq(range(train$Petal.Length)[1], 
                          range(train$Petal.Length)[2], by = 0.1)

petal_width_range <- seq(range(train$Petal.Width)[1], 
                          range(train$Petal.Width)[2], by = 0.1)

length_prediction <- sapply(petal_length_range, function(i){
  y_hat <- ifelse(train$Petal.Length > i, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
length_cutoff <- petal_length_range[which.max(length_prediction)]
length_cutoff

width_prediction <- sapply(petal_width_range, function(i){
  y_hat <- ifelse(train$Petal.Width > i, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
width_cutoff <- petal_width_range[which.max(width_prediction)]
width_cutoff

y_hat <- ifelse(test$Petal.Length > length_cutoff |
                  test$Petal.Width > width_cutoff, "virginica", "versicolor")
mean(y_hat == test$Species)