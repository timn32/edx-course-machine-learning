# question 1
library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)
class(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
head(d)

# question 2

# answer code
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# question 3
image(as.matrix(d))


# part 2 of comprehension check
# question 1
library(caret)
library(dslabs)
data("heights")

x<- heights$sex
y <- heights$height

set.seed(1)
test_ind <- createDataPartition(y, times = 1, p=0.5, list = FALSE)
train_set <- heights %>% slice(-test_ind)
test_set <- heights %>% slice(test_ind)

ks <- seq(1, 101, 3)

F_1 <- sapply(ks, function(k){
  knn_fit <- knn3(sex~height, data = train_set, k=k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class")
  F_1 <- F_meas(data = y_hat_knn, reference = factor(test_set$sex))
tibble(k, F_1)
  })
max(F_1[2])
ks[which.max(F_1$F_1)]
F_1

# Answer code
library(dslabs)
library(tidyverse)
library(caret)
data("heights")

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]


# question 2
library(dslabs)
library(caret)
library(magrittr)
library(purrr)
library(dplyr)
library(tidyverse)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
df <- data.frame(x=tissue_gene_expression$x, y =tissue_gene_expression$y)

set.seed(1)
test_index <- createDataPartition(df$y, times = 1, p = 0.5, list = FALSE)
test_set <- df %>% slice(test_index)
train_set <- df %>% slice(-test_index)

ks <- c(1,3,5,7,9,11)

accuracy <- map_df(ks, function(k){
  fit_train <- knn3(y~., data = train_set, k = k)
  y_hat <- predict(fit_train, train_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train_set$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit_train, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k, train_error, test_error)
})
accuracy


# answer code
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})