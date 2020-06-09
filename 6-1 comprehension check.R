library(tidyverse)
library(dslabs)
library(caret)
library(randomForest)
library(Rborist)
library(matrixStats)

# question 1
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


# Question 2
fun <- function(x){
  predict(x, mnist_27$test)
  
}
pred <- sapply(fits, fun)
dim(pred)

# Answer code
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)


# Question 3
fun_3 <- function(x){
  confusionMatrix(factor(pred[,x]), factor(mnist_27$test$y))$overall["Accuracy"]}
result <- (sapply(seq(1,10), fun_3))
mean(result)

# Answer code
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)


# Question 4
fun_4 <- function(x){
  a <- mean(pred[x,]==7)
  ifelse(a > 0.5, 7, 2)
  }

y_hat_4 <- sapply(seq(1,200), fun_4)
y_hat_4

confusionMatrix(factor(y_hat_4), factor(mnist_27$test$y))$overall["Accuracy"]

# Answer code
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)


# question5 
tibble(models, result)
y_5 <- ifelse(result > 0.81, 1, 0)tibble(models, result)
tibble(models, result, y_5) %>% arrange(desc(y_5))

# Answer code
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]
ind

# Question 6
plot(fits$glm$finalModel$y)
mean

fits$glm$resample
fits[1]$resample
fits$glm$results["Accuracy"]
class(fits)
fits[1]


fun_6 <- function(x){
  a <- fits[[x]]
  b <- min(a$results["Accuracy"])
}
min_accuracies <- sapply(seq(1,10), fun_6)
mean(min_accuracies)
tibble(models, sapply(seq(1,10), fun_6))

# answer code
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# Question 7
dat_7 <- tibble(models, result) %>%
  mutate(result = round(result,1.5)) %>%  
  filter(result >= 0.78)
dat_7
pred_7 <- pred[,which(colnames(pred) %in% dat_7$models)]
pred_7
votes_7 <- rowMeans(pred_7 == 7)
y_hat_7 <- ifelse(votes_7 >= 0.5, 7, 2)
mean(y_hat_7 == mnist_27$test$y)

# Answer code
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
