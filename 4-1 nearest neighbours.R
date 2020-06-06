# section 4-1-1 Distance

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

# the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

# distance between 2 numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

# compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

# compute the distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3, 1:3]

# visualise these distances
image(as.matrix(d))

# order the distance by labels
image(as.matrix(d)[order(y), order(y)])

# compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d)[order(y), order(y)])
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))


# section 4-1-3 Knn
library(caret)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)

# logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data = mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5,7,2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

# fit knn model
knn_fit <- knn3(y~., data = mnist_27$train)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)
knn_fit <- knn3(y~., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


# section 4-1-4 overtraining and oversmoothing
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# fit knn with k=1
knn_fit_1 <- knn3(y~., data = mnist_27$train, k=1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn_1, reference = mnist_27$train$y)$overall["Accuracy"]

# fit knn with k = 401
knn_fit_401 <- knn3(y~., data = mnist_27$train, k=401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn_401, reference = mnist_27$train$y)$overall["Accuracy"]

# pick the k in knn
ks <- seq(3, 251,2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y~., data=mnist_27$train, k=k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data=y_hat, reference=mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type="class")
  cm_test<- confusionMatrix(data=y_hat, reference=mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train=train_error, test=test_error)
})

# pick the k that maximises accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)

