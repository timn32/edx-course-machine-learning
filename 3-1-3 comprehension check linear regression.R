# question 1
set.seed(1) 
#set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#set.seed(1)
set.seed(1, sample.kind="Rounding")

myfunction <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
  rmse
})

mean(myfunction)
sd(myfunction)


#question 2
set.seed(1, sample.kind = "Rounding")
n <- c(100, 500, 1000, 5000, 10000)
fun <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    rmse <- sqrt(mean((y_hat - test_set$y)^2))
  })
  rbind(n, mean(rmse), sd(rmse))
}

result <- sapply(n, fun)
result


# answer code
set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

 
#question 4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#set.seed(1)
set.seed(1, sample.kind="Rounding")

myfunction4 <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  rmse <- sqrt(mean((y_hat - test_set$y)^2))
  rmse
})

mean(myfunction4)
sd(myfunction4)


# question 6
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)
library(caret)

set.seed(1)
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
# only x_1
  fit_x1 <- lm(y ~ x_1, data = train_set)
  y_hat_x1 <- predict(fit_x1, test_set)
  rmse_x1 <- sqrt(mean((y_hat_x1 - test_set$y)^2))

# only x_2
  fit_x2 <- lm(y ~ x_2, data = train_set)
  y_hat_x2 <- predict(fit_x2, test_set)
  rmse_x2 <- sqrt(mean((y_hat_x2 - test_set$y)^2))

# x_1 and x_2
  fit_x1_x2 <- lm(y ~ x_1+x_2, data = train_set)
  y_hat_x1_x2 <- predict(fit_x1_x2, test_set)
  rmse_x1_x2 <- sqrt(mean((y_hat_x1_x2 - test_set$y)^2))

rmse_x1
rmse_x2
rmse_x1_x2


# question 8
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)
library(caret)

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# only x_1
fit_x1 <- lm(y ~ x_1, data = train_set)
y_hat_x1 <- predict(fit_x1, test_set)
rmse_x1 <- sqrt(mean((y_hat_x1 - test_set$y)^2))

# only x_2
fit_x2 <- lm(y ~ x_2, data = train_set)
y_hat_x2 <- predict(fit_x2, test_set)
rmse_x2 <- sqrt(mean((y_hat_x2 - test_set$y)^2))

# x_1 and x_2
fit_x1_x2 <- lm(y ~ x_1+x_2, data = train_set)
y_hat_x1_x2 <- predict(fit_x1_x2, test_set)
rmse_x1_x2 <- sqrt(mean((y_hat_x1_x2 - test_set$y)^2))

rmse_x1
rmse_x2
rmse_x1_x2
