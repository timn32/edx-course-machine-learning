# note run codes in 6-2 first to ensure this works
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y=movielens$rating, times = 1, 
                                  p=0.2, list = FALSE)
train_set<- movielens[-test_index,]
test_set <- movielens[test_index,]

class(train_set)
class(test_set)
str(train_set)
str(test_set)
dim(train_set)
dim(test_set)

fun <- function(x,y){
  class_x = class(x)
  class_y = class(y)
  dim_x = dim(x)
  dim_y = dim(y)
  data_frame_x = data.frame(group = "x", class = class_x, dim_row = dim_x[1], dim_col = dim_x[2])
  data_frame_y = data.frame(group = "y", class = class_y, dim_row = dim_y[1], dim_col = dim_y[2])
  full_join(data_frame_x, data_frame_y)
  }
fun(test_set, train_set)

test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method="Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating)
movei_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating -mu))
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effects Model",
                                     RMSE=model_1_rmse))

user_avgs <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movei_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method = "Movie + User Effects Model",
                                     RMSE = model_2_rmse))

# 10 of the biggest mistakes
test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>%
  slice(1:10) %>%
  knitr::kable()

movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()

# 10 best by our rating
movie_avgs %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()

# 10 worst by our rating
movie_avgs %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()


# check how often the 10 best and worst were rated
train_set %>% dplyr::count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

train_set %>% dplyr::count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# compute regularised estimates using lambda = 3
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating-mu)/(n()+lambda), n_i = n())

# plot regularised estimates against least squares estimates
data_frame(original = movie_avgs$b_i, 
           regularized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) +
  geom_point(shape=1, alpha= 0.5)

# top 10 movies based on penalised estimates (bi and lambda)
train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# and the 10 worst based on penalised estiamtes (bi and lambda)
train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# check if results were improved
predicted_ratings <- test_set %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Regularized Movie Effects Model",
                                     RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

# Choosing the penaly terms (lambda)
lambdas <-  seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarise(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i + l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)
lambdas[which.min(rmses)]

# using cross validation to predict lambda
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Regularized Movie + User Effect Model",
                                     RMSE = min(rmses)))

rmse_results  %>% knitr::kable()