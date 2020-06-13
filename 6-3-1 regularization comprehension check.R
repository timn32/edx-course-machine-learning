library(dslabs)
library(tidyverse)
library(caret)
# setup code
options(digits = 7)

set.seed(1986) #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1986, sample.kind="Rounding")` instead
n <- round(2^rnorm(1000, 8, 1))

set.seed(1) #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1, sample.kind="Rounding")` instead
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# simulate student test scores
set.seed(1) #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1, sample.kind="Rounding")` instead
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
head(schools)

# Question 1
str(schools)
class(schools)
head(schools)

schools %>%
  arrange(desc(score)) %>%
  top_n(10, score)

# answer code
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# question 2
#overall
schools %>%
  summarise(median_size = median(size))
#top 10 schools
schools %>%
  top_n(10, score) %>%
  summarise(median_size = median(size))

# Question 3
schools %>%
  arrange(desc(score)) %>%
  top_n(-10, score) %>%
  summarise(median_size = median(size))

# Question 4
score_10th <- schools %>% arrange(desc(quality)) %>% select(score) %>% .[10,]

schools %>% 
  mutate(quality_top_10 = ifelse(quality >= score_10th,1,0)) %>%
  ggplot(aes(size, score)) + 
  geom_point()

# Answer code
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

# Question 5
overall <- mean(sapply(scores, mean))
alpha <- 25

fun <- function(x){
  sum(x - overall)/(length(x) + alpha)
  }

scores_reg <- sapply(scores, fun)
str(scores_reg)

schools_reg <- schools %>%
  bind_cols(scores_reg) %>%
  rename(b_i = ...6) %>%
  mutate(score_reg = overall + b_i)
str(schools_reg)

schools_reg %>%
  arrange(desc(score_reg)) %>%
  head(10)

# answer code (mine is more step by step, same thing)
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Question 6
alpha_6 <- seq(10, 250)

sum_only_6 <- sapply(scores, function(x){
  s_i <- sum(x - overall)
  s_i
})

no_scores_6 <- sapply(scores, function(x){
  n_i <- length(x)
})
head(no_scores_6)

sum_only_and_no <- bind_cols(sum_only_6, no_scores_6) %>%
  rename(s = ...1, n = ...2)
head(sum_only_and_no)

schools_reg_6 <- bind_cols(schools, sum_only_and_no) 
head(schools_reg_6)

fun_6 <- function(alpha) {
  predictions <- schools_reg_6 %>%
    mutate(b_i = s/(n+alpha)) %>%
    mutate(pred = overall + b_i)
  rmse <- sqrt(1/1000 * sum((predictions$quality - predictions$pred)^2))
  rmse
}
fun_6(25)
rmse <- sapply(alpha_6, fun_6)
bind_cols(alpha_6, rmse) %>%
  rename(alpha = ...1, rmse = ...2) %>%
  arrange(rmse) 

# Answer code
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# question 7 
alpha <- 135

scores_reg_7 <- sapply(scores, fun)
str(scores_reg_7)

schools_reg_7 <- schools %>%
  bind_cols(scores_reg_7) %>%
  rename(b_i = ...6) %>%
  mutate(score_reg = overall + b_i)
str(schools_reg_7)

schools_reg_7 %>%
  arrange(desc(score_reg)) %>%
  head(10)

# answer code
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


# Question 8

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
