P_tp_d <- 0.85
P_tp_h <- 1 - P_tp_d
P_tn_h <- 0.9
P_tn_d <- 1 - P_tn_h

P_d <- 0.02
P_h <- 1-P_d


# question 2 
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)

# question 3
mean(test[disease == 1]) * 0.02 / (1-mean(test))

(1-mean(test[disease == 1])) * 0.02 / (1-mean(test))

mean(disease[test == 0])

# question 4
mean(disease[test == 1])

# question 5
mean(disease[test == 1])/ mean(disease == 1)

# question 6
library(tidyverse)
library(dslabs)
data("heights")
# MISSING CODE
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  
qplot(height, p, data =.)

# question 7
ps <- seq(0, 1, 0.1)
heights %>% 
  # MISSING CODE
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# question 8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  # MISSING CODE
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  
  qplot(x, y, data =.)