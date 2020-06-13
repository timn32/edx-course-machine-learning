library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Question 1
head(movielens)
movielens %>% 
  group_by(year) %>%
  summarise(no_ratings = sum(rating)/mean(rating)) %>%
  ggplot(aes(year, no_ratings)) +
  geom_point() +
  scale_y_sqrt() + 
  geom_label(aes(year))

movielens %>% 
  group_by(year) %>%
  summarise(no_ratings = sum(rating)/mean(rating)) %>%
  arrange(desc(no_ratings))
class(movielens$rating)  

# Answer code
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#question 2
class(movielens$year)
data <- movielens %>% 
  group_by(movieId) %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), title = title[1], 
            years = 2018 - first(year),avg_rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

# Answer code
movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarise(n = n(), years = 2018 - first(year),
            title = title[1], 
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

# Question 3
data %>% 
  mutate(rate_strata = factor(round(rate))) %>%
  group_by(rate_strata) %>%
  summarise(avg_rating = mean(avg_rating)) %>%
  plot()

# answer code
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# Question 5
movielens <- mutate(movielens, date = as_datetime(timestamp))
head(movielens)

# Question 6
class(movielens$date)
movielens %>%
  mutate(date = round_date(movielens$date, "week")) %>%
  group_by(date) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# Answer code
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


# Question 8
movielens %>% 
  group_by(genres) %>%
  summarise(avg_rating = mean(rating), n = n(), se = sd(rating)/sqrt(n)) %>%
  filter(n > 1000) %>%
  arrange(avg_rating)

# Answer code
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  