# code from course (doesnt work)
polls_2008 %>%
   ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color = "red", span = 0.15, method.args = list(degree = 1))



# code from text book
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)

polls_2008 %>%
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method.args = list(degree=1))