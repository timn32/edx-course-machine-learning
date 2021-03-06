# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))

prev <- mean(y == "Male")
prev

confusionMatrix(data = y_hat, reference = test_set$sex)