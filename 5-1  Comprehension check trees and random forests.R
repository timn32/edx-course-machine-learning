library(dslabs)
library(tidyverse)
library(caret)

# Question 1
library(rpart)
n <- 1000
sigma <- 0.25
#set.seed(1) 
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

str(dat)
head(dat)

fit <- rpart(y ~ ., data = dat) 
fit

# Question 2
# visualise the splits
plot(fit, margin=0.1)
text(fit, cex = 0.75)


# Question 3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  #BLANK
  geom_step(aes(x, y_hat), col=2)

# Question 4
library(randomForest)
fit <- #BLANK
  randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
# Question 5
plot(fit)

# question 6
library(randomForest)
fit <- #BLANK
  randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  

# Comprehension Check part 2
# Question 1
library(dslabs)
data("tissue_gene_expression")

dat <- data.frame(x = tissue_gene_expression$x, y = tissue_gene_expression$y)
fit <- rpart(y ~ ., data = dat) 
fit
set.seed(1991)
train_rpart <- train(y~.,
                     method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), 
                     data = dat)
plot(train_rpart)


# Answer code
library(caret)
library(rpart)          
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)


# Question 2
set.seed(1991)
fit_rpart <- with(tissue_gene_expression, 
            train(x,y, method = "rpart", 
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), 
                  control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)


# Answer code
library(rpart)
set.seed(1991)
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)



# Question 3
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

# Question 4
set.seed(1991)
fit4 <- with(tissue_gene_expression, 
           train(x, y, method = "rf",
                 tuneGrid = data.frame(mtry = seq(50, 200, 25)), 
                                       nodesize =1))
ggplot(fit4)
max(fit4$results$Accuracy)
fit4$results[which.max(fit4$results$Accuracy)]
fit4$results
print(fit4)
fit4$bestTune

# Answer code
set.seed(1991)
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

# Question 5
imp <- varImp(fit4)
imp 

# Question 6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

imp$importance

# Answer code
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)