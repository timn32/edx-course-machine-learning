# part 1
library(tidyverse)
library(caret)
library(dslabs)

# Question 1
set.seed(1996) #if you are using R 3.5 or earlier
#set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results
fit

# question 2
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

str(tt)
pvals <- tt$p.value

# Question 3
ind <- pvals[pvals<0.01]
ind
str(ind)

# answer code
ind <- which(pvals <= 0.01)
length(ind)

# Question 4
x_subset <- as.matrix(which(pvals <= 0.01))
colnames(x_subset) <- paste("x")
head(x_subset)
class(x_subset)
str(x_subset)
str(tt$p.value)

x_subset_1 <- x[,ind]
colnames(x_subset_1) <- paste("x", 1:ncol(x_subset_1), sep = "_")
str(x_subset_1)
fit_1 <- train(x_subset_1, y, method = "glm")
fit_1$results

# Question 5
k = seq(101, 301, 25)
fit_5 <- train(x_subset_1, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit_5)

# Question 7
data("tissue_gene_expression")
fit_7 <- train(tissue_gene_expression$x, tissue_gene_expression$y,
               method = "knn", tuneGrid = data.frame(k=seq(1,7,2)))
ggplot(fit_7)
fit_7$results

# Answer code
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results


# part 2
#  Question 1
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995) # if R 3.6 or later, set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

str(indexes)
which(indexes$Resample01 == 3)
which(indexes$Resample01 == 4)
which(indexes$Resample01 == 7)

# answer code
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

# Question 2
sum(indexes[[1]] == 3) +
sum(indexes[[2]] == 3) +
sum(indexes[[3]] == 3) + 
sum(indexes[[4]] == 3) +
sum(indexes[[5]] == 3) +
sum(indexes[[6]] == 3) +
sum(indexes[[7]] == 3) +
sum(indexes[[8]] == 3) +
sum(indexes[[9]] == 3) +
sum(indexes[[10]] == 3)

str(indexes)

# answer code
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

# Question 3
y <- rnorm(100, 0, 1)
head(y)
str(y)
qnorm(0.75)
quantile(y, 0.75)

B <- 10000
set.seed(1)
M <- replicate(B, {
  y <- rnorm(100,0,1)
  quantile(y, 0.75)
})
mean(M)
sd(M) 

# Question 4
set.seed(1)
y <- rnorm(100, 0, 1)

B <- 10
set.seed(1)
M_star <- replicate(B, {
  y_star <- sample(y, 100, replace = TRUE) 
  quantile(y_star, 0.75)
})

mean(M_star)
sd(M_star)

# Answer code
set.seed(1)    # set.seed(1, sample.kind="Rounding") if R 3.6 or later
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)


# Question 5
B <- 10000
set.seed(1)
M_star <- replicate(B, {
  y_star <- sample(y, 100, replace = TRUE) 
  quantile(y_star, 0.75)
})

mean(M_star)
sd(M_star)

# Answer code
set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)


# Question 6
