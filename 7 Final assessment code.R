options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# Question 1
str(brca)
levels(brca$x)
head(brca$x)
head(brca$y)
mean(brca$y == "M")

means_1 <- colMeans(brca$x)
max_mean_1 <- max(means_1)
str(means_1)
which(as.numeric(means_1) == max_mean_1)
sd_1 <- colSds(brca$x)
min_sd_1 <- min(sd_1)
which(as.numeric(sd_1) == min_sd_1)

# Answer code
dim(brca$x)[1]
dim(brca$x)[2]
mean(brca$y == "M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))


# Question 2
x_centred <- sweep(brca$x, 2, colMeans(brca$x), FUN = "-")
x_scaled <- sweep(a, 2, colSds(a), FUN = "/")
head(x_scaled)
str(x_scaled)
class(x_scaled)
sd(x_scaled[,1])
median(x_scaled[,1])


# Question 3
ind_B <- which(brca$y == "B")
ind_M <- which(brca$y == "M")


d_1 <- x_scaled
d_1 <- dist(d_1)
d_1 <- as.matrix(d_1)
mean(d_1[1,2:357])

str(d_1[,1])
class(d_1)
str(d_1)
length(d_1)
dim(d_1)

mean(d_1[1,358:569])


d <- as.matrix(dist(x_scaled))
mean(d[1,2:357])
mean(d[1,358:569])

# Answer code
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
str(d_samples)
str(dist_BtoB)
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)

# Question 4
heatmap(as.matrix(dist(x_scaled)), 
        labRow = NA, labCol = NA)

# Answer code
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)


d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features))

# Question 5
h <- hclust((dist(t(x_scaled))))
groups <- cutree(h, k=5)
names(groups)[groups == 1]
names(groups)[groups == 2]
names(groups)[groups == 3]
names(groups)[groups == 4]
names(groups)[groups == 5]
split(names(groups), groups)


# Question 6
pca <- prcomp(x_scaled)
dim(pca$rotation)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
var_explained
which(var_explained >= 0.9)

# Answer code
summary(pca)     # first value of Cumulative Proportion that exceeds 0.9: PC7

# Question 7
str(pca)
plot(pca$x[ind_B,1:2])
plot(pca$x[ind_M,1:2])
pca %>% 
  ggplot(aes(x[,1], x[,2])) +
  geom_point()

B_PC1 <- mean(pca$x[ind_B,1])
B_PC2 <- mean(pca$x[ind_B,2])
M_PC1 <- mean(pca$x[ind_M,1])
M_PC2 <- mean(pca$x[ind_M,2])
tibble(B_PC1, B_PC2, M_PC1, M_PC2)

# Answer code
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

# Question 8
fun <- function(i){
  pca$x[,i]
}
dat <- data.frame(type = brca$y, fun(1:10))
head(dat)

dat %>% ggplot(aes(type, PC1)) + geom_boxplot()
dat %>% ggplot(aes(type, PC2)) + geom_boxplot()
dat %>% ggplot(aes(type, PC3)) + geom_boxplot()
dat %>% ggplot(aes(type, PC4)) + geom_boxplot()
dat %>% ggplot(aes(type, PC5)) + geom_boxplot()
dat %>% ggplot(aes(type, PC6)) + geom_boxplot()
dat %>% ggplot(aes(type, PC7)) + geom_boxplot()
dat %>% ggplot(aes(type, PC8)) + geom_boxplot()
dat %>% ggplot(aes(type, PC9)) + geom_boxplot()
dat %>% ggplot(aes(type, PC10)) + geom_boxplot()

# Answer code
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

# Question 9
set.seed(1) # if using R 3.5 or earlier
#set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y == "B")
mean(test_y == "B")

# Question 10
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

set.seed(3)

k <- kmeans(train_x, centers = 2)
pred <- predict_kmeans(test_x, k)
pred <- factor(ifelse(pred == 1, "B", "M"))
head(pred)
class(pred)
mean(test_y == pred)

# answer code
set.seed(3) # if using R 3.5 or earlier
#set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

# Question 10B
dat <- data.frame(test_y = test_y, pred = pred)
dat %>% filter(test_y == "B") %>% summarize(mean = mean(test_y==pred))
dat %>% filter(test_y == "M") %>% summarize(mean = mean(test_y==pred))

# Answer code
sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")


# Question 11
train_set <- data.frame(B_M = train_y, train_x) 
test_set <- data.frame(B_M = test_y, test_x)

train_glm <- train(B_M ~ ., method = "glm", data = train_set)
y_hat <- predict(train_glm, test_set, type = "raw")
confusionMatrix(y_hat, test_set$B_M)$overall[["Accuracy"]]


# Question 12
train_qda <- train(B_M ~., method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set, type = "raw")
confusionMatrix(y_hat_qda, test_set$B_M)$overall[["Accuracy"]]


train_lda <- train(B_M ~., method = "lda", data = train_set)
y_hat_lda <- predict(train_lda, test_set, type = "raw")
confusionMatrix(y_hat_lda, test_set$B_M)$overall[["Accuracy"]]

# Question 13
library(gam)
set.seed(5)
train_loess <- train(B_M ~ ., method = "gamLoess", data = train_set)
y_hat_loess <- predict(train_loess, test_set)
confusionMatrix(y_hat_loess, test_set$B_M)$overall[["Accuracy"]]

# Question 14
set.seed(7)
train_knn <- train(B_M ~ ., method = "knn", 
                   tuneGrid = data.frame(k = seq(3,21,2)), data = train_set)
max(train_knn$bestTune)
y_hat_knn <-predict(train_knn, test_set)
confusionMatrix(y_hat_knn, test_set$B_M)$overall[["Accuracy"]]


# Question 15
library(randomForest)
set.seed(9)
train_rf <- randomForest(B_M ~ ., data = train_set, 
                         mtry = c(3,5,7,9), importance = TRUE)
y_hat_rf <- predict(train_rf, test_set)
confusionMatrix(y_hat_rf, test_set$B_M)$overall[["Accuracy"]]
train_rf$mtry
max(train_rf$importance)
min(train_rf$importance)
train_rf$importance
str(train_rf)
varImp(train_rf)

# Question 16A
dat_16A <- data.frame(y_hat_knn, y_hat, y_hat_lda, y_hat_qda, 
                        y_hat_loess, y_hat_rf, y_hat_kmeans = factor(kmeans_preds))
head(dat_16A)
votes <- rowMeans(dat_16A == "B")
pred_16A <- ifelse(votes > 0.5 , "B", "M")
head(votes)
head(pred_16A)

mean(test_set$B_M == pred_16A)

# Answer code
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

# Question 16B
tibble(rf = confusionMatrix(y_hat_rf, test_set$B_M)$overall[["Accuracy"]],
       knn = confusionMatrix(y_hat_knn, test_set$B_M)$overall[["Accuracy"]],
       loess = confusionMatrix(y_hat_loess, test_set$B_M)$overall[["Accuracy"]],
       lda = confusionMatrix(y_hat_lda, test_set$B_M)$overall[["Accuracy"]],
       qda = confusionMatrix(y_hat, test_set$B_M)$overall[["Accuracy"]],
       glm = confusionMatrix(y_hat, test_set$B_M)$overall[["Accuracy"]],
       kmeans = mean(kmeans_preds == test_y), 
       ensemble = mean(test_set$B_M == pred_16A)) 

      
p
class(y_hat_knn)
class(kmeans_preds)
