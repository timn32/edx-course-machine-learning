# 3-3-1 matrices
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x<- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# 3-3-2 matrix notation
length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)

# 3-3-3 converting a vector to a matrix
my_vector <- 1:15

# fill the matrix by column
mat <- matrix(my_vector, 5,3)
mat

# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
grid
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[,28:1])


# 3-3-4 row and column summaries and apply
sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)
avgs
sds


# 3-3-5 Filtering columns based on summaries
library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

# extract columns and rows
x[ , c(351, 352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) >60]
dim(new_x)
class(x[,1])
dim(x[1,])

# preserve the matrix class
class(x[ ,1, drop = FALSE])
dim(x[ , 1, drop = FALSE])


# Indexing with Matrices and Binarizing the Data
# use logical operations with matrices
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# binarize the data using just matrix operations
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_x

# index with matrices
mat <- matrix(1:15, 5,3)
as.vector(mat)
qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5,3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5,3)
mat[mat > 6 & mat < 12] <- 0
mat

# binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1 # coercing to logicals and then converting to binary numbers


# 3-3-6 Vectorization for matrices and matrix algebra operations
# We can scale each row of a matrix using this line of code:
(x - rowMeans(x))/rowSds(x)

# to scale each column of a matrix we use this code
t(t(x) - colMeans(x))

# function sweep is simliar to apply
X_mean_0 <- sweep(x, 2, colMeans(x))

# divide by the standard deviation
x_standardized <- sweep(X_mean_0, 2, colSds(x), FUN = "/")