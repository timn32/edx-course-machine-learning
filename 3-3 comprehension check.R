# question 1
x <- matrix(rnorm(100*10), 100, 10)

# question 2
dim(x)
nrow(x) # nrow(x) or dim(x)[1] or length(x[,1])
ncol(x) # ncol(x) or dim(x)[2] or length(x[1,])


#question 3
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x),"+")

# question 4
x <- sweep(x, 2, 1:ncol(x), FUN = "+")

# question 5
rowMeans(x)
colMeans(x)

# question 6
x2 <- mnist$train$images
x2[x2<=50] <- 0
x2[x2 >= 205] <- 0
x2[x2>50 & x2< 205] <- 1

max(x2)
min(x2)
mean(x2)

# answer code
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")