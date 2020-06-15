# 6-3-2 Comprehension check matrix factorization
# simulate data
set.seed(1987)
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

str(y)
y[[1]]
y[[2]]
y


# Question 1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# Question 2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Question 3
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- sum(y^2)

# answer code
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

# queston 4
plot(ss_y)
plot(ss_yv)

# Question 5
# Answer code
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

# Question 6
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

pca_6 <- prcomp(y)
var_6 <- cumsum(pca_6$sdev[1:3]^2/sum(pca_6$sdev[1:3]^2))
var_6
# incorrect....answer code is
sum(s$d[1:3]^2) / sum(s$d^2)


# Question 7
identical(t(s$u %*% diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*")) # correct one
identical(s$u %*% t(diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s, FUN = "*"))

# Question 8
student_avg <- rowMeans(y)
UD <- sweep(s$u, 2, s$d, FUN = "*")
plot(data.frame(UD[,1],student_avg))

# answer code
plot(-s$u[,1]*s$d[1], rowMeans(y))

# Question 9
image(t(s$v))
# answer code
my_image(s$v)


# question 10
# answer code
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

# Question 11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Answer code
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

# Question 12
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Answer code
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

# Question 13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Answer code
y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))