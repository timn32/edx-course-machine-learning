str(read_mnist())
mnist$images
mnist <- read_mnist()
str(mnist$train)
str(mnist$test)
ncol(mnist$train$images)