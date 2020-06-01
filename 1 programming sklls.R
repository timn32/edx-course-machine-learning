library(dslabs)
data(heights)
heights

# question 1
class(heights)
str(heights)
class(heights$sex)
class(heights$height)
class(75.00000)

# question 2
nrow(heights)

# question 3
heights$height[777]

# question 5
max(heights$height)
which.min(heights$height)

# quetion 6
summary(heights$height)

# question 7
mean(heights$sex == "Male")

# question 8
sum(heights$height >78)

# question 9
sum(heights$sex == "Female" & heights$height > 78)