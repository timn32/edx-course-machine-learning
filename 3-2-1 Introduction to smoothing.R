library(dslabs)
library(tidyverse)
data("polls_2008")

qplot(day, margin, data = polls_2008)