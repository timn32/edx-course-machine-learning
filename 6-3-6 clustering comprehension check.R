library(dslabs)
library(tidyverse)
library(caret)
data("tissue_gene_expression")

# question 1
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

# Question 2
h <- hclust(d)
str(h)
plot(h, cex = 0.65, xlab = h$labels )

# question 3
k <- kmeans(tissue_gene_expression$x, centers = 7, nstart = 25)
table(k$cluster, tissue_gene_expression$y)

# Question 4
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]


heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))