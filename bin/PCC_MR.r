# 导入数据
exprMat<-args[1]
expr_data <- read.table(exprMat, header = TRUE, row.names = 1)

# 计算基因之间的相关系数
corr_matrix <- cor(expr_data, method = "pearson")

# 计算基因的平均排名
rank_data <- apply(expr_data, 1, rank)
avg_rank <- apply(rank_data, 2, mean)

# 计算MR值
mr_values <- apply(expr_data, 1, function(x) cor(x, avg_rank, method = "pearson"))

# 构建共表达网络
library(igraph)
network <- graph.adjacency(corr_matrix, mode = "undirected", weighted = TRUE)

# 设置阈值
threshold <- quantile(mr_values, 0.95)

# 删除低于阈值的边
delete_edges <- which(E(network)$weight < threshold)
network <- delete.edges(network, delete_edges)

# 可视化共表达网络
plot(network, vertex.label = NA, edge.width = E(network)$weight / mean(E(network)$weight))

