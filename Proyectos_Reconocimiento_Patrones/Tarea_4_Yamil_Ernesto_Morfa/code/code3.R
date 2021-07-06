data = OxCGRT_latest[order(OxCGRT_latest$V6),]
data = subset(data, data$V6==20210101)


X=cbind(data$V2, as.numeric(as.character(data$V7)),as.numeric(as.character(data$V9)),as.numeric(as.character(data$V11)),as.numeric(as.character(data$V13)),as.numeric(as.character(data$V15)),as.numeric(as.character(data$V17)),as.numeric(as.character(data$V19)),as.numeric(as.character(data$V21)))

for (i in 2:9) {
  X=subset(X, !is.na(X[,i]))
  
}

colors = unclass(as.factor(X[,1]))

h = hclust(dist(X[,2:9]))
plot(cutree(h, k=6), col = colors, ylim = c(0,7))
text(cutree(h, k = 6), X[,1], srt = 45, pos =  1)


km = kmeans(X[,2:9], 6)
plot(km$cluster, col = colors, ylim = c(0,7))
text(km$cluster, X[,1], srt = 45, pos =  1)



#lis = matrix(, nrow = 100)
#for (i in 1:100){
#  km = kmeans(X[,2:9], i)
#  lis[i] = km$tot.withinss
#}
#plot(1:100, lis, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")
#points(15, lis[15], col = 'red')
