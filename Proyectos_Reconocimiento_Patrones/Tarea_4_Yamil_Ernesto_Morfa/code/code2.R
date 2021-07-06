library('MVA')

data = heptathlon[1:7]
marca = heptathlon[8]

h = hclust(dist(data))
plot(cutree(h, k=5), ylim = c(0,5.5))
text(cutree(h, k=5),  rownames(data) , cex = 0.7, pos = 1, col = "red", srt=45 )



km = kmeans(data, 5)
plot(km$cluster, ylim = c(0,6))
text(km$cluster,  rownames(data) , cex = 0.7, pos = 1, col = "red", srt=45 )



lis = matrix(, nrow = 25)
for (i in 1:25){
  km = kmeans(data, i)
  lis[i] = km$tot.withinss
}
plot(1:25, lis, type="b", xlab="Número de Grupos",
     ylab="WCSS")
