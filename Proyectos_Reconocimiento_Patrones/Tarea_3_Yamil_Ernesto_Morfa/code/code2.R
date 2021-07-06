library('RDRToolbox')  
library('tsne')
library('lle')

X = matrix(unlist(deport[1:8]), nrow = 55, ncol = 8)
labels = deport[9]

isomap = Isomap(X, dims = 2, k=5, mod = FALSE, verbose = TRUE)$dim2
lle = LLE(X, dim=2, k=5)
tsn = tsne(X, k = 2, perplexity = 5, initial_dims = 5, max_iter = 5000, epoch = 1000)
som = SOM(X)

plot(isomap[,1], isomap[,2], main='ISOMAP')
text(isomap[,1], isomap[,2], rownames(deport), cex = 0.6, pos = 4, col = "red" )

plot(lle[,1], lle[,2], main = 'LLE')
text(lle[,1], lle[,2], rownames(deport), cex = 0.6, pos = 4, col = "red" )

plot(tsn[,1], tsn[,2], main = 'T-SNE')
text(tsn[,1], tsn[,2], rownames(deport), cex = 0.6, pos = 4, col = "red" )

