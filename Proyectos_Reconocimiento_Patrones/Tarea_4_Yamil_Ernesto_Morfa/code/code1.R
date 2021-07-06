## Datos 
n= 40

x1<-rnorm(n,-2,1.5)
y1<-rnorm(n,-2,1.5)
x2<-rnorm(n,2,1.5)
y2<-rnorm(n,2,1.5)
x<-c(x1,x2)
y<-c(y1,y2)
plot(x,y, col = c(rep(1,n), rep(2,n)), xlim = c(-11,11), ylim = c(-11,11), 
     pch=16,main='Datos')

xx = c(x, -10)
yy = c(y, 10)
plot(xx,yy, col = c(rep(1,n), rep(2,n), 'green'), xlim = c(-11,11), 
     ylim = c(-11,11), pch=16, main='Datos + Atípico')

rm(x1, y1, x2, y2)

h<-hclust(dist(cbind(x,y)))
plot(x,y,col=cutree(h,k=2), pch = 16, xlim = c(-11,11), ylim = c(-11,11),
     main = 'Hierachical Clustering')

hh<-hclust(dist(cbind(xx,yy)))
plot(xx,yy,col=cutree(h,k=2),pch = 16, xlim = c(-11,11), ylim = c(-11,11),
     main = 'Hierachical Clustering')

data<-cbind(x,y)
cl1 <- kmeans(data, 2)
plot(data, col = cl1$cluster,pch=16, xlim = c(-11,11), ylim = c(-11,11), 
     main = 'K-means')
points(cl1$centers, col = 1:2, pch = 8, cex=4)

center = cl1$centers
center[1,1] = -10
center[1,2] = 10
data<-cbind(xx,yy)
cl2 <- kmeans(data, center)
plot(data, col = cl2$cluster,pch=16, xlim = c(-11,11), ylim = c(-11,11),
     main = 'K-means')
points(cl2$centers, col = 1:2, pch = 8, cex=4)
