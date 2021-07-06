
### Generando Normales
x1 = rnorm(30, 0, 1)
y1 = rnorm(30, 0, 1)
x2 = rnorm(29, 4, 1)
y2 = rnorm(29, 4, 1)

norm1 = cbind(x1, y1)
norm2 = cbind(x2, y2)

rm (x1, x2, y1, y2)

X = rbind(norm1, norm2)
label = c(rep(1, 30), rep(2, 29))
df = data.frame(X[,1], X[,2], label)


### Grid

k = 1
grid = matrix(, ncol = 2, nrow = 40401)
for (i in seq(-2,8, 0.05)){
  for (j in seq(-2,8,0.05)){
    grid[k,] = c(i, j)
    k = k + 1
  }
}
grid = matrix(grid, ncol = 2)

grid = data.frame(X...1. = grid[,1], X...2. = grid[,2])

rm(k,i, j)

### LDA

l = lda(label ~ ., data = df)
p = predict(l, grid)

plot(X[,1], X[,2], col = label, xlim = c(-2,8), ylim = c(-2,8), pch = 16, main = 'LDA' )
points(grid, col = p$class, pch = '.', cex = 0.5 )

print(l$means)
### atipical data

X = rbind(X, c(8,8))
label = c(rep(1, 30), rep(2, 30))
df = data.frame(X[,1], X[,2], label)

l = lda(label ~ ., data = df)
p = predict(l, grid)

plot(X[,1], X[,2], col = c(rep(1, 30), rep(2, 29), 3), xlim = c(-2,8), ylim = c(-2,8), pch = 16, main = 'LDA+ dato atípico' )
points(grid, col = p$class, pch = '.', cex = 0.5 )

print(l$means)
### LR

logit<-function(x, beta, alpha){return(1/(1+exp(-(beta%*%x + alpha) )))}

pred = function(grid, beta, alpha){
  pp = c()
  for (i in 1:40401){
    pp[i]<-rbinom(1,1,logit( c(grid$X...1.[i], grid$X...2.[i]), beta, alpha  ))
  }
  return(pp)
}

X = rbind(norm1, norm2)
label = c(rep(1, 30), rep(2, 29))
df = data.frame(X[,1], X[,2], label)
plot(X[,1], X[,2], col = label, xlim = c(-2,8), ylim = c(-2,8), pch = 16, main = 'LR' )


df$label = df$label - 1

lr <- glm(label ~.,data = df,  family=binomial(link=logit))

beta = lr$coefficients[2:3]
alpha = lr$coefficients[1]
p = pred(grid, beta, alpha)
points(grid, col = p + 1, pch = '.', cex = 0.5 )

print(lr$coefficients)
### atilical data

X = rbind(X, c(8,8))
label = c(rep(1, 30), rep(2, 30))
df = data.frame(X[,1], X[,2], label)
df$label = df$label - 1

lr <- glm(label ~.,data = df,  family=binomial(link=logit))

beta = lr$coefficients[2:3]
alpha = lr$coefficients[1]
p = pred(grid, beta, alpha)


plot(X[,1], X[,2], col = c(rep(1, 30), rep(2, 29), 3), xlim = c(-2,8), ylim = c(-2,8), pch = 16, main = 'LR + dato atípico' )
points(grid, col = p+1, pch = '.', cex = 0.5 )
print(lr$coefficients)

