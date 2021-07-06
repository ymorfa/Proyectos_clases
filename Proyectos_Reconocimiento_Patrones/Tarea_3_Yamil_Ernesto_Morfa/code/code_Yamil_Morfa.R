library('pracma')
library('latex2exp')
library('tsne')

norm_vec_2 <- function(x){sum(x**2)} ## norma cuadrada de un vector

ones = function(n){ ## matriz de 1s
  one = matrix(,nrow = n, ncol = n)
  for (i in 1:n){
    for (j in i:n){
      one[i, j] = one[j, i] <- 1 
    }
  }
  return(one)
}

get_k_ij = function(x, y, sigma){ ## kernel
  return(exp(-norm_vec_2(x - y)/sigma))
}

get_K = function(X, sigma){
  n = dim(X)[1]
  K = matrix(,nrow = n, ncol = n )
  for (i in 1:n){
    for (j in i:n){
      K[i,j] = K[j,i] = get_k_ij(X[i,], X[j,], sigma)      
    }
  }
  return(K)
}

cent = function(A){
  n = dim(A)[1]
  C = diag(n) - (1/n)*(ones(n))
  Ac = C %*% A %*% C
  return(Ac)
}

get_KPCA = function(X, sigma, ncomp){
  K = get_K(X, sigma)
  Kc = cent(K)
  u = svd(Kc)$u
  return(t(u[,1:ncomp]))
}

### Creando Datos

n = 30
dom1 = linspace(-1,1,n)
dom2 = linspace(0,2,n)
imag1 = -dom1**2 + 1
imag2 =  (dom2 - 1)**2 - 0.5
plot(dom1, imag1, xlim = c(-1.5,2.5), ylim = c(-0.7, 1.2), col = 'red', xlab = 'x', ylab = 'y')
points(dom2, imag2, col = 'blue')

data1 = rbind(dom1, imag1)
data2 = rbind(dom2, imag2)
data = cbind(data1, data2)

rm(data1, data2, dom1, dom2, imag1, imag2)

X = t(data)%*%data
Xc = cent(X)
primc_fix = prcomp(Xc)$rotation[,1]
err = matrix(,nrow = 100)

proyec = (X %*% primc_fix)
plot(proyec[1:30], rep(1,30), col = 'red', xlab = 'PCA 1', ylab = ' ')
points(proyec[31:60], rep(1,30), col = 'blue')

ind = 1
for (i in linspace(0.01, 200, 100)){
  primc = get_KPCA(t(data), i, 1)
  err[ind] = sqrt(norm_vec_2(primc_fix - primc))
  ind = ind + 1
}
#rm(i)0
plot(linspace(0.01, 200, 100),err, xlab = TeX('$\\sigma$'), ylab='err')

c = 1
primc = get_KPCA(t(data), 0.01, 1)
plot((X %*% t(primc))[1:30], rep(0,30), col = 'red', xlab = 'KPCA', ylab = ' ', xlim = c(-8,12),ylim = c(0,6))
points((X %*% t(primc))[31:60], rep(0,30), col = 'blue')
text(x = -7,y = 0, labels = '0.01', col = 'red')
for (i in linspace(0.1,100,4)){
  primc = get_KPCA(t(data), i, 1)
  points((X %*% t(primc))[1:30], rep(c,30), col = 'red', xlab = 'KPCA', ylab = ' ')
  points((X %*% t(primc))[31:60], rep(c,30), col = 'blue')
  tx = as.character(i)
  text(-7,c, tx, col = 'red')
  c = c+1
}

points(proyec[1:30], rep(5,30), col = 'red')
points(proyec[31:60], rep(5,30), col = 'blue')
text(-7.5,5, 'PCA1', col = 'red')


tsne = tsne(X, k = 1, perplexity = 5, max_iter = 5000)
plot(tsne[1:30], rep(1,30), col = 'blue', xlim = c(-4000,4000))
points(tsne[31:60], rep(1,30), col = 'red')
