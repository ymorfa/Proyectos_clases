## Creando Datos
n = 40
x1<-rnorm(n,-5,2.5)
y1<-rnorm(n,-5,2.5)

x2<-rnorm(n,5,2.5)
y2<-rnorm(n,5,2.5)

x3 = rnorm(n, 0, 2.5)
y3 = rnorm(n, 0, 2.5)

x<-c(x1,x2, x3)
y<-c(y1,y2, y3)

plot(x,y, col = c(rep(1,n), rep(2,n), rep(3, n)), xlim = c(-10,10), ylim = c(-10,10), 
     pch=16,main='Datos')
rm(x1, y1, x2, y2, x3, y3)

X = cbind(x, y)
rm(x, y, n)

## Funtions

Gaussian = function(x, mu, sigma){
  d = length(x)
  
  coef = 1/(((2*pi)^d)*(det(sigma))^(1/2))
  ex = (-1/2)*t(x - mu)%*%(solve(sigma)%*%(x-mu))
  return( coef*exp(ex))
}

posteriori = function(x, alpha, mu, sigma, k, K){
  aux = 0
  aux1 = alpha[k]*Gaussian(x, mu[k,], sigma[[k]])
  for (i in 1:K){
    aux = aux + alpha[i]*Gaussian(x, mu[i,], sigma[[i]])
  }
  return((aux1/aux)[1])
}


get_W = function(X, alpha, mu, sigma, K){
  N = dim(X)[1]
  d = dim(X)[2]
  
  W = matrix(, nrow = N, ncol = K)
  for (i in 1:N){
    for (k in 1:K){
      W[i,k] = posteriori(X[i,], alpha, mu, sigma, k, K )
    }
  }
  return(W)
}

get_Nk = function(W){
  N = dim(W)[1]
  K = dim(W)[2]
  
  N_list = matrix(, nrow = K)
  for (k in 1:K){
    N_list[k] = sum(W[,k])
  }
  return(N_list)
}

get_alpha = function(N_list){
  K = length(N_list)
  alpha = matrix(, nrow = K)
  for (k in 1:K){
    alpha[k] = N_list[k]/(N)
  }
  return(alpha)
}

get_mu = function(X, W, N_list){
  N = dim(X)[1]
  d = dim(X)[2]
  K = dim(W)[2]
  
  mu = matrix(, nrow = K, ncol = d)
  for (k in 1:K){
    suma = 0
    for (i in 1:N){
      suma = suma + W[i,k]*X[i,]
    }
    mu[k, ] = (1/N_list[k])*suma
  }
  return(mu)
}

get_sigma = function(X, W, mu, N_list){
  N = dim(X)[1]
  d = dim(X)[2]
  K = dim(W)[2]
  
  sigma = replicate(K, diag(d), simplify=F)
  for (k in 1:K){
    suma = 0
    for (i in 1:N){
      suma = suma + W[i,k]*(X[i,] - mu[k,])%*%t(X[i,] - mu[k,])
    }
    sigma[[k]] = (1/N_list[k])*suma
  }
  return(sigma)
}

get_sigma_ = function(X, mu, K){
  N = dim(X)[1]
  d = dim(X)[2]

  sigma = replicate(K, diag(d), simplify=F)
  for (k in 1:K){
    suma = 0
    for (i in 1:N){
      suma = suma + (X[i,] - mu[k,])%*%t(X[i,] - mu[k,])
    }
    sigma[[k]] = (1/(N-1))*suma
  }
  return(sigma)
}

log_lkehood = function(X, alpha, mu, sigma, K){
  N = dim(X)[1]
  d = dim(X)[2]
  suma1 = 0
  for (i in 1:N){
    suma = 0
    for (k in 1:K){
      suma = suma + alpha[k]*Gaussian(X[i,], mu[k,], sigma[[k]])
    }
    suma1 = suma1 + log(suma)
  }  
  return(suma1)
}
norm_vec <- function(x) sqrt(sum(x^2))


get_plot = function(X,ver, strs){
  N = dim(ver[[1]])[1]
  K = dim(ver[[1]])[2]
  
  plot(X[,1], X[,2], xlim = c(-10,10), ylim = c(-10,10), main = strs)
  for (i in 1:N){ 
    color = which(ver[[1]][i,] == max(ver[[1]][i,]))
    points(X[i,1], X[i,2], col = color, pch = 16) 
  } 
}

## EM

EM = function(X, K, n_iters, epsilon, mu){
  
  ## inicializar parámetros
  alpha = rep(1/K, K)
  
  if (is.null(mu)){
    
    
    mu = matrix(, nrow = K, ncol = d)
    for (i in 1:K){
      mu[i,] = X[sample(1:N, 1),]
    }
    
    sigma = replicate(K, diag(d), simplify = FALSE)
    #sigma = get_sigma_(X, mu, K)
    
  } else {
    sigma = get_sigma_(X, mu, K)
  }
  ## Globales
  N = dim(X)[1]
  d = dim(X)[2]
  
  
  loglikehood = log_lkehood(X, alpha, mu, sigma, K)
  
  for (iters in 1:n_iters){
    ## E-step
    W = get_W(X, alpha, mu, sigma, K )
    ## M-step
    N_list = get_Nk(W)
    alpha_new = get_alpha(N_list)
    mu_new = get_mu(X, W, N_list)
    sigma_new = get_sigma(X,W,mu_new, N_list)
    
    loglikehood_new = log_lkehood(X, alpha_new, mu_new, sigma_new, K)
    
    if (abs(loglikehood - loglikehood_new)[1] <= epsilon){
      print('Stop por convergencia')
      break
    }
    
    loglikehood = loglikehood_new
    alpha = alpha_new
    mu = mu_new
    sigma = sigma_new
    
  }
  if (iters == n_iters){
    print('Stop por número de iteraciones')
  }
  return(list(W, mu, sigma))
}

N = dim(X)[1]
d = dim(X)[2]
ver = EM(X,3,10, 0.00001, NULL)
get_plot(X, ver, 'EM')


kmean = kmeans(X, 3)
plot(X[,1], X[,2], col = kmean$cluster,  xlim = c(-10,10), ylim = c(-10,10), pch = 16, main = 'K-means' )

ver = EM(X,3,10, 0.00001, kmean$centers)
get_plot(X, ver, 'EM K-Means')
