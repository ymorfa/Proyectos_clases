
##### a

bayes_opt = function(x){
  x = as.matrix(x, ncol= 1)
  A = matrix(c(1, 0.4, 0.4, 1), ncol = 2)
  invA = solve(A)
  m1 = matrix(c(1,2), ncol = 1)
  w = t(m1)%*%invA
  w0 = t(m1) %*% invA %*% m1
  if (w %*% x < w0/2){
    l = 1
  } else {
    l = 2
  }
  return(l)
}

pred_bayes = function(test){
  y = c()
  for (i in 1:dim(test)[1]){
    y[i] = bayes_opt( c(test$x1[i], test$x2[i]) )
  }
  return(y)
}

##### b

norm_vec <- function(x) sqrt(sum(x^2))


get_bivn = function(n, rho, m){
  x1 = rnorm(n,0,1) # Normal estandar
  x2 = rnorm(n, 0, 1) # """"
  theta = acos(rho) ## angulo de rotación
  
  ## Gram–Schmidt
  x1_ = x1 / norm_vec(x1) ## Normalizando 
  proj = ((x1%*%x2)/(x1%*%x1))[1,1]*x1  ## Projección 
  x2 = x2 - proj ## Ortogonalizando
  x2_ = x2/ norm_vec(x2) ## Normalizando
  x2_ = x2_ + x1_*(1/tan(theta)) ## Rotando el vector 
  x1 = x1_ * norm_vec(x1) + rep(m[1], n) 
  x2 = x2_ * norm_vec(x2) + rep(m[2], n)
  return(data.frame(x1,x2))
}

for (n in c(50, 100, 500)){
  norm1 = get_bivn(n, 0.4, c(0,0))
  norm2 = get_bivn(n, 0.4, c(1,2))
  
  ## Plot
  xlim = c(min(norm1$x1), max(norm1$x1))
  ylim = c(min(norm1$x2), max(norm1$x2))
  plot(c(norm1$x1, norm2$x1), c(norm1$x2, norm2$x2), col = c(rep(1, n), rep(2, n)), asp = 0.5, 
       pch=16, cex = 0.5, xlab = 'x', ylab = 'y', main = n)
  lines(sort(norm1$x1), dnorm(sort(norm1$x1)) + ylim[1], col = 1)
  lines(dnorm(sort(norm1$x2))+xlim[1] , sort(norm1$x2), col=1)
  lines(dnorm(sort(norm2$x2),2,1)+xlim[1] , sort(norm2$x2), col=2)
  lines(sort(norm2$x1), dnorm(sort(norm2$x1),1,1) + ylim[1], col = 2)

}
rm(n, xlim, ylim, norm1, norm2)


##### c

library(rpart)
library(rpart.plot)

get_data = function(n){
  norm1 = get_bivn(n, 0.4, c(0,0))
  norm2 = get_bivn(n, 0.4, c(1,2))
  x1 = c(norm1$x1, norm2$x1)
  x2 = c(norm1$x2, norm2$x2)
  y=c(rep(1,n), rep(2,n))
  data <- data.frame(x1,x2,y)
  return(data)
}

pred = function(prob, y){
  y_ = c()
  for (i in 1:dim(prob)[1]){
    if (prob[i,1] > prob[i,2]){
      y_[i] = 1
    } else {
      y_[i] = 2
    }
  }
  acc = sum(y_==y)/length(y)
  return(acc)
}

data = get_data(200)
cp_set = (1/(2^seq(1, 20, 1)))
acc_ = c()
for (cp in cp_set){
  r1 <- rpart(y~.,data=data,method='class',cp = cp)
  #rpart.plot(r1, main=cp)
  #rpart.rules(r1)
  acc = pred(rpart.predict(r1), data$y)   # error sobre conjunto de entrenamiento
  acc_ = append(acc_, acc)
}


plot(cp_set, acc_, main = 'tree accuracy')
rpart.plot(r1, main=cp_set[15])
rpart.rules(r1)

acc_ = matrix(NA, nrow = 100, ncol = 2)
for (i in 1:100){
  data  = get_data(500)
  tree1 = rpart(y~.,data=data,method='class',cp = cp_set[20])
  #tree2 = rpart(y~.,data=data,method='class',cp = cp_set[5])
  acc_t1 = pred(rpart.predict(tree1), data$y)
  #acc_t2 = pred(rpart.predict(tree2), data$y)
  acc_bayes = sum(pred_bayes(data) == data$y)/1000
  acc_[i,] = c(acc_t1, acc_bayes)
}

plot(acc_[,1], main = 'Accuracy', col = 1, ylim = c(0.7,1))
lines(rep(mean(acc_[,1]), dim(acc_)[1], col = 'black'))
points(acc_[,2], col = 2)
lines(rep(mean(acc_[,2]), dim(acc_)[1]) , col = 'red')


full_cross_val_part = function(n,B){
  data  = get_data(n)
  position = matrix(sample(2*n, 2*n, replace = FALSE), nrow = B)
  test_sets = list()
  train_sets = list()
  for(i in 1:B){
    test_pos = position[i,]
    test_sets[[i]] = data[test_pos,]
    train_sets[[i]] = data[-test_pos,]
  }
  return( list(train_sets, test_sets) )
}

full_cross_val = function(cp, n, B){
  list_data = full_cross_val_part(n, B)
  list_train = list_data[[1]]
  list_test = list_data[[2]]
  set_acc_bayes = c()
  set_acc_tree = c()
  perc =  dim(list_test[[1]])[1]/(2*n)
  #print(perc )
  for (i in 1:B){
    train = list_train[[i]]
    test = list_test[[i]]
    
    ## Bayes 
    acc_bayes = sum(pred_bayes(test) == test$y)/length(test$y)
    set_acc_bayes = append(set_acc_bayes, acc_bayes)
    ## Tree
    tree1 = rpart(y~.,data=train,method='class',cp = cp)
    acc_tree = pred(predict(tree1, test), test$y)
    set_acc_tree = append(set_acc_tree, acc_tree)
  }
  return( list(set_acc_bayes, set_acc_tree, perc))
}

n = 1680
acc_bayes = c()
acc_tree = c()
perc = c()
for (i in c(rep(2,5), rep(3, 5), rep(4, 5),rep(5, 5), rep(6, 5),rep(7, 5),rep(8, 5),rep(10, 5))){
  aux = full_cross_val(cp_set[15], n, i )
  acc_bayes = append(acc_bayes, mean(aux[[1]]))
  acc_tree = append(acc_tree, mean(aux[[2]]))
  perc = append(perc, aux[[3]])
  
}

plot(perc, acc_bayes, col='red', ylim = c(0.78,0.85))
points(perc, acc_tree, col='black')
lines(perc, rep(mean(acc_bayes), length(acc_bayes)) , col = 'red')
lines(perc, rep(mean(acc_tree), length(acc_tree)) , col = 'black')


### d

data = get_data(200)
data$y = data$y -2
data$y[data$y == 0] = 1
data$y = as.factor(data$y)
library(kernlab)


svm = ksvm(y~., data = data, kernel='rbfdot')
acc_svm = sum(predict(svm, data) == data$y)/length(data$y)  

plot(svm, data = data)
### Grid

k = 1
grid = matrix(NA, ncol = 2, nrow = 29141)

for (i in seq(-4,4, 0.05)){
  for (j in seq(-3,6,0.05)){
    grid[k,] = c(i, j)
    k = k + 1
  }
}
grid = matrix(grid, ncol = 2)

grid = data.frame(x1 = grid[,1], x2 = grid[,2])

rm(k,i, j)
col = pred_bayes(grid)
plot(data$x1, data$x2, pch = c(rep(2,200), rep(1,200)), main = 'Bayes',
     xlab = 'x1', ylab = 'x2')#, col = c(rep(1,200), rep(2,200)) )
points(grid, col = col, pch = '.', cex = 0.5 )

full_cross_val = function(n, B){
  list_data = full_cross_val_part(n, B)
  list_train = list_data[[1]]
  list_test = list_data[[2]]
  set_acc_bayes = c()
  set_acc_svm = c()
  perc =  dim(list_test[[1]])[1]/(2*n)
  #print(perc )
  for (i in 1:B){
    train = list_train[[i]]
    train$y = as.factor(train$y)
    test = list_test[[i]]
    
    ## Bayes 
    acc_bayes = sum(pred_bayes(test) == test$y)/length(test$y)
    set_acc_bayes = append(set_acc_bayes, acc_bayes)
    ## svm
    svm = ksvm(y~.,data=train,kernael = 'rbfdot')
    acc_svm =  sum(predict(svm, test) == test$y)/length(test$y)
    set_acc_svm = append(set_acc_svm, acc_svm)
  }
  return( list(set_acc_bayes, set_acc_svm, perc))
}

n = 1680
acc_bayes = c()
acc_svm = c()
perc = c()
for (i in c(rep(2,5), rep(3, 5), rep(4, 5),rep(5, 5), rep(6, 5),rep(7, 5),rep(8, 5),rep(10, 5))){
  aux = full_cross_val( n, i )
  acc_bayes = append(acc_bayes, mean(aux[[1]]))
  acc_svm = append(acc_svm, mean(aux[[2]]))
  perc = append(perc, aux[[3]])
  
}

plot(perc, acc_bayes, col='red', ylim = c(0.78,0.85))
points(perc, acc_svm, col='black')
lines(perc, rep(mean(acc_bayes), length(acc_bayes)) , col = 'red')
lines(perc, rep(mean(acc_svm), length(acc_svm)) , col = 'black')

