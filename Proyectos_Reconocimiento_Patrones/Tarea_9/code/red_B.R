
### Datos a usar ####

### Grid

X1 = seq(-2.1, 0.1, by = 0.02)
X2 = seq(-1.1, 2.2, by = 0.02)
grid_set = data.frame(expand.grid(X1, X2))

get_label = function(X){
  lab = matrix(NA, nrow = dim(X)[1], ncol = 1)
  
  for (i in 1:dim(X)[1]){
    x = X[i,1]
    y = X[i,2]
    
    if ( y<(-1) | y>2 | x >0 | x<(-2)){
      lab[i, 1] = 0
    } else {
      
      if ( y > x + 2 | y < (-(1/2)*x - 1)){
        lab[i,1] = 0
      } else {
        lab[i, 1] = 1
      }
    }
  }
  return( lab)
}

X = cbind(grid_set$Var1, grid_set$Var2)
Y = get_label(X)
plot(X[,1], X[,2], col = Y + 1, pch = '.', cex = .7, main = 'Datos')


sam = sample(2, length(Y), replace = TRUE, prob = c(0.7, 0.3))


X_train = X[sam == 1, ]
Y_train = Y[sam == 1]
X_test = X[sam == 2, ]
Y_test = Y[sam == 2]
dim(Y_train) <- c(length(Y_train), 1)
dim(Y_test) <- c(length(Y_test), 1)


plot(X_train[,1], X_train[,2], col = Y_train[,1] + 1, pch = '.', cex = .7, main = 'Tarin')
plot(X_test[,1], X_test[,2], col = Y_test[,1] + 1, pch = '.', cex = .7, main = 'Test')

rm(grid_set, X, X1, X2, sam, Y, get_label)


##### NNet ######

### Obteniendo dimension para inicializar parámetros

getLayerSize = function(X, y, hidden_neurons, train = TRUE){
  n_x = dim(X)[1]
  n_h = hidden_neurons
  n_y = dim(y)[1]
  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)
  return(size)
}

layer_size <- getLayerSize(t(X_train), t(Y_train), hidden_neurons = 4)

## Inicializar parámetros aleatorios

initializeParameters <- function(list_layer_size){
  
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  W1 <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = TRUE) * 0.01
  b1 <- matrix(rep(0, n_h), nrow = n_h)
  W2 <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = TRUE) * 0.01
  b2 <- matrix(rep(0, n_y), nrow = n_y)
  
  params <- list("W1" = W1,
                 "b1" = b1, 
                 "W2" = W2,
                 "b2" = b2)
  
  return (params)
}

init_params <- initializeParameters( layer_size)

### Funcion de activación ####

sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}

### Forward Propagation ###

forwardPropagation <- function(X, params, list_layer_size, loss){
  
  m <- dim(X)[1]
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  b1_new <- matrix(rep(b1, m), nrow = n_h)
  b2_new <- matrix(rep(b2, m), nrow = n_y)
  
  Z1 <- W1 %*% t(X) + b1_new
  A1 <- loss(Z1)
  Z2 <- W2 %*% A1 + b2_new
  A2 <- loss(Z2)
  
  cache <- list("Z1" = Z1,
                "A1" = A1, 
                "Z2" = Z2,
                "A2" = A2)
  
  return (cache)
}


fwd_prop <- forwardPropagation(X_train, init_params, layer_size, sigmoid)
#fwd_prop <- forwardPropagation(X_train, init_params, layer_size, sign)
lapply(fwd_prop, function(x) dim(x))

### Función de Costo

computeCost <- function(X, y, cache) {
  m <- dim(t(X))[2]
  A2 <- cache$A2
  logprobs <- (log(A2) * t(y)) + (log(1-A2) * (1-t(y)))
  cost <- -sum(logprobs/m)
  return (cost)
}

cost <- computeCost(X_train, Y_train, fwd_prop)

### Back Propagation ###

backwardPropagation <- function(X, y, cache, params, list_layer_size){
  
  m <- dim(X)[1]
  
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  A2 <- cache$A2 
  A1 <- cache$A1
  W2 <- params$W2
  
  dZ2 <- 1/m * (A2 - t(y))
  dW2 <-  dZ2 %*% t(A1) 
  db2 <- matrix( sum(dZ2), nrow = n_y)
  db2_new <- matrix(rep(db2, m), nrow = n_y)
  
  dZ1 <- (t(W2) %*% dZ2) * (A1 * (1 - A1))
  dW1 <- (dZ1 %*% X)
  db1 <- matrix( sum(dZ1), nrow = n_h)
  db1_new <- matrix(rep(db1, m), nrow = n_h)
  
  grads <- list("dW1" = dW1, 
                "db1" = db1,
                "dW2" = dW2,
                "db2" = db2)
  
  return(grads)
}

back_prop <- backwardPropagation(X_train, Y_train, fwd_prop, init_params, layer_size)

### Actualizar parámetros

updateParameters <- function(grads, params, learning_rate){
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  dW1 <- grads$dW1
  db1 <- grads$db1
  dW2 <- grads$dW2
  db2 <- grads$db2
  
  
  W1 <- W1 - learning_rate * dW1
  b1 <- b1 - learning_rate * db1
  W2 <- W2 - learning_rate * dW2
  b2 <- b2 - learning_rate * db2
  
  updated_params <- list("W1" = W1,
                         "b1" = b1,
                         "W2" = W2,
                         "b2" = b2)
  
  return (updated_params)
}

update_params <- updateParameters(back_prop, init_params, learning_rate = 0.01)

### Predecir

makePrediction <- function(X, y, hidden_neurons, updated_params, loss){
  layer_size <- getLayerSize(t(X), t(y), hidden_neurons)
  params <- updated_params
  fwd_prop <- forwardPropagation(X, params, layer_size, loss)
  pred <- fwd_prop$A2
  
  return (pred)
}

y_pred <- makePrediction(X_train, Y_train, 4, update_params, sigmoid)
y_pred <- round(y_pred)
sum(y_pred[1,] == Y_train[,1])/dim(Y_train)[1]

trainModel <- function(X, y, num_iteration, hidden_neurons, lr, loss){
  
  layer_size <- getLayerSize(t(X), t(y), hidden_neurons)
  init_params <- initializeParameters( layer_size)
  cost_history <- c()
  accs = c()
  for (i in 1:num_iteration) {
    fwd_prop <- forwardPropagation(X, init_params, layer_size, loss)
    cost <- computeCost(X, y, fwd_prop)
    back_prop <- backwardPropagation(X, y, fwd_prop, init_params, layer_size)
    update_params <- updateParameters(back_prop, init_params, learning_rate = lr)
    init_params <- update_params
    cost_history <- c(cost_history, cost)
    y_pred <- makePrediction(X, y, hidden_neurons, update_params, loss)
    y_pred <- round(y_pred)
    acc = sum(y_pred[1,] == y[,1])/dim(y)[1]
    accs = c(accs, acc)
    
    if (i %% 100 == 0) cat("Iteration", i, " | Cost: ", cost, " | Acc: ", acc*100, "\n")
  }
  
  model_out <- list("updated_params" = update_params,
                    "cost_hist" = cost_history, 'accuracy' = accs)
  return (model_out)
}

EPOCHS = 2000
HIDDEN_NEURONS = 40
LEARNING_RATE = 0.5

train_model <- trainModel(X_train, Y_train, hidden_neurons = HIDDEN_NEURONS,
                          num_iteration = EPOCHS, lr = LEARNING_RATE, sigmoid)

plot(train_model$cost_hist, pch = 16, cex = 0.2, col ='red', main = 'Loss vs Epocas')
plot(train_model$accuracy, pch = 16, cex = 0.2, col ='red', main = 'Accuracy vs Epocas')
