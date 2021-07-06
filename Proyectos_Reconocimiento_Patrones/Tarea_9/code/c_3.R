library(kernlab)
library(ggplot2)
library(caret)

data(spam)

spam$type = unclass(as.factor(spam$type))
X = spam

k = 5
accuracy1 <- rep(NA, k) # NN con 1 capas ocultas
accuracy2 <- rep(NA, k) # NN con 2 capas ocultas



for (i in 1:k){
  # Conjuntos de entrenamiento y validación 
  test_indexes <- sample(2, dim(X)[1], replace = TRUE, prob = c(0.7, 0.3))
  test_data <- X[test_indexes==2, ]
  train_data <- X[test_indexes==1,]
  
  # ground truth
  actual <- test_data$type
  
  # Creando modelos con datos de entrenamiento
  nn1 <- train(type~., train_data, method = 'nnet', trace = FALSE)
  nn2 <- train(type~., train_data, method = 'pcaNNet', trace = FALSE)
  
  # Predecir
  results1 <- predict(nn1,test_data)
  results2 <- predict(nn2,test_data)  
  
  # Calculando la accuracy
  accuracy1[i] <- mean(results1 == actual)  
  accuracy2[i] <- mean(results2 == actual)
  
  print(i)
}

accuracy = data.frame(index = 1:k, acc = c(accuracy1, accuracy2), 
                      group = rep(c('NNet', 'pcaNNet'), each = k))
ggplot(accuracy, aes(index, acc, col = group)) + geom_line()

