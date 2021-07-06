library('faraway')
library('neuralnet')
library("ggplot2")

### Datos ####
data(wbca)

X = wbca

k = 50
accuracy1 <- rep(NA, k) # NN con 1 capas ocultas
accuracy2 <- rep(NA, k) # NN con 2 capas ocultas
accuracy3 <- rep(NA, k) # NN con 3 capas ocultas
accuracy4 <- rep(NA, k) # Regresión Logística


for (i in 1:k){
  # Conjuntos de entrenamiento y validación 
  test_indexes <- sample(2, dim(X)[1], replace = TRUE, prob = c(0.7, 0.3))
  test_data <- X[test_indexes==2, ]
  train_data <- X[test_indexes==1,]
  
  # ground truth
  actual <- test_data$Class
  
  # Creando modelos con datos de entrenamiento
  nn1 <- neuralnet(Class~., train_data, hidden = c(30), linear.output = FALSE, threshold = 0.000001)
  nn2 <- neuralnet(Class~., train_data, hidden = c(30,30), linear.output = FALSE, threshold = 0.000001)
  nn3 <- neuralnet(Class~., train_data, hidden = c(30,30,30), linear.output = FALSE, threshold = 0.000001)
  lr <- glm(Class ~ ., data = train_data, family = 'binomial')
  
  
  # Ejecute datos de prueba a través de redes neuronales
  results1 <- compute(nn1,test_data)
  results2 <- compute(nn2,test_data)  
  results3 <- compute(nn3,test_data)  
  
  # obteniendo etiquetas estimadas 
  estimate1 <- round(results1$net.result)
  estimate2 <- round(results2$net.result)
  estimate3 <- round(results3$net.result)
  estimate4 <- round(predict(lr, test_data, type = 'response'))
  
  # Calculando la accuracy
  accuracy1[i] <- mean(estimate1 == actual)  
  accuracy2[i] <- mean(estimate2 == actual)
  accuracy3[i] <- mean(estimate3 == actual)
  accuracy4[i] <- mean(estimate4 == actual)
}

accuracy = data.frame(index = 1:k, acc = c(accuracy1, accuracy2, accuracy3, accuracy4), 
                      group = rep(c('nn1', 'nn2', 'nn3', 'lr'), each = k))
ggplot(accuracy, aes(index, acc, col = group)) + geom_line()


