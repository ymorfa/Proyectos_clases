library(palmerpenguins)
library(ggplot2)
library(kernlab)
library(MASS)
library(FastKNN)
library(pracma)

data = subset(penguins, penguins$species!='Adelie')
data = subset(data, !is.na(data$bill_length_mm))

get_data = function(data, proba){
  K = length(proba)  
  sampl = sample(2, dim(data)[1], replace = TRUE, prob = proba)
  
  X = cbind( data$bill_length_mm, data$bill_depth_mm, data$flipper_length_mm, data$body_mass_g)
  
  categoria<-factor(data$species)
  
  X_train = X[sampl==1,]
  X_test = X[sampl==2,]
  categoria_train = categoria[sampl==1]
  categoria_test = categoria[sampl==2]
  
  d_train<-data.frame(X_train[,1],X_train[,2],X_train[,3], X_train[,4], categoria_train)
  names(d_train)<-c("X1","X2","X3","X4","categoria")
  
  d_test<-data.frame(X_test[,1],X_test[,2],X_test[,3], X_test[,4], categoria_test)
  names(d_test)<-c("X1","X2","X3","X4","categoria")
  
  return(list(d_train, d_test))
}


### SVM
acc = c()
for (i in 1:100){
  d = get_data(data, c(0.7,0.3))
  d_train = d[[1]]
  d_test = d[[2]]
  s<-ksvm(categoria~X1+X2 ,data=d_train,kernel="p",cost=1,kpar=list(degree=1,offset=0))
  acc[i] = sum(predict(s, d_test) == d_test$categoria)/length(d_test$categoria)  
}

plot(acc, main = 'SVM acc')
lines(1:100, rep(sum(acc)/100, 100))

#plot(s,data=d_train, xlim=c(13, 20))

### LDA

acc = c()
for (i in 1:100){
  d = get_data(data, c(0.7,0.3))
  d_train = d[[1]]
  d_test = d[[2]]
  l = lda(categoria ~ X1+X2, data = d_train)
  acc[i] = sum(predict(l, d_test)$class == d_test$categoria)/length(d_test$categoria)  
}

plot(acc, main = 'LDA acc')
lines(1:100, rep(sum(acc)/100, 100))


### KNN

classifier = function(dis, train_labels,  K){
  #pred_labels = matrix(, ncol = dim(dis)[1])
  pred_labels = c()
  for (i in 1:dim(dis)[1]){
    neighbor_index = order(dis[i,])[1:K]
    label = train_labels[neighbor_index]
    pred_labels[i] = Mode(label)
  }
  return(pred_labels)
} 


acc = c()
for (i in 1:100){
  d = get_data(data, c(0.7,0.3))
  d_train = d[[1]]
  d_test = d[[2]]
  dis = Distance_for_KNN_test(d_test, d_train)
  pred = predict(s, d_test)
  acc[i] = sum(pred == d_test[,5])/length(pred)
}


plot(acc, main = 'K-nn acc')
lines(1:100, rep(sum(acc)/100, 100))




