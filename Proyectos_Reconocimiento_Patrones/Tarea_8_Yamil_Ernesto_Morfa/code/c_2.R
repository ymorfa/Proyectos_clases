
data(spam)

sam = sample(2, dim(spam)[1], replace = TRUE, prob = c(0.7,0.3))

train = spam[sam == 1, ]
test = spam[sam == 2, ]

library(rpart)
library(rpart.plot)

pred = function(prob, y){
  y_ = c()
  for (i in 1:dim(prob)[1]){
    if (prob[i,1] > prob[i,2]){
      y_[i] = 'nonspam'
    } else {
      y_[i] = 'spam'
    }
  }
  acc = sum(y_==y)/length(y)
  return(acc)
}

cp_set = seq(0.5, 0.0001, -0.01)
acc_ = c()
for (cp in cp_set){
  r1 <- rpart(type~.,data=train,method='class',cp = cp)
  #rpart.plot(r1, main=cp)
  #rpart.rules(r1)
  acc = pred(predict(r1, test), test$type)   # error sobre conjunto de entrenamiento
  acc_ = append(acc_, acc)
}



plot(cp_set, acc_, main = 'tree accuracy')

cp = cp_set[acc_ == max(acc_)][1]

r1 <- rpart(type~.,data=train,method='class',cp = cp)
rpart.plot(r1, main=cp)
rpart.rules(r1)

svm = ksvm(type~., data = train, kernel='rbfdot')
acc_svm = sum(predict(svm, test) == test$type)/length(test$type)  

acc_tree_ = c()
acc_svm_ = c()
for (i in 1:50){
  sam = sample(2, dim(spam)[1], replace = TRUE, prob = c(0.7,0.3))
  train = spam[sam == 1, ]
  test = spam[sam == 2, ]
  r1 <- rpart(type~.,data=train,method='class',cp = 0.01)
  svm = ksvm(type~., data = train, kernel='rbfdot')
  acc_tree = pred(predict(r1, test), test$type)
  acc_svm = sum(predict(svm, test) == test$type)/length(test$type)
  
  acc_tree_ = append(acc_tree_, acc_tree)
  acc_svm_ = append(acc_svm_, acc_svm)
}

plot(1:50, acc_tree_, col = 'red', ylab = 'acc', main='Accuracy', ylim=c(0.87, 0.95))
points(1:50, acc_svm_, col = 'blue')
lines(1:50, rep(mean(acc_tree_), length(acc_tree_)) , col = 'red')
lines(1:50, rep(mean(acc_svm_), length(acc_svm_)) , col = 'blue')
