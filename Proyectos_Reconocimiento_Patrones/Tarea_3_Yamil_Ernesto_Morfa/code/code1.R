library('pracma')


dis = function(theta1, theta2){
  return( theta1*log(theta1/theta2) + (1-theta1)*log((1-theta1)/(1-theta2)) )
}

theta1 = 0.5
theta2 = linspace(0.001, 0.999, 100)
d = matrix(,nrow = 100)
for (i in 1:100){
  d[i] = dis(theta1, theta2[i])
}

plot(theta2, d, type = 'l', col = 'red')
points(theta1,0, col = 'blue')
