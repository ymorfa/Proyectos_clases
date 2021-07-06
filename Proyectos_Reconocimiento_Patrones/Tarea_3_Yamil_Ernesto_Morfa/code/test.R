where = function(x, val){
  bool = (x == val)
  index = 0
  for (i in 1:length(bool)){
    if (bool[i]==TRUE){
      index = i
      break
    }
  }
  return( index ) 
}

euclidean = function(x, y){
  return(sum((x-y)^2))
}

distances_matrix = function(X){
  n = dim(X)[1]
  D = matrix(,nrow = n, ncol = n)
  for (i in 1:n){
    for (j in i:n){
      D[i,j] = D[j,i] = euclidean(X[i], X[j])
    }
  }
  return(D)
}

get_graph = function(X, k){
  n = dim(X)[1]
  dis = distances_matrix(X)
  psitions = matrix(, nrow = n, ncol = k)
  for (i in 1:n){
    k_n = sort(dis[i,i:n])[2:k+1]
    for (j in 1:k){
      positions[i,j] = where(dis[i], k_n[j])
    }
  }
  return(positions)
}


