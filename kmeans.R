library(readr)
data(iris)

#Randomizing the observations.
gp <- runif(150,0,1)
iris <- iris[order(gp),]


#Implementing K-means algorithm
set.seed(9000)
call_kmeans <- function(data,k,iter){
  tol <- 0.01
  sse <- 100000
  #Initializing the first k points as the centroids
  centroids <- data[1:k,]
  i <- 1
  while(sse>tol & i<=iter){
  i <- i+1
  #Calculating euclidean distance from the centers
  distances <- dist_fun(data,centroids)
  
  #Renaming distances matrix column by the centroid numbers
  colnames(distances) <- c(1:k)

  #Finding which cluster does each observation belong to
  min_distance <- colnames(distances)[apply(distances,1,which.min)]
  
  #Clustering every group and finding out new centroids by taking the mean of those values
  new_centroids <- aggregate(.~min_distance,data,FUN=mean)
  
  #Calculating the sum of squared errors between the previous centroid value and the current centroid values
  sse <- sum(sqrt(rowSums((new_centroids[,-1] - centroids)^2)))
  
  #Updating new centroid values
  centroids <- new_centroids[,2:ncol(new_centroids)]
  }
  #Return the cluster to which each observation belongs to.
  min_distance
}


dist_fun <- function(x1,x2){
  dist_matrix <- matrix(rep(0),nrow(x1),nrow(x2))
  for(i in 1:nrow(x1)){
    for( j in 1:nrow(x2)){
      dist_matrix[i,j] <- as.matrix(sqrt(sum((x1[i,] - x2[j,])^2)))
    }
  }
  dist_matrix
}

dist_fun_manhattan <- function(x1,x2){
  dist_matrix <- matrix(rep(0),nrow(x1),nrow(x2))
  for(i in 1:nrow(x1)){
    for( j in 1:nrow(x2)){
      dist_matrix[i,j] <- as.matrix((sum(abs(x1[i,] - x2[j,]))))
    }
  }
  dist_matrix
}


results <- call_kmeans(iris[,c(1:4)],3,200)
#Evalutaing kmeans 
table(results,as.numeric(factor(iris$Species)))
#Since it is an unsupervised technique the label names don't always match.In this case they match.
