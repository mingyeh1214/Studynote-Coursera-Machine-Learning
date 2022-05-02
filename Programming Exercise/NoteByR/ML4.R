library(R.matlab)
library(dplyr)
library(plotly)

path <- '/Users/liumingyeh/Desktop/OpenCourse/Machine Learning by Standford/machine-learning-ex7/ex7/ex7data2.mat'
data <- path %>% readMat()
X <- data$X
myKMeans <- function(X, K, max_iters){

  kMeansInitCentroids  <- function(X, K) {
    centroids <- matrix(0, K, dim(X)[2])
    randidx <- sample(dim(X)[1])
    centroids <- X[randidx[1:K],]
    centroids
  }
  
  findClosestCentroids <- function(X, centroids) {
    m <- dim(X)[1]
    n <- dim(X)[2]
    K <- dim(centroids)[1]
    for (i in 1:m) {
      a <- matrix(rep(X[i,], time = K), c(K, n), byrow = T) - centroids
      if(i == 1) idx <- which.min(rowSums(a ^ 2))
      else idx <- c(idx, which.min(rowSums(a ^ 2)))
    }
    idx
  }
  
  computeCentroids  <- function(X, idx, K) {
    m <- dim(X)[1]
    n <- dim(X)[2]
    for (k in 1:K) {
      if (k == 1) centroids <- colMeans(X[idx == k,])
      else centroids <- rbind(centroids, colMeans(X[idx == k,]))
    }
    centroids
  }
  
  runkMeans  <- function(X, initial_centroids, max_iters, plot=TRUE) {
    m <- dim(X)[1]
    n <- dim(X)[2]
    K <- dim(initial_centroids)[1]
    centroids <- array(0, dim = c(dim(initial_centroids), max_iters + 1))
    centroids[,,1] <- initial_centroids
    idx <- matrix(0, m, max_iters + 1)
    for (i in 1:max_iters) {
      idx[,i] <- findClosestCentroids(X, centroids[,,i])
      centroids[,,i+1] <- computeCentroids(X, idx[,i], K)
    }
    idx[,max_iters + 1] <- findClosestCentroids(X, centroids[,,max_iters + 1])
    if (plot==TRUE){
      plot<-plot_ly() %>% 
        add_trace(x=X[,1],y=X[,2],type='scatter',mode='markers',
                  color=factor(idx[,max_iters + 1])) 
      for(i in 1:K){
        plot<-plot%>%
          add_trace(x=centroids[i,1,],
                    y=centroids[i,2,],type='scatter',mode='lines+markers',
                    line=list(dash='dot',color='black',alpha=0.8),
                    marker = list(size = 8,symbol='x',alpha=0.75,color='red'),
                    showlegend = FALSE, visible=TRUE)
      }
    }
    list(centroids = centroids, idx=idx, plot=plot)
  }
  
  initial_centroids <- kMeansInitCentroids(X, K)
  kMean <- runkMeans(X, initial_centroids, max_iters)
  kMean
}
model<-myKMeans(X,3,10)
model$idx[,11]
error <- rep(0,10)
for(i in 1:10){
  kmean_cluster <- kmeans(iris[,1:4],3,nstart=20)
  error[i] <- kmean_cluster$tot.withinss
}
?kmeans_cluster$totss
kmean_cluster <- kmeans(iris[,1:4],3,nstart=10)
table(kmean_cluster$cluster, iris$Species)
kmean_cluster$tot.withinss

plot_ly(x=1:10,y=error,type='scatter',mode='markers+lines')
library(cluster)
cl<-clusplot(iris[,1:4],kmean_cluster$cluster, color=T,hade=T,labels=0,lines=0)
plot_ly(cl)
cl$
pca<-prcomp(iris[,1:4])
plot(pca,type='line')
vars<- pca$sdev^2
props<-vars/sum(vars)
cumsum(props)
