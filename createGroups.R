createGroups <- function(trainData, numberOfClasses, algorithm = 'kmeans'){
  if(missing(trainData)){
    stop('No train data to create model.')
  }
  if(missing(numberOfClasses)){
    stop('Number of classes needed.')
  }
  
  if(algorithm == 'kmeans'){
    result <- kmeans(trainData, numberOfClasses, iter.max = 20, nstart=10)
    labels <- result$cluster
    centers <- result$centers
    
  }else if(algorithm == 'kmedoids'){
    result <- pam(trainData, diss = FALSE, numberOfClasses, stand = FALSE)
    labels <- result$clustering
    
    centers <- matrix(0, numberOfClasses, ncol(trainData))
    for(i in 1:numberOfClasses){
      centers[i, ] <- trainData[which(apply(pamResult$data, 1, function(x) all.equal(x, pamResult$medoids[i,])) == "TRUE"),] # indeks medoidu
    }
    
  }else if(algorithm == 'hierarchic'){
    agnesResult <- agnes(trainData, stand = TRUE)
    labels <- cutree(agnesResult, k=numberOfClasses)
    
    centers <- matrix(0, numberOfClasses, ncol(trainData))
    for(i in 1:length(unique(labels))){
      tmp <- trainData[which(labels==i),]
      centers[i,] <- kmeans(tmp, 1, iter.max = 20, nstart=20)$centers
    }
    
  }else{
    labels <- NULL
    centers <- NULL
    warning('Unknown algorithm.')
  }
  
  return(list('labels' = labels, 'centers' = centers))
}