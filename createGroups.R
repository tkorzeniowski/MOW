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
    name <- 'kmeans'
    
  }else if(algorithm == 'kmedoids'){
    result <- pam(trainData, diss = FALSE, numberOfClasses, stand = FALSE)
    labels <- result$clustering
    
    centers <- data.frame(matrix(0, numberOfClasses, ncol(trainData)))
    colnames(centers) <- colnames(trainData)
    for(i in 1:numberOfClasses){
      centers[i, ] <- trainData[which(apply(result$data, 1, function(x) all.equal(x, result$medoids[i,])) == "TRUE"),] # indeks medoidu
    }
    name <- 'kmedoids'
    
  }else if(algorithm == 'hierarchic'){
    agnesResult <- agnes(trainData, stand = TRUE)
    labels <- cutree(agnesResult, k=numberOfClasses)
    
    centers <- matrix(0, numberOfClasses, ncol(trainData))
    for(i in 1:length(unique(labels))){
      tmp <- trainData[which(labels==i),]
      centers[i,] <- kmeans(tmp, 1, iter.max = 20, nstart=20)$centers
    }
    name <- 'hierarchic'
    
  }else{
    labels <- NULL
    centers <- NULL
    name <- NULL
    warning('Unknown algorithm.')
  }
  
  return(list('labels' = labels, 'centers' = centers, 'name' = name))
}