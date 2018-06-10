predictAnomalies <- function(model, testData, testClasses, alfa = 0.8, beta = 2){
  classes <- model$labels
  classNames <- unique(classes)
  numberOfClasses <- length(classNames)

  print('wyznaczanie klastrow ')
  print(Sys.time())
  # wyznaczanie malych i duzych klastrow
  classNameSize <- data.frame(matrix(0, numberOfClasses, 3)) # className, classSize, Lc=1/SC=0
  classNameSize[,1] <- classNames
  for(i in 1:numberOfClasses){
    #classNameSize[i,1] <- classNames[i]
    classNameSize[i,2] <- length(which(classes==classNames[i]))
  }
  
  classNameSize <- data.frame(classNameSize[order(classNameSize[,2], decreasing = TRUE),])
  for(i in 1:(numberOfClasses-1)){
    if(numberOfClasses > 1){
      if((sum(classNameSize[1:i,2]) <= length(classes)*alfa) || classNameSize[i,2]/classNameSize[i+1,2] >= beta){
        classNameSize[i,3] <- 1
      }
    }else{classNameSize[1,3] <- 1}
  }
  print('wyznaczanie wskaznikow nietypowosci ')
  print(Sys.time())
  # wyznaczanie wskaznikow nietypowosci
  centers <- model$centers
  
  outlierM <- matrix(0, nrow(testData), 3) # miara nietypowosci
  distance <- matrix(0, numberOfClasses, 1) # odleglosc punktu p do wszystkich grup

  LC <- classNameSize[which(classNameSize[,3] == 1), 1]
  for(i in 1:nrow(testData)){
    
    for(j in 1:numberOfClasses){
      if(model$name == 'kmedoids'){
        distance[j] <- dist(rbind(as.matrix(testData[i,]), as.matrix(centers[j,])))
      }else{
        distance[j] <- dist(rbind(as.matrix(testData[i,]), centers[j,]))
      }
    }
    
    minDistIndex <- which(distance == min(distance)) # ps' index of nearest cluster 
    
    
    # uCBLOF
    if(classNameSize[which(classNameSize[,1]==minDistIndex), 3]){ # if p is in LC
      outlierM[i,2] <- distance[minDistIndex]
    }else{ # p is in SC
      #LC <- classNameSize[which(classNameSize[,3] == 1), 1]
      outlierM[i,2] <- min(distance[LC])
    }
    
    # CBLOF
    outlierM[i,1] <- classNameSize[which(classNameSize[,1]==minDistIndex), 2] * outlierM[i,2]
    
    "
    if(classNameSize[which(classNameSize[,1]==minDistIndex), 3]){ # if p is in LC
    outlierM[i,1] <- classNameSize[which(classNameSize[,1]==minDistIndex), 2] * distance[minDistIndex]
    }else{ # p is in SC
    LC <- classNameSize[which(classNameSize[,3] == 1), 1]
    outlierM[i,1] <- classNameSize[which(classNameSize[,1]==minDistIndex), 2] * min(distance[LC])
    }
    "
  
    # LDCOF
    avgDist <- mean(distance)
    if(classNameSize[which(classNameSize[,1]==minDistIndex), 3]){ # if p is in LC
      outlierM[i,3] <- distance[minDistIndex] / avgDist
    }else{ # p is in SC
      #LC <- classNameSize[which(classNameSize[,3] == 1), 1]
      outlierM[i,3] <- min(distance[LC]) / avgDist
    }
  
  }
  
  print('wyznaczanie anomalii ')
  print(Sys.time())
  # wyznaczanie anomalii
  accTable <- matrix(0, nrow(testData), 3) # CBLOF, uCBLOF, LDCOF - miara mowi, ze anomalia => 1, 0 w p.p.
  #accTable[,4] <- testClasses
  confusionMatrix <- matrix(0, 3, 4) # dla kazdego wskaznika (TP, FP, FN, TN)
  anomalyClass <- setdiff(unique(testClasses), unique(classes)) # ktora etykieta klasy to anomalie

  m1 <- max(outlierM[,1]) - min(outlierM[,1])
  m2 <- max(outlierM[,2]) - min(outlierM[,2])
  m3 <- max(outlierM[,3]) - min(outlierM[,3])
  min1 <- min(outlierM[,1])
  min2 <- min(outlierM[,2])
  min3 <- min(outlierM[,3])

  for(i in 1:nrow(testData)){
    
    if((outlierM[i,1]-min1)/m1 > 0.5){ # czysta improwizacja
      accTable[i,1] <- 1
    }
    
    if((outlierM[i,2] - min2)/ m2 > 0.5){ # p-stwo, że p jest outlierem?
      accTable[i,2] <- 1
    }
    
    if((outlierM[i,3] - min3)/ (m3+0.000001) > 0.5){ # wskaznik > 0.5 -> outlier ?
      accTable[i,3] <- 1
    }
    
    for(j in 1:3){
      if(accTable[i,j] == 0 && testClasses[i] != anomalyClass){ # TP
        confusionMatrix[j, 1] <- confusionMatrix[j, 1] + 1
      }
      
      else if(accTable[i,j] == 0 && testClasses[i] == anomalyClass){ # FP
        confusionMatrix[j, 2] <- confusionMatrix[j, 2] + 1
      }
      
      else if(accTable[i,j] == 1 && testClasses[i] != anomalyClass){ # FN
        confusionMatrix[j, 3] <- confusionMatrix[j, 3] + 1
      }
      
      else if(accTable[i,j] == 1 && testClasses[i] == anomalyClass){ # TN
        confusionMatrix[j, 4] <- confusionMatrix[j, 4] + 1
      }
    }
    
    
  }
  
  print('wyznaczanie wskaznikow jakosci ')
  print(Sys.time())
  accuracy <- matrix(0,3,1)
  precision <- matrix(0,3,1)
  
  for(i in 1:3){
    accuracy[i] <- (confusionMatrix[i,1] + confusionMatrix[i,4])/sum(confusionMatrix[i,])
    precision[i] <- confusionMatrix[i,1] / (confusionMatrix[i,1] + confusionMatrix[i,2])
  }

  
  
  return(list('classNameSize' = classNameSize, 'outlier' = outlierM, 'anomalies' = accTable, 'confusionMatrix' = confusionMatrix, 'accuracy' = accuracy, 'precision' = precision))
}