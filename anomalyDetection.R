library(arules)
library(cluster)
data("AdultUCI")

ad <- as.numeric(AdultUCI$workclass)
age <- as.numeric(AdultUCI$age)

x <- 0
for(i in 1:length(AdultUCI$age)){
  if(AdultUCI$age[i] == age[i]) {
    x <- x+1
  }
}




data <- AdultUCI
for (i in 1:ncol(data)){
  data[,i] <- as.numeric(data[,i])
}

data[is.na(data)] <- -1


wss <- vector(mode = "integer" ,length = ncol(data))
for (i in 1:ncol(data)) {  # Od 1 do 15 grup
  kmeans.group <- kmeans(data, centers = i, iter.max = 20, nstart=20)
  wss[i] <- kmeans.group$tot.withinss # Całkowita suma odległości wewnątrz grup
}

result <- kmeans(data, 3)


x2 <- 0
for(i in 1:nrow(data)){
  if(data$income[i] == result$cluster[i]) {
    x2 <- x2+1
  }
}
x2 <- x2/nrow(data)


k <- 2
pokemonKmeans <- kmeans(as.matrix(data[1:1000,]), k, iter.max = 20)
plot(data[1:1000,], col = pokemonKmeans$cluster)












#########################################
data <- matrix(0, 10, 3)
colnames(data) <- c('p', 'd', 't')
data[1:5,1] <- runif(5, 1, 10)
data[6:10,1] <- runif(5, 15, 30)
data[1:5,2] <- runif(5, 1, 10)
data[6:10,2] <- runif(5, 15, 30)
data[1:5,3] <- 5
data[6:10,3] <- 2

agn1 <- agnes(data[,1:2], metric = "manhattan", stand = TRUE)
agn1Class <- cutree(agn1, k=2)

hcl <- hclust(dist(data[,1:2]), method = "centroid")
hclClass <- cutree(hcl, k=2)
hcl$labels
plot(hcl)


knnAlg <- knn(data[c(1,2,3,6,7,8),1:2], data[c(4,5,9,10),1:2], data[c(1,2,3,6,7,8),3])
