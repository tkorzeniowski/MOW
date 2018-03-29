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


