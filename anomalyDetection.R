#install.packages("RWeka")
#install.packages("partykit")
#install.packages('matrixStats')
library(cluster)
library(class)
library(caret)
library(RWeka)
library(partykit)
library(matrixStats)
library(clusterSim)
source('createGroups.R')
source('predict.R')
set.seed(123)


# wczytywanie zbiorow danych i wybor atrybutow decyzyjnych (klas poszczegolnych obserwacji)
############################################

# MUSHROOM dataset

############################################
mushroomDataset <- as.matrix(read.table('mushroom.txt', sep = ','))

anomalies <- mushroomDataset[which(mushroomDataset[,1]=='p'),]
mushroomDataset <- mushroomDataset[-which(mushroomDataset[,1]=='p'),]

for(i in 1:nrow(anomalies)){
  for(j in 1:ncol(anomalies)){
    if(anomalies[i,j]=="?"){
      anomalies[i,j] <- "i" # litera i nie jest wykorzystywana jako wartosc atrybutu, wiec mozna ja wykorzystac zamiast ? (wartosci brakujacych)
    }
    anomalies[i,j] <- which(anomalies[i,j] == letters)
  }
}

for(i in 1:nrow(mushroomDataset)){
  for(j in 1:ncol(mushroomDataset)){
    if(mushroomDataset[i,j]=="?"){
      mushroomDataset[i,j] <- "i" # litera i nie jest wykorzystywana jako wartosc atrybutu, wiec mozna ja wykorzystac zamiast ? (wartosci brakujacych)
    }
    mushroomDataset[i,j] <- which(mushroomDataset[i,j] == letters)
  }
}


anomalies <- as.data.frame(anomalies)
anomalies[,1] <- 2
mushroomDataset <- as.data.frame(mushroomDataset)

data <- mushroomDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
trainClasses <- matrix(1, nrow(trainData), 1)
trainData[,1] <- NULL
testData <-data[-idTrainData,]
testData[,1] <- 1 

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,1])
testData[,1] <- NULL

######### Sztuczne anomalie ##########

mushroomDataset <- as.matrix(read.table('mushroom.txt', sep = ','))

for(i in 1:nrow(mushroomDataset)){
  for(j in 1:ncol(mushroomDataset)){
    if(mushroomDataset[i,j]=="?"){
      mushroomDataset[i,j] <- "i" 
    }
    mushroomDataset[i,j] <- which(mushroomDataset[i,j] == letters)
  }
}

mushroomDataset <- as.data.frame(mushroomDataset)
data <- mushroomDataset

idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]

data = as.matrix(as.data.frame(lapply(data, as.numeric)))

# Sprawdzam maksymalne i minimalne wartosci kazdej z cech
max_min_feature_values <- matrix(0, 2, ncol(data))
for(i in 1:ncol(data)){
  max_min_feature_values[1,i] = max(data[,i])
  max_min_feature_values[2,i] = min(data[,i])
}


# obliczam progi dla anomalii: wystepuja od wartosci 2*max przez 1/3 zakresu danej cechy
ranges <- matrix(0, 2, ncol(data))
for(i in 1:ncol(data)){
  ranges[1,i] <- 2 * max_min_feature_values[1,i]
  ranges[2,i] <- ranges[1,i] + (max_min_feature_values[1,i] - max_min_feature_values[2,i])/3
}

anomalies <- matrix(0, 150, ncol(data))
for(i in 1:ncol(data)){
  anomalies[,i] <- runif(150, ranges[1,i], ranges[2,i])
}
anomalies[,1] <- 3


testData = as.matrix(as.data.frame(lapply(testData, as.numeric)))
testData <- rbind(testData, anomalies)
testClasses <- testData[,1]
testData <- as.data.frame(testData)
testData[,1] <- NULL





############################################

# LETTER dataset

############################################

letterDataset <- as.matrix(read.table('letterRecognition.txt', sep = ','))

anomalies <- letterDataset[which(letterDataset[,1]=='Z'),]
anomalies[,1] <- 26
letterDataset <- letterDataset[-which(letterDataset[,1]=='Z'),]

for(i in 1:nrow(letterDataset)){
  letterDataset[i,1] <- which(letterDataset[i,1] == LETTERS)
}
letterDataset <- as.data.frame(letterDataset)

letterClasses <- as.numeric(letterDataset[,1])

data <- letterDataset

idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,1])
testData[,1] <- NULL


######### Sztuczne anomalie ##########

letterDataset <- as.matrix(read.table('letterRecognition.txt', sep = ','))
for(i in 1:nrow(letterDataset)){
  letterDataset[i,1] <- which(letterDataset[i,1] == LETTERS)
}
letterDataset <- as.data.frame(letterDataset)
letterClasses <- letterDataset[,1]
data <- letterDataset

idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]

data = as.matrix(as.data.frame(lapply(data, as.numeric)))

# Sprawdzam maksymalne i minimalne wartosci kazdej z cech
max_min_feature_values <- matrix(0, 2, ncol(data))
for(i in 1:ncol(data)){
  max_min_feature_values[1,i] = max(data[,i])
  max_min_feature_values[2,i] = min(data[,i])
}


# obliczam progi dla anomalii: wystepuja od wartosci 2*max przez 1/3 zakresu danej cechy
ranges <- matrix(0, 2, ncol(data))
for(i in 1:ncol(data)){
  ranges[1,i] <- 2 * max_min_feature_values[1,i]
  ranges[2,i] <- ranges[1,i] + (max_min_feature_values[1,i] - max_min_feature_values[2,i])/3
}

anomalies <- matrix(0, 400, ncol(data))
for(i in 1:ncol(data)){
  anomalies[,i] <- runif(400, ranges[1,i], ranges[2,i])
}
anomalies[,1] <- 26


testData = as.matrix(as.data.frame(lapply(testData, as.numeric)))
testData <- rbind(testData, anomalies)
testClasses <- testData[,1]
testData <- as.data.frame(testData)
testData[,1] <- NULL






############################################

# SENSOR dataset

############################################

sensorDataset <- read.table('SensorlessDriveDiagnosis.txt')

anomalies <- sensorDataset[which(sensorDataset[,49]==11),]
# tylko dla algorytmu hierarchicznego
"
sensor <- 0
for(i in 1:10){
  sensor <- rbind(sensor, sensorDataset[which(sensorDataset[,49]==i)[1:2700],])
}
sensor <- sensor[-1,]
data <- sensor
"

sensorDataset <- sensorDataset[-which(sensorDataset[,49]==11),]

data <- sensorDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
trainClasses <- trainData[,49]
trainData[,49] <- NULL
testData <-data[-idTrainData,]

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,49])
testData[,49] <- NULL

######### Sztuczne anomalie ##########

sensorDataset <- read.table('SensorlessDriveDiagnosis.txt')
data <- sensorDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
trainClasses <- trainData[,49]
trainData[,49] <- NULL
testData <-data[-idTrainData,]

# Sprawdzam maksymalne i minimalne wartosci kazdej z cech
max_min_feature_values <- matrix(0, 2, ncol(data))
for(i in 1:ncol(data)){
    max_min_feature_values[1,i] = max(data[,i])
    max_min_feature_values[2,i] = min(data[,i])
}


# obliczam progi dla anomalii: wystepuja od wartosci 2*max przez 1/3 zakresu danej cechy
ranges <- matrix(0, 2, ncol(data))
for(i in 1:ncol(data)){
  ranges[1,i] <- 2 * max_min_feature_values[1,i]
  ranges[2,i] <- ranges[1,i] + (max_min_feature_values[1,i] - max_min_feature_values[2,i])/3
}

anomalies <- matrix(0, 1500, ncol(data))
for(i in 1:ncol(data)){
    anomalies[,i] <- runif(1500, ranges[1,i], ranges[2,i])
}
anomalies[,49] <- 11

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,49])
testData[,49] <- NULL







############################################

# generowanie wynikow dla grupowania + predykcji na podstawie wskaznikow nietypowosci

############################################
#mushroom
CGResult <- createGroups(trainData, 1, 'kmedoids')
predictResult <- predictAnomalies(CGResult, testData, testClasses)

#letter
CGResult <- createGroups(trainData, 25, 'hierarchic')
predictResult <- predictAnomalies(CGResult, testData, testClasses)

#sensor
CGResult <- createGroups(trainData, 10, 'kmeans')
predictResult <- predictAnomalies(CGResult, testData, testClasses)

############################

"!!! 
ogólnie nie wiadomo ktora etykieta bedzie odpowiednia dla klasy, tzn. w przypadku grzybow 
etykiety to 16 i 5, a algorytm k-srednich daje 1 i 2, jak sie dowiedziec czy 16==1 czy 16==2 itd.
!!! 
moze sprobowac wszystkich kombinacji i ta, ktora daje najwieksza dokladnosc 
to jest wlasciwe etyrkietowanie?
w przypadku wiekszej liczby klas nalezy sie spodziewac, ze problem ten bedzie narastal, gdyz 
bedzie wiecej mozliwych kombinacji i sprawdzene kazdej z nich zajmie dluzej niz 
dzialanie algorytmu grupowania

aby temu zaradzic do sprawdzenia dokladnosci patrzymy czy przyklad zostal zaklasyfikowany jako 
anomalia czy jako dowolna ze znalezionych klas (niewazne ktora, bo i tak nie zgadniemy prawidlowych etykiet)
"



################################### klasyfikacja ########################################
############################################

# J48

############################################

# mushroom - 100%
mushroomDataset <- as.matrix(read.table('mushroom.txt', sep = ','))

for(i in 1:nrow(mushroomDataset)){
  for(j in 1:ncol(mushroomDataset)){
    if(mushroomDataset[i,j]=="?"){
      mushroomDataset[i,j] <- "i"
    }
    mushroomDataset[i,j] <- which(mushroomDataset[i,j] == letters)
  }
}

mushroomDataset <- as.data.frame(mushroomDataset)
data <- mushroomDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <- data[idTrainData,]
dataToJ48Classification <- trainData
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]
testClasses <- testData[,1]
testData[,1] <- NULL
resultJ48 <- J48(V1~., data = dataToJ48Classification, control= Weka_control(M=15))#


#letter - 68-70%
letterDataset <- as.matrix(read.table('letterRecognition.txt', sep = ','))
for(i in 1:nrow(letterDataset)){
  letterDataset[i,1] <- which(letterDataset[i,1] == LETTERS)
}
letterDataset <- as.data.frame(letterDataset)
#letterClasses <- as.factor(letterDataset[,1])
letterClasses <- letterDataset[,1]
data <- letterDataset

idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
#dataToJ48Classification <- letterDataset
dataToJ48Classification <- trainData
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]
#testClasses <- as.numeric(testData[,1])
testClasses <- testData[,1]
testData[,1] <- NULL
resultJ48 <- J48(V1~., data = dataToJ48Classification, control= Weka_control(M=15))#


#sensor - 98%
sensorDataset <- read.table('SensorlessDriveDiagnosis.txt')
dataToJ48Classification <- sensorDataset

data <- sensorDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
dataToJ48Classification <- trainData

trainClasses <- trainData[,49]
trainData[,49] <- NULL
testData <-data[-idTrainData,]
#testClasses <- as.numeric(testData[,49])
testClasses <- testData[,49]
testData[,49] <- NULL

dataToJ48Classification$V49 <- as.factor(dataToJ48Classification$V49)
resultJ48 <- J48(V49~., data = dataToJ48Classification, control= Weka_control(M=15))#


pred <- predict(resultJ48, testData)

cfMatrix <- confusionMatrix(pred, testClasses)
acc <- sum(diag(cfMatrix$table)) / nrow(testData)
precision <- sum(diag(cfMatrix$table)) / (sum(diag(cfMatrix$table)) + sum(lower.tri(cfMatrix$table)))
acc
precision
cfMatrix


############################################

# KNN

############################################

# normalizacja

#mushroom - dokladnosc 100% bez normalizacji
train_normalized <- trainData[,c(-16)] # 16 kolumna generuje wszedzie NaN, gdyz dzielimy 0/0, dlatego mozna pominac
test_normalized <- testData[,c(-16)]

#letter - dokladnosc 95% bez normalizacji
train_normalized <- trainData
test_normalized <- testData

#sensor - dokladnosc 82% z normalizacja (bez normalizacji około 11%)
train_normalized <- trainData
test_normalized <- testData


# możliwa normalizacja danych, wymadana dla sensorow
train_normalized = as.matrix(as.data.frame(lapply(train_normalized, as.numeric)))#

means_train = colMeans(train_normalized)
train_normalized = data.matrix(train_normalized)
sds_train = colSds(train_normalized)

train_normalized <- as.data.frame(train_normalized)
means_train <- as.data.frame(means_train)
sds_train <- as.data.frame(sds_train)



test_normalized = as.matrix(as.data.frame(lapply(test_normalized, as.numeric)))#

means_test = colMeans(test_normalized)
test_normalized = data.matrix(test_normalized)
sds_test = colSds(test_normalized)

test_normalized <- as.data.frame(test_normalized)
means_test <- as.data.frame(means_test)
sds_test <- as.data.frame(sds_test)

for(i in 1:nrow(means_train)){
  train_normalized[,i] <- (train_normalized[,i] - means_train[i,])/sds_train[i,]
  test_normalized[,i] <- (test_normalized[,i] - means_train[i,])/sds_train[i,]
  
}


knnAlg <- knn(train_normalized, test_normalized, trainClasses, k = 3, l = 1)

cfMatrix <- confusionMatrix(knnAlg, testClasses)
acc2 <- sum(diag(cfMatrix$table)) / nrow(testData)
precision <- sum(diag(cfMatrix$table)) / (sum(diag(cfMatrix$table)) + sum(lower.tri(cfMatrix$table)))

acc
precision
cfMatrix
