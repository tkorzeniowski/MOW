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
dataToJ48Classification <- mushroomDataset

data <- mushroomDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.9))
#idTrainData <- sample(2, nrow(data), replace=TRUE, prob=c(0.9, 0.1))
trainData <-data[idTrainData,]
trainClasses <- matrix(1, nrow(trainData), 1)
trainData[,1] <- NULL
testData <-data[-idTrainData,]
testData[,1] <- 1 

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,1])
testData[,1] <- NULL


############################



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

idTrainData <- unlist(createDataPartition(data[,1], p=0.7))
trainData <-data[idTrainData,]
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,1])
testData[,1] <- NULL



############################


sensorDataset <- read.table('SensorlessDriveDiagnosis.txt')

anomalies <- sensorDataset[which(sensorDataset[,49]==11),]
sensorDataset <- sensorDataset[-which(sensorDataset[,49]==11),]

data <- sensorDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.8))
trainData <-data[idTrainData,]
dataToJ48Classification <- trainData
trainClasses <- trainData[,49]
trainData[,49] <- NULL
testData <-data[-idTrainData,]

testData <- rbind(testData, anomalies)
testClasses <- as.numeric(testData[,49])
testData[,49] <- NULL



############################
# wyniki implementacji
#mushroom
CGResult <- createGroups(trainData, 1, 'kmeans')
predictResult <- predict(CGResult, testData, testClasses)

#letter
CGResult <- createGroups(trainData, 25, 'kmeans')
predictResult <- predict(CGResult, testData, testClasses)

#sensor
CGResult <- createGroups(trainData, 10, 'kmeans')
predictResult <- predict(CGResult, testData, testClasses)

############################

"!!! 
ogÃ³lnie nie wiadomo ktora etykieta bedzie odpowiednia dla klasy, tzn. w przypadku grzybow 
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

#J48

# mushroom
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
idTrainData <- unlist(createDataPartition(data[,1], p=0.5))
trainData <-data[idTrainData,]
dataToJ48Classification <- trainData
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]
testClasses <- testData[,1]
testData[,1] <- NULL
resultJ48 <- J48(V1~., data = dataToJ48Classification, control= Weka_control(M=15))#


#letter
letterDataset <- as.matrix(read.table('letterRecognition.txt', sep = ','))
for(i in 1:nrow(letterDataset)){
  letterDataset[i,1] <- which(letterDataset[i,1] == LETTERS)
}
letterDataset <- as.data.frame(letterDataset)
letterClasses <- as.numeric(letterDataset[,1])
data <- letterDataset
dataToJ48Classification <- letterDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.7))
trainData <-data[idTrainData,]
trainClasses <- trainData[,1]
trainData[,1] <- NULL
testData <-data[-idTrainData,]
testClasses <- as.numeric(testData[,1])
testData[,1] <- NULL
resultJ48 <- J48(V1~., data = dataToJ48Classification, control= Weka_control(M=15))#


#sensor
sensorDataset <- read.table('SensorlessDriveDiagnosis.txt')
dataToJ48Classification <- sensorDataset

data <- sensorDataset
idTrainData <- unlist(createDataPartition(data[,1], p=0.6))
trainData <-data[idTrainData,]
trainClasses <- trainData[,49]
trainData[,49] <- NULL
testData <-data[-idTrainData,]
testClasses <- as.numeric(testData[,49])
testData[,49] <- NULL

dataToJ48Classification$V49 <- as.factor(dataToJ48Classification$V49)
resultJ48 <- J48(V49~., data = dataToJ48Classification, control= Weka_control(M=15))#

pred <- predict(resultJ48, testData)
v <- data.frame(matrix(0, nrow(testData), 2))
v[,1] <- pred
v[,2] <- testClasses
acc <- length(which(v[,1]==v[,2]))/nrow(testData)
acc




# knn

# normalizacja

train_normalized <- trainData;
test_normalized <- testData;

# Funkcja Normalization z pakietu clusterSim teoretycznie powinna normalizowaæ dane, ale 
# z jakiegoœ powodu mi nie dzia³a i nie daje ¿adnej poprawy skutecznoœci knn, wiêc 
# poni¿ej zrobi³am normalizacjê "rêcznie"
#train_normalized <- data.Normalization (train_normalized,type="n1",normalization="column")


# mushroom & letter
train_normalized = as.matrix(as.data.frame(lapply(train_normalized, as.numeric)))#

means_train = colMeans(train_normalized)
train_normalized = data.matrix(train_normalized)
sds_train = colSds(train_normalized)

train_normalized <- as.data.frame(train_normalized)
means_train <- as.data.frame(means_train)
sds_train <- as.data.frame(sds_train)

for(i in 1:nrow(means_train)){
  for(j in 1:nrow(train_normalized)){
    train_normalized[j,i] = (train_normalized[j,i] - means_train[i,])/sds_train[i,]
  }
  for(j in 1:nrow(test_normalized)){
    test_normalized[j,i] = (test_normalized[j,i] - means_train[i,])/sds_train[i,]
  }
}

#knnAlg <- knn(trainData, testData, trainClasses, k = 3, l = 1) # problem niewlasciwych etykiet?
knnAlg <- knn(train_normalized, test_normalized, trainClasses, k = 3, l = 1)
v <- data.frame(matrix(0, nrow(testData), 2))
v[,1] <- knnAlg
v[,2] <- testClasses
acc <- length(which(v[,1]==v[,2]))/nrow(testData)
acc
