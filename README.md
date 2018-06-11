## MOW
Metody odkrywania wiedzy 18L

### Temat
Nienadzorowana detekcja anomalii za pomocą odpowiednio opakowanych wybranych algorytmów grupowania dostępnych w R. Porównanie z nadzorowaną detekcją anomalii za pomocą dostępnych w R algorytmów klasyfikacji.

### Algorytmy grupowania
 * k-średnich ([kmeans](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html))
 * k-medoidów ([pam](http://ugrad.stat.ubc.ca/R/library/cluster/html/pam.html))
 * hiererchiczny ([agnes](https://stat.ethz.ch/R-manual/R-patched/library/cluster/html/agnes.html))

### Wskaźniki nietypowości
Określanie czy dany przykład jest anomalią na podstawie wskaźników CBLOF, uCBLOF, LDCOF opisanymi w [artykule](https://www.goldiges.de/publications/Anomaly_Detection_Algorithms_for_RapidMiner.pdf)


### Klasyfikacja
 * drzewa decyzyjne ([J48](https://cran.r-project.org/web/packages/RWeka/RWeka.pdf))
 * algorytm kNN ([knn](https://stat.ethz.ch/R-manual/R-devel/library/class/html/knn.html))
