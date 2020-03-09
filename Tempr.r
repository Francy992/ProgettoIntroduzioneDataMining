install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")
install.packages("tidyverse")
	 
library("cluster")
library("fpc")
library("tidyverse")
library("factoextra")
library("gridExtra")


load("data.RData")
#Creazione dei dataframe dalle matrici iniziali
 weibulldf <- as.data.frame(weibull.params)
 lognormdf <- as.data.frame(lognorm.params)
 #weibull50df <- as.data.frame(weibull.params[1:50, 1:2])
 #lognorm50df <- as.data.frame(lognorm.params[1:50, 1:2])


#Primo plot esplorativo di Weibull
gg1=ggplot(weibulldf,aes(x=weibulldf[, 1:1],y=weibulldf[, 2:2]))+geom_point(size=4)
gg1 + ggtitle("Plot Weibull.Params Matrix") +
	xlab("Shape") + ylab("Scale")  

# Pulizia dei dati con eliminazione delle righe con Scale > 5000 e stampa dello stesso
weibullRipulito <- as.data.frame(weibull.params)
weibullRipulito = weibullRipulito[weibullRipulito$scale < 5000, ]
gg1=ggplot(weibullRipulito,aes(x=weibullRipulito[, 1:1],y=weibullRipulito[, 2:2]))+geom_point(size=4)
gg1 + ggtitle("Plot Weibull.Params Matrix ripulita") +
	xlab("Shape") + ylab("Scale")  

####################################################CLUSTERING Gerarchico
#----Calcoliamo la matrice delle distanze
dist.weibull=dist(weibullRipulito)

#Applichiamo hclust.
clust.weibull=hclust(dist.weibull)

#Plottiamo i risultati
plot(clust.weibull)

hcd <- as.dendrogram(clust.weibull)
par(mfrow=c(3,1))
plot(hcd, main="Main")
#Taglio ad altezza 12
plot(cut(hcd, h=12)$lower[[2]], 
     main="Taglio Dendogramma ad altezza 12.")

#Conteggio del numero di elementi per ogni cluster:
groupss.13 = cutree(clust.weibull,k=13)
groupss.13

#E sapere quanti elementi per ogni gruppo.
table(groupss.13)


####################################################DBSCAN
#Calcolo del parametro epsilon
set.seed(123)
dbscan::kNNdistplot(weibullRipulito, k =  5)
abline(h = 21, lty = 2)

#Calcolo e plot DbScan utilizzando epsilon scelto tramite grafico
db <- fpc::dbscan(weibullRipulito, eps = 21, MinPts = 5)
# Plot DBSCAN results
plot(db, weibullRipulito, main = "DBSCAN", frame = FALSE)





####################################################K-MEANS
#Funzione per trovare l'andamento di K.
set.seed(123)
wss <- function(k) {
  kmeans(weibullRipulito, k, nstart = 10 )$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Numero di Cluster K",
       ylab="Somma totale dei quadrati all'interno dei cluster")

#Applicazione algoritmo con il miglior K (5)
km.out=kmeans(weibullRipulito[,1:2],centers=5,nstart=25)
weibullRipulito$cluster = as.factor(km.out$cluster)
gg2=ggplot(weibullRipulito,aes(x=weibullRipulito[, 1:1],y=weibullRipulito[, 2:2], color = cluster))+geom_point(size=4)

gg2+geom_point(aes(x=km.out$center[1,1],y=km.out$center[1,2]),size=5,color="black")+
	 geom_point(aes(x=km.out$center[2,1],y=km.out$center[2,2]),size=5,color="black")+
	 geom_point(aes(x=km.out$center[3,1],y=km.out$center[3,2]),size=5,color="black")+
	 geom_point(aes(x=km.out$center[4,1],y=km.out$center[4,2]),size=5,color="black")+
	 geom_point(aes(x=km.out$center[5,1],y=km.out$center[5,2]),size=5,color="black")+
	 annotate("text", x=km.out$center[1,1],y=km.out$center[1,2]-0.2, label = "Centroid 1")+
     annotate("text", x=km.out$center[2,1],y=km.out$center[2,2]-0.2, label = "Centroid 2")+
     annotate("text", x=km.out$center[3,1],y=km.out$center[3,2]-0.2, label = "Centroid 3")+
     annotate("text", x=km.out$center[4,1],y=km.out$center[4,2]-0.2, label = "Centroid 4")+
     annotate("text", x=km.out$center[5,1],y=km.out$center[5,2]-0.2, label = "Centroid 5")+
     ggtitle("k-means clustering, k=5")+ theme(plot.title = element_text(size = rel(5))) +xlab("Shape") + ylab("Scale")  





######################################################################
######################################################################
###########################Lognorm.params#############################
######################################################################
######################################################################
######################################################################
#Primo plot esplorativo di lognorm.params
gg1=ggplot(lognormdf,aes(x=lognormdf[, 1:1],y=lognormdf[, 2:2]))+geom_point(size=4)
gg1 + ggtitle("Plot Lognorm.params Matrix") +
	xlab("Meanlog") + ylab("Sdlog")  

####################################################CLUSTERING Gerarchico
#----Calcoliamo la matrice delle distanze
dist.lognorm=dist(lognormdf)

#Applichiamo hclust.
clust.lognorm=hclust(dist.lognorm)

#Plottiamo i risultati
plot(clust.lognorm)

hcd <- as.dendrogram(clust.lognorm)
par(mfrow=c(3,1))
plot(hcd, main="Main")
#Taglio ad altezza 12
plot(cut(hcd, h=1)$lower[[2]], 
     main="Taglio Dendogramma ad altezza 1.")

#Conteggio del numero di elementi per ogni cluster:
groupss.30 = cutree(clust.lognorm,k=30)
groupss.30

#E sapere quanti elementi per ogni gruppo.
table(groupss.30)


####################################################DBSCAN
#Calcolo del parametro epsilon
set.seed(123)
dbscan::kNNdistplot(lognormdf, k =  10)
abline(h = 21, lty = 2)

#Calcolo e plot DbScan utilizzando epsilon scelto tramite grafico
db <- fpc::dbscan(lognormdf, eps = 0.12, MinPts = 10)
# Plot DBSCAN results
plot(db, lognormdf, main = "DBSCAN", frame = FALSE)



####################################################K-MEANS
#Funzione per trovare l'andamento di K.
set.seed(123)
wss <- function(k) {
  kmeans(lognormdf, k, nstart = 10 )$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Numero di Cluster K",
       ylab="Somma totale dei quadrati all'interno dei cluster")

#Applicazione algoritmo con il miglior K (5)
lognormdf.out=kmeans(lognormdf[,1:2],centers=10,nstart=25)
lognormdf$cluster = as.factor(lognormdf.out$cluster)
gg2=ggplot(lognormdf,aes(x=lognormdf[, 1:1],y=lognormdf[, 2:2], color = cluster))+geom_point(size=4)

gg2+geom_point(aes(x=lognormdf.out$center[1,1],y=lognormdf.out$center[1,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[2,1],y=lognormdf.out$center[2,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[3,1],y=lognormdf.out$center[3,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[4,1],y=lognormdf.out$center[4,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[5,1],y=lognormdf.out$center[5,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[6,1],y=lognormdf.out$center[6,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[7,1],y=lognormdf.out$center[7,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[8,1],y=lognormdf.out$center[8,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[9,1],y=lognormdf.out$center[9,2]),size=5,color="black")+
	 geom_point(aes(x=lognormdf.out$center[10,1],y=lognormdf.out$center[10,2]),size=5,color="black")+
	 annotate("text", x=lognormdf.out$center[1,1],y=lognormdf.out$center[1,2]-0.2, label = "Centroid 1")+
     annotate("text", x=lognormdf.out$center[2,1],y=lognormdf.out$center[2,2]-0.2, label = "Centroid 2")+
     annotate("text", x=lognormdf.out$center[3,1],y=lognormdf.out$center[3,2]-0.2, label = "Centroid 3")+
     annotate("text", x=lognormdf.out$center[4,1],y=lognormdf.out$center[4,2]-0.2, label = "Centroid 4")+
     annotate("text", x=lognormdf.out$center[5,1],y=lognormdf.out$center[5,2]-0.2, label = "Centroid 5")+
     annotate("text", x=lognormdf.out$center[6,1],y=lognormdf.out$center[6,2]-0.2, label = "Centroid 6")+
     annotate("text", x=lognormdf.out$center[7,1],y=lognormdf.out$center[7,2]-0.2, label = "Centroid 7")+
     annotate("text", x=lognormdf.out$center[8,1],y=lognormdf.out$center[8,2]-0.2, label = "Centroid 8")+
     annotate("text", x=lognormdf.out$center[9,1],y=lognormdf.out$center[9,2]-0.2, label = "Centroid 9")+
     annotate("text", x=lognormdf.out$center[10,1],y=lognormdf.out$center[10,2]-0.2, label = "Centroid 10")+
     ggtitle("k-means clustering, k=10")+ theme(plot.title = element_text(size = rel(5))) +xlab("Meanlog") + ylab("Sdlog")  














#First kmeans
 gg1=ggplot(weibulldf,aes(x=weibulldf[, 1:1],y=weibulldf[, 2:2]))+geom_point(size=4)
 gg1 + ggtitle("Plot Weibull.Params Matrix") +
  xlab("Shape") + ylab("Scale")  





 km.out=kmeans(weibull50df[,1:2],centers=2,nstart=20)
 km.out

#Passo i centroidi nell'oggetto originale.
 weibull50df$cluster = as.factor(km.out$cluster)

#Plotto il risultato tutto di un colore
 gg2=ggplot(weibull50df,aes(x=weibull50df[1:50, 1:1],y=weibull50df[1:50, 2:2], color = cluster))+geom_point(size=4)

 gg2+geom_point(aes(x=km.out$center[1,1],y=km.out$center[1,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[2,1],y=km.out$center[2,2]),size=5,color="black")+
     annotate("text", x=km.out$center[1,1],y=km.out$center[1,2]-0.2, label = "Centroid 1")+
     annotate("text", x=km.out$center[2,1],y=km.out$center[2,2]-0.2, label = "Centroid 2")+
     ggtitle("k-means clustering, k=2")+ theme(plot.title = element_text(size = rel(2)))
	 

weibullRipulito <- as.data.frame(weibull.params)
weibullRipulito = weibullRipulito[weibullRipulito$scale < 5000, ]

 # More complex
 #clusplot(weibullRipulito, km.out$cluster, color=TRUE, shade=TRUE, 
  # labels=2, lines=0)
  
 #------------------------------ 3 
km.out=kmeans(weibullRipulito[,1:2],centers=15,nstart=20)
km.out

#Passo i centroidi nell'oggetto originale.
 weibullRipulito$cluster = as.factor(km.out$cluster)

#Plotto il risultato tutto di un colore
 gg2=ggplot(weibullRipulito,aes(x=weibullRipulito[, 1:1],y=weibullRipulito[, 2:2], color = cluster))+geom_point(size=4)

 gg2+geom_point(aes(x=km.out$center[1,1],y=km.out$center[1,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[2,1],y=km.out$center[2,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[3,1],y=km.out$center[3,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[4,1],y=km.out$center[4,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[5,1],y=km.out$center[5,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[6,1],y=km.out$center[6,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[7,1],y=km.out$center[7,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[8,1],y=km.out$center[8,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[9,1],y=km.out$center[9,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[10,1],y=km.out$center[10,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[11,1],y=km.out$center[11,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[12,1],y=km.out$center[12,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[13,1],y=km.out$center[13,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[14,1],y=km.out$center[14,2]),size=5,color="black")+
     geom_point(aes(x=km.out$center[15,1],y=km.out$center[15,2]),size=5,color="black")+
     annotate("text", x=km.out$center[1,1],y=km.out$center[1,2]-0.2, label = "Centroid 1")+
     annotate("text", x=km.out$center[2,1],y=km.out$center[2,2]-0.2, label = "Centroid 2")+
     annotate("text", x=km.out$center[3,1],y=km.out$center[3,2]-0.2, label = "Centroid 3")+
     annotate("text", x=km.out$center[4,1],y=km.out$center[4,2]-0.2, label = "Centroid 4")+
     annotate("text", x=km.out$center[5,1],y=km.out$center[5,2]-0.2, label = "Centroid 5")+
     annotate("text", x=km.out$center[6,1],y=km.out$center[6,2]-0.2, label = "Centroid 6")+
     annotate("text", x=km.out$center[7,1],y=km.out$center[7,2]-0.2, label = "Centroid 7")+
     annotate("text", x=km.out$center[8,1],y=km.out$center[8,2]-0.2, label = "Centroid 8")+
     annotate("text", x=km.out$center[9,1],y=km.out$center[9,2]-0.2, label = "Centroid 9")+
     annotate("text", x=km.out$center[10,1],y=km.out$center[10,2]-0.2, label = "Centroid 10")+
     annotate("text", x=km.out$center[11,1],y=km.out$center[11,2]-0.2, label = "Centroid 11")+
     annotate("text", x=km.out$center[12,1],y=km.out$center[12,2]-0.2, label = "Centroid 12")+
     annotate("text", x=km.out$center[13,1],y=km.out$center[13,2]-0.2, label = "Centroid 13")+
     annotate("text", x=km.out$center[14,1],y=km.out$center[14,2]-0.2, label = "Centroid 14")+
     annotate("text", x=km.out$center[15,1],y=km.out$center[15,2]-0.2, label = "Centroid 15")+
     ggtitle("k-means clustering, k=3")+ theme(plot.title = element_text(size = rel(5)))
	 
	 
	 
	 
# DBSCAN - Il valore minpts dovrebbe essere 3 ossia D + 1. Essendo i dati numerosi è stato deciso di assegnare 5.


#Pulizia database - Infatti vi sono pochi campioni con scale > 5000 ma che plottati graficamente fanno perdere significato al tutto. Rimosis.

weibullRipulito <- as.data.frame(weibull.params)
weibullRipulito = weibullRipulito[weibullRipulito$scale < 5000, ]

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(weibullRipulito, eps = 15, MinPts = 5)
# Plot DBSCAN results
plot(db, weibullRipulito, main = "DBSCAN", frame = FALSE)

dbscan::kNNdistplot(weibullRipulito, k =  5)

abline(h = 0.15, lty = 2)





#Clustering gerarchico:
#Ripuliamo i dati.
weibullRipulito <- as.data.frame(weibull.params)
weibullRipulito = weibullRipulito[weibullRipulito$scale < 5000, ]

#----Calcoliamo la matrice delle distanze
dist.weibull=dist(weibullRipulito)

#Applichiamo hclust.
clust.weibull=hclust(dist.weibull)

#Plottiamo i risultati
plot(clust.weibull)


#Per raggruppare i dati:
groupss.13 = cutree(clust.weibull,k=13)
groupss.13

#E sapere quanti elementi per ogni gruppo.
table(groupss.13)





#K-means con calcolo del miglior k:

weibullRipulito <- as.data.frame(weibull.params)
weibullRipulito = weibullRipulito[weibullRipulito$scale < 5000, ]

k3 <- kmeans(weibullRipulito, centers = 3, nstart = 25)
k4 <- kmeans(weibullRipulito, centers = 4, nstart = 25)
k5 <- kmeans(weibullRipulito, centers = 5, nstart = 25)
k6 <- kmeans(weibullRipulito, centers = 6, nstart = 25)
k7 <- kmeans(weibullRipulito, centers = 7, nstart = 25)
k8 <- kmeans(weibullRipulito, centers = 8, nstart = 25)

#Plot diverso dei risultati
p3 <- fviz_cluster(k3, geom = "point",  data = weibullRipulito) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = weibullRipulito) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = weibullRipulito) + ggtitle("k = 5")
p6 <- fviz_cluster(k6, geom = "point",  data = weibullRipulito) + ggtitle("k = 6")
p7 <- fviz_cluster(k7, geom = "point",  data = weibullRipulito) + ggtitle("k = 7")
p8 <- fviz_cluster(k8, geom = "point",  data = weibullRipulito) + ggtitle("k = 8")

library(gridExtra)
grid.arrange(p3, p4, p5, p6, p7, p8, nrow = 2)




#Funzione per trovare l'andamento di K.
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(weibullRipulito, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
	   
#Fine funzione per trovare l'andamento di K
	   
	   
#Traccia cose da scrivere:
scopo del progetto:
	Ossia esplorare la correlazione che è presente nelle due matrici attraverso algoritmi di clustering.
Panoramica tecnologie:
-	Cosa è R.
-	 Cosa è il clustering
	- Gerarchico
	- DBSCAN
	- K-means
- Approccio e risultati ottenuti.
	- E' stata pulita la matrice andando a tagliare gli outlier, applicati tutte e tre i suddetti algoritmi andando a sottolineare
	l' attenzione per il calcolo dei parametri, k in k-means, epsilon e minpts per dbscan. Da capire per il clustering gerarchico.

https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/#parameter-estimation
Metodo per determinare il valore eps ottimale
Il metodo proposto qui consiste nel calcolare le distanze k vicine più vicine in una matrice di punti.

L'idea è di calcolare la media delle distanze di ogni punto dai suoi k vicini più vicini. Il valore di k sarà specificato dall'utente e corrisponde a MinPts .

Successivamente, queste k-distanze vengono tracciate in ordine crescente. L'obiettivo è determinare il "ginocchio", che corrisponde al parametro eps ottimale .

Un ginocchio corrisponde a una soglia in cui si verifica un brusco cambiamento lungo la curva della distanza k.

La funzione kNNdistplot () [nel pacchetto dbscan ] può essere usata per disegnare il grafico della distanza k:




