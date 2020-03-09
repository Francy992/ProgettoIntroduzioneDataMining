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







