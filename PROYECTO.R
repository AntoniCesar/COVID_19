rm(list = ls())

# cargamos las librerias
library(PerformanceAnalytics)
library(GGally)
library(lattice)
library(cptcity)
library(psych)
library(readxl)
library(mice)
library(VIM)
library(tidyverse)
library(dplyr)
library(factoextra)
library(NbClust)



#  leemos la data 
data <- read_xls("Mundial.xls") %>% 
  select(-Paises, -Casos, -Poblacion, -Death)

# VER SI LA DATA ESTA COMPLETA

mice::md.pattern(data, rotate.names = TRUE)
VIM::aggr(data, number = T, sortVar = T)

# BOXPLOT  PARA VER LA DISTRIBUCIÓN

# Estandarizamos con scale
scale_data <- scale(data)
cor(scale_data)
cov <- cov(scale_data)
diag(cov) %>% sum()

# Calculo del eigenvalues and eigenvector

ev <- eigen(cov)
sum(ev$values)


# calculo de las cargas y score


  
# JUSTIFICAR 

mtx <- cor(scale_data)
chart.Correlation(scale_data, histogram = T, pch = 20)

ggpairs(as.data.frame(scale_data))

levelplot(mtx)
levelplot(
  mtx,
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = F
    )
)


# PCA 
library(ade4)
pca <-
  dudi.pca(
    scale_data,
    scale = F, scannf = F,
    nf = ncol(scale_data)
  )

summary(pca)


# valores propios
pca$eig
sum(pca$eig)

# vectores propios
pca$c1

# Scree plot
library(factoextra)
fviz_eig(pca, addlabels = T)

# correlaciones entre variables y CP
pca$co
levelplot(
  as.matrix(pca$co),
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = F
    )
)



# contribución de variables a las Componentes
# cuadrado de las componenetes
contrib <- as.matrix(pca$co * pca$co)
contrib

corrplot(as.matrix(pca$co), is.corr = F)

library(corrplot)
corrplot(contrib, is.corr = F)


# obteniendo Scores o puntuaciones
as.tibble(scale_data)
head(pca$li)
dim(pca$li)
result <-
  as.tibble(pca$li) %>%
  dplyr::select(sprintf("Axis%1$s", 1:4))









# calculo de las distancia
distancia <- dist(scale(result), method = "euclidean")
as.matrix(distancia) %>% dim()
cluster::daisy(result, metric = "euclidean")

# metodo jerarquico, mediante enlace ward
cluster::agnes()
cluster::diana()
hmodel <- hclust(distancia, method = "ward.D")
plot(hmodel)

# proceso de agrupamiento indicando distancias
hmodel$height
plot(hmodel$height, type = "p")
lines(hmodel$height)
(hmodel$height)[18]


# k clusters
plot(hmodel)
output <- cutree(hmodel, k = 3)
length(output)
table(output)

dplyr::bind_cols(scale(result), cluster = output)


# heatmap
library(rgl)
dist2 <- as.matrix(distancia)
heatmap(dist2)




# boxplote, caracterizacion de clusters
data2 <- dplyr::mutate(as.data.frame(scale(result)), cluster = output)
boxplot(
  data2$Axis1 ~ data2$cluster,
  col = c("blue", "red", "green")
)

boxplot(
  data2$Axis2 ~ data2$cluster,
  col = c("blue", "red", "green")
)










### K-means


## Mejor K-Cluster

set.seed(2021)
factoextra::fviz_nbclust(
  result, kmeans, method = "silhouette",
  k.max = 20
)


set.seed(2021)
factoextra::fviz_nbclust(
  result, kmeans, method = "wss",
  k.max = 20
)


library(NbClust)

nb <-  
  NbClust::NbClust(
    as.matrix(result), diss = NULL, distance = "euclidean",     
    min.nc = 2, max.nc = 20, method = "kmeans", index = "all"
    )
factoextra::fviz_nbclust(nb)



## K-means
set.seed(2021)
model <-
  kmeans(
    x = result, centers = 3, iter.max = 200,
    nstart = 200, algorithm = "Hartigan-Wong",
    trace = F
  )

#  Suma de cuadrados Interclaster 
model$withinss
model$tot.withinss

# valores de silueta
sil <- cluster::silhouette(model$cluster, dist(result))
class(sil)
head(sil)

factoextra::fviz_silhouette(sil) +
  coord_flip() +
  theme_bw()


## k-means++
library(LICORS)

set.seed(2021)

model_02 <-
  LICORS::kmeanspp(
    data = result, k = 3,
    start = "random", iter.max = 100,
    nstart = 100, algorithm = "Hartigan-Wong",
    trace = 0
  )


## Validacion 

# Indice_Davis_Boulding
library(clusterSim)

grupo <- model_02$cluster
index <- clusterSim::index.DB(result, grupo, centrotypes = "centroids")
index$DB


# Indice de Dum
library(clValid)
clValid::dunn(Data = result, clusters = grupo, distance = NULL)
