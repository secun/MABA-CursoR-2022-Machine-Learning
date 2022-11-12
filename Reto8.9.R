################## R Lab ################## 
################## Análisis de clusters ##################
 #----------------------------------------------------------#
 #----------------------------------------------------------#
####### Caso a estudiar - Datos de supermercado ##################

#### Contexto de los datos ####
#https://www.economiadigital.es/empresas/ulabox-resultados-2016_525869_102.html
#https://www.lavanguardia.com/economia/20220427/8224699/unicornio-checo-rohlik-adquiere-ulabox.html

#### 1 - Cargo / importo datos ####
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("df.RData")
datos = dat_UB

#Exploracion rapida de datos
summary(datos)
str(datos) #todos son datos numericos

#### 2 - Data cleansing (tidying) ####
datos <- na.omit(datos) #datos enteramente limpios
#Visualización de datos muestra que ha habido una preparación previa de datos
#   total_items: numero de items comprados
#   discount: %
#   weekday: día de la compra
#   hour: momento de la compra
#   resto: % individual dedicado a diferentes categorías
#   customer: identificación de clientes
#   order: ERP id
# Nos interesan

#### 3 - Transform ####
#datos <- scale(datos) #Notese que los datos (cols 7:14) ya están normalizados por filas

#### 4 - Visualize ####

####### Retos de clase - 8.3 Implementacion de K-means  #################
#### 5 - Model ####
#### 8.9 - Model hierachical - Ward ####
library(cluster)
set.seed(456) #reproducibilidad de resultados
data.points =sample (size=100, x=1:nrow(datos)) #100 random data-points
data_cluster <-  datos[data.points,7:14] #select more rows to see computing demands
?agnes #Agglomerative Nesting

#Model with Manahattan distance, method average
agnes_result = agnes(data_cluster,
                     diss =FALSE, # datos de observaciones
                     metric = "manhattan",  #metrica a utilizar
                     method ="average") #método de clustering
str(agnes_result)
agnes_result$ac #agglomerative clustering index
#agglomerative coefficient: toma valores entre 0 y 1, valores cercanos a 1 significa que hay estructura de cluster en los datos
pltree(agnes_result) # plot tree

#Model with Euclidean distance, method average
agnes_result = agnes(data_cluster,
                     diss =FALSE, 
                     metric = "euclidean", 
                     method ="ward") 
agnes_result$ac #ac has increased
pltree(agnes_result) # plot tree

#¿what's the best method?

#produce dendrogram
pltree(agnes_result, cex = 0.6, hang = -1, main = "Dendrogram")  
#Explicar dendograma: leafs, distancias


#Cut tree
cutree(agnes_result, k = 7)
data_cluster = cbind(data_cluster, 
                     cluster = cutree(agnes_result, k = 7)) 

table(data_cluster$cluster) #Pattern identified
#¿outliers? ¿grupos numerosos?

######### Opcional #########
#### Gráficos de silueta  #######3
#¿Which is the optimun number of clusters then?
si = silhouette(x= cutree(agnes_result, k = 5), # k = 5 
                dist = daisy(data_cluster, metric= "euclidean"))
summary(si)
plot(si, nmax = 80, cex.names = 0.5)


#### 6 - Communicate ####

