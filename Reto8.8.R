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

####### Retos de clase - 8.8.1 Implementacion de K-means  #################
#### 5 - Model ####
#### 5.1 - Model partitioning - K-Means ####
set.seed(456) #reproducibilidad de resultados
data.points =sample (size=500, x=1:nrow(datos)) #500 random data-points
data_cluster = datos[data.points,7:14] #Para 500 observaciones aleatorias!!!
?kmeans
kmeans_result = kmeans(x=data_cluster, 
                       centers = 5, #numero de clusters
                       iter.max = 10,  #numero maximo de iteraciones
                       nstart = 25) # conjuntos aleatorios

# ¿Qué es eso de nstart?
# Es el número de configuraciones iniciales que se prueban y el algoritmo escoge el mejor.
# Por ejemplo, nstart=25 generará centroides 25 veces y se quedará con la configuración
# que presenta la menor variación intra-clúster.

str(kmeans_result) #explicar resultados

kmeans_result$centers #centroids
kmeans_result$size #numero de observaciones en cada cluster, tamaño del cluster
kmeans_result$withinss #intra-clusters variation


#De estos resultados puede deducirse que los 5 clústeres son de distintos tamaños y, 
#por tanto, tienen distinta variación intra-clúster. 
#Los individuos de algunos clústeres están muy centrados en comprar productos de una 
#sola categoría, como el clúster 1 (Drinks), el clúster 2 (Food) y el clúster 3 (Baby). 

#¿como visualizamos los resultados? Tenemos 7 variables! - PCA - Reducción de variables
#Entender los centroids como una caracterizaciOn de los clusters: los que comen fresco, 
#los que compran en la categoría baby, bebidas,...

#### visualize data
#Create dataset to be plot, with five clusters
center_df <- data.frame(cluster=c(1:5), kmeans_result$centers)
#Reshape the data
center_reshape <- tidyr::gather(center_df, features, values, Food. : Pets.)
head(center_reshape)

# Create the palette
library(RColorBrewer)
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
#plot data
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradient(low="brown", high="yellow")

#¿Cuanto tiempo crees que tardará el algoritmo en ejecutarse para las 30000 observaciones?
#Repetir y comentar resultados: rapido y simple, "leer y comparar" resultados

#El dato puede ser enriquecido con el numero del cluster --> Insight y acción específicas
data_cluster = cbind (data_cluster, cluster= kmeans_result$cluster)
View(data_cluster) #Pattern identified

# --- Fin resolucion  8.8.1 ---


####### Retos de clase - 8.8.2 Optimizacion de k(numero clusters)  #################

# --- Inicio resolucion  ---
#Ejercicio :repite el ejercicio 8.8.1 para 8 clusters. ¿observas algún cambio en los resultados de k-means? 
#Se deben observar distintos clusters (withins, centers, size)

#¿cual es el número optimo de clusters? 
#install.packages("factoextra")
library(factoextra)
?fviz_nbclust

#Hagamos visualización  (método del codo)
fviz_nbclust(x = data_cluster, 
             kmeans,        #Algoritmo
             method = "wss", #Total within cluster sum of squares (función de coste)
             k.max=20)     #max number of cluster
#codo en 4-8

#Hagamos visualización  (método de la silueta promedio)
fviz_nbclust(x=data_cluster, 
             kmeans, 
             method = "silhouette",
             k.max=20)
#Maximo en 8
# --- Fin resolucion  8.8.2 ---
