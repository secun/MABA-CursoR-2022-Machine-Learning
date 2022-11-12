################## R Lab ################## 
################## Análisis de clusters ##################
 #---------------------------------------------------------- #
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
#   customer: identificacion de clientes
#   order: ERP id
#   total_items: numero de items comprados
#   discount: % descuento total
#   weekday: dia de la compra
#   hour: momento de la compra
#   categorías: % individual dedicado a diferentes categorías (share of wallet)
# Nos interesan ls categorías

#### 3 - Transform ####
#datos <- scale(datos) #Notese que los datos (cols 7:14) ya están normalizados por filas


#### 4 - Visualize ####
####### Retos de clase - 8.3 Comparar matrices de distancia  #################
# --- Inicio resolucion  ---
## Calcular distancia Euclidea/Manhattan para 10 observaciones (filas de datos). 
## Comparar las matrices anteriores,entender el valor de cada elemento de la matriz, entender la diagonal.
## Pensad en ordenaciones basados en ranking,
## Utilizar como features datos de % de compra en las diferentes categorias (variables 7:14)
## Explicar el feature selection previo

?dist
print(dist(datos[1:10,7:14], method = "euclidean"))
print(dist(datos[1:10,7:14], method = "manhattan", upper =TRUE))

#Visualizamos distancia euclidea para 10 observaciones/filas
dst <- as.matrix(dist(datos[1:10,7:14], method = "euclidean",upper=TRUE) )
dim = ncol(dst)
image(1:dim, 1:dim, dst, axes = TRUE, xlab="", ylab="")
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", dst), cex=0.6)
title(main = "Calculo de distancia Euclidea para 10 observaciones", font.main = 4)
#Explicación de matriz de distancias : simetría, graduacion del color vs distancia

#Visualizamos distancia manhattan para 10 observaciones/filas
dst <- as.matrix(dist(datos[1:10,7:14], method = "manhattan",upper=TRUE) )
dim = ncol(dst)
image(1:dim, 1:dim, dst, axes = TRUE, xlab="", ylab="")
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", dst), cex=0.6)
title(main = "Calculo de distancia Manhattan para 10 observaciones", font.main = 4)

#Comentarios: simetría de la matriz, ambos plots son iguales y/o diferentes en algunos valores,... 
#Take-away: la seleccion del tipo de distancia produce matrices diferentes 

#Todo lo anterior se hace mas sencillo de la manera siguiente con una función de R
#Mapa de calor para 10 observaciones/filas
?heatmap
heatmap(as.matrix(dist(datos[1:10,7:14])), # euclidean distance, 
        Colv = NA, Rowv = NA  , #remove dendrogram
        distfun = "euclidean" ,
        scale= "row",   #no es necesario el escalado
        xlab = "Observaciones", 
        ylab = "Observaciones",
        main = "Mapa de calor para 10 observaciones")

# --- Fin resolucion  8.3---

