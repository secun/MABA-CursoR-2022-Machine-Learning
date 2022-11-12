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


###### Bonus track: Repite el ejercicio 8.8 anterior pero con algoritmo PAM #####
library(cluster)
?pam
data_cluster = datos[sample(c(1:500)),7:14] #Para 500 observaciones aleatorias!!!
pam_results = pam(x=data_cluster, 
                  k=5, 
                  metric = "euclidean", 
                  diss = FALSE)

str(pam_results)
pam_results$medoids
head(pam_results$clustering)

###### Bonus track: Entiende como llamar a CLARA#####
library(cluster)
?clara
###### Bonus track- FIN #####

