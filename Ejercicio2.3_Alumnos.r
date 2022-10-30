#Importamos todos los paquetes necesarios (puede importar todos los que vayas a usar)
install.packages("purrr")
install.packages("readr")
install.packages("caret")

library(tidyverse) 
library(caret)
library(corrplot)

library(purrr)


library(readr)

library(dplyr)
library(ggplot2)

#Nombre de los integrantes del grupo:

##----------------------------------------

#Elegimos el directorio de trabajo 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Comprobamos que el directorio de trabajo es el que queremos
getwd()

#Leemos todos los archivos CSV del directorio
data <-  list.files(pattern = "*.csv") %>% map_df(~read_csv(.))

#Comprobamos si hay valores que falten (missing values)
data %>% summarise(across(,~ sum(is.na(.))))

#Nos quedamos únicamente con aquellas filas que no tienen ningún na
data <- na.omit(data)
data %>% summarise(across(,~ sum(is.na(.))))

#Convertimos la variable mpg a l/100km
#Recordemos que consumo l/100km = 235,215/consumo mpg
data$consumo_litros = 235.215/data$mpg

#EDA - Exploratory Data Analysis - Realizad un análisis exploratorio inicial de los datos
#Elegid pares de variables y representadlas en un scatterplot. También podéis añadir otras variables como color para que se pueda obtener más información
#Por ejemplo, podéis representar el precio vs. el kilometraje representando el tipo de combustible según el color

#NOTA: Podéis filtrar por las primeras 500 filas, por ejemplo, si véis que tarda mucho en realizar los cálculos

#########CÓDIGO AQUÍ#########


data %>% mutate(Year = as.factor(year)) %>% ggplot() + geom_boxplot(aes(x=Year, y=price)) + ggtitle("Average Price per year")

colors <- c('blue', 'red', 'green','black','yellow')[unclass(as.factor(data$fuelType))]
labels <- c('Diesel', 'Electric', 'Hybrid','Other','Petrol')[unclass(as.factor(data$fuelType))]


data %>% ggplot() + geom_point(aes(x=mileage, y=price, col = labels)) + ggtitle("Average Price per fuelType")

data %>% select(year, price, transmission, mileage, tax, engineSize, consumo_litros) %>% plot(col = colors)

#PREGUNTA: ¿Qué primeras conclusiones podéis obtener de los datos mostrados?
#PREGUNTA: ¿Qué variable es la que queremos predecir?

##----------------------------------------

######################### División en TRAIN/TEST ########################################33

#Dividimos el dataset en train y test
#Calculamos qué número de registros es un 80% del total del dataset y redondeamos a un número entero
#########CÓDIGO AQUÍ#########

#Nos quedamos con el 80% de los registros tomados de forma aleatoria para nuestro set de entrenamientos
#########CÓDIGO AQUÍ#########

#Creamos nuestro set de entrenamiento
#########CÓDIGO AQUÍ#########
Data_train .....

#Creamos nuestro set de test
#########CÓDIGO AQUÍ#########
Data_test ....

#Definimos la fórmula que tomará nuestra regresión lineal (eliminad la variable model y year)
#########CÓDIGO AQUÍ#########
..... as.formula("price ~ transmission + mileage + fuelType + tax + engineSize + consumo_litros")

#Creamos y entrenamos el primer modelo con la función lm
#########CÓDIGO AQUÍ#########
.... lm(formula = ...., 
        data = -----)

#Observamos un resumen del modelo
#########CÓDIGO AQUÍ#########

#Guardamos el valor de R2

#########CÓDIGO AQUÍ#########

#PREGUNTA: ¿Qué podemos decir en un primer momento del modelo?

##----------------------------------------

#PREGUNTA: Simplifican el modelo, tratando de eliminar una variable del mismo y razonad el porqué

##----------------------------------------


#Realizamos predicciones para el dataset de test

#########CÓDIGO AQUÍ#########

#Calculamos los residuos

#########CÓDIGO AQUÍ#########

#Calculamos el Root Mean Squared Error como medida de la cantidad de residuos

#########CÓDIGO AQUÍ#########

#Dibujad los residuos

#########CÓDIGO AQUÍ#########

#PREGUNTA: ¿Qué podemos decir acerca de los residuos?

##----------------------------------------

##PREGUNTA: generad un nuevo modelo con la variable a eliminar. ¿Se ve afectada la calidad del modelo?