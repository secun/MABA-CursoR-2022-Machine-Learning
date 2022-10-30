##Ejemplo 2.1


####### Parte 1 ###### 
#Ajuste y predicción
library(readxl)

#Entorno
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Importamos el dataset

dataset <- read_excel("RegresionLineal_Casas.xlsx")

#Vemos un resumen del dataset
summary(dataset)
str(dataset)
#Hacemos un scatterplot para ver la relación
#https://www.tutorialspoint.com/r/r_scatterplots.htm
plot(dataset$M2, dataset$Precio, main = "Relación Precio-M2",
     xlab = "Metros Cuadrados",
     ylab = "Precio",
     xlim = c(50,200),
     ylim = c(250000, 1000000))

#Veamos la correlación entre las dos variables, recordad de Business Analytics
cor(dataset$M2, dataset$Precio)

#Creamos la regresión lineal
#http://r-statistics.co/Linear-Regression.html

linearMod <- lm(formula = Precio ~ M2, data=dataset)
print(linearMod)

#Predecimos el valor de la casa que queremos comprar
casa_a_predecir <- list(M2=c(150))

predict(linearMod, casa_a_predecir)


####### Parte 2 ###### 
#Valoración del modelo
summary(linearMod)

#Diagnóstico del modelo
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(linearMod)
