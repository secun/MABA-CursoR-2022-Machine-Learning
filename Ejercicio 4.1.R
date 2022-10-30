############ Ejercicio 4.11 #############
################## Algoritmo k-nn ################
# Librería que contiene método KNN
library(class)

# Librería que contiene métodos de normalización
library(caret)

# Leemos el fichero de datos URL
data = read.csv("https://raw.githubusercontent.com/reisanar/datasets/master/RidingMowers.csv") 

# Normalizamos los valores de entrada
#https://topepo.github.io/caret/pre-processing.html
preproc.values <- preProcess(data[, 1:2], method=c("range"))
class(preproc.values)
data.norm = predict(preproc.values, data[, 1:2])
data.norm$Ownership = data$Ownership

# Mostramos el gráfico
plot(Lot_Size ~ Income, 
     data=data.norm, 
     pch=ifelse(data$Ownership =="Owner", 1, 3), 
     col=ifelse(data$Ownership=="Owner","dark green","dark red"))

legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4), col=c("dark green","dark red","dark blue"))

# Nuevo dato a evaluar
X <- data.frame("Income"=60,"Lot_Size"=18)

# Normalizamos el nuevo dato
X.norm <- predict(preproc.values, X)
# Mostramos el nuevo dato en el gráfico
text(X.norm, "X", col="Black")

#Creamos modelo y predecimos para k=1
?knn
knn (train=data.norm [,1:2], 	# Conjunto de entrada (Income, Lot_Size)
     test=X.norm, 		# Nuevo dato a evaluar
     cl=data.norm$Ownership, 	# Clases de salida
     k = 1, 		# Número de vecinos a evaluar (k más cercanos)
     prob = TRUE)		# Muestra la proporción de los votos
#Explicar valor de probabilidad

#Creamos modelo y predecimos para k=3
knn (train=data.norm [,1:2], 	# Conjunto de entrada (Income, Lot_Size)
     test=X.norm, 		# Nuevo dato a evaluar
     cl=data.norm$Ownership, 	# Clases de salida
     k = 3, 		# Número de vecinos a evaluar (k más cercanos)
     prob = TRUE)		# Muestra la proporción de los votos
#Explicar valor de probabilidad

  #Creamos modelo y predecimos para k=6
knn (train=data.norm [,1:2], 	# Conjunto de entrada (Income, Lot_Size)
     test=X.norm, 		# Nuevo dato a evaluar
     cl=data.norm$Ownership, 	# Clases de salida
     k = 6, 		# Número de vecinos a evaluar (k más cercanos)
     prob = TRUE)		# Muestra la proporción de los votos
#Explicar valor de probabilidad


############ Ejercicio 4.12 #############
##############  Seleccion de mejor valor de K #################
# Se elige 80% de índices aleatorios
set.seed(123)
train.idx <- sample(1:nrow(data),size=nrow(data)*0.8,replace = FALSE)

# conjunto de entrenamiento
train <- data[train.idx,] # 80% training data
# conjunto de validación
valid <- data[-train.idx,] # remaining data

# normalización de los valores
preproc.values <- preProcess(train[, 1:2], method=c("range"))
# conjunto entrenamiento normalizado
train.norm <- predict(preproc.values, train[, 1:2])
train.labels <- data[train.idx,3] # etiquetas del conjunto de entrenamiento

# conjunto y etiquetas de test
test.norm <- predict(preproc.values, valid[, 1:2])
test.labels <- data[-train.idx,3]

# vector donde dejaremos la precisión de la clasificación
acc <- vector("numeric",nrow(train.norm))

# Para cada valor de entrenamiento
for (i in 1:NROW(train.norm)) {
  # sacamos el modelo con k=i para la clasificación de datos de validación
  mod <- knn (train=train.norm, 
              test=test.norm, 
              cl=train.labels, 
              k=i)

    # se calcula la precisión en los aciertos
  acc[i] <- 100*sum(test.labels == mod) / nrow(test.norm)
}

# Se muestra la gráfica
plot(acc, type="b", xlab="K- Value",ylab="Accuracy level %")

which.max(acc)

