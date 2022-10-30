#Ejercicio 5.3

library(caret)
library(itree)
library(ModelMetrics)
library(rpart.plot)


# Asegurar que estamos en el directorio de trabajo correcto
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Importamos el dataset
winequality <- read.csv("winequality-red.csv", sep = ";")

#Mostramos un resumen
str(winequality)

#Eliminad la variable quality ya que en este caso vamos a predecir la variable alcohol
winequality = ...
#Dividimos en test y train
set.seed(1)
nobs <- nrow(winequality)
itrain <- sample(nobs, 0.8 * nobs)
train <- winequality[itrain, ]
test <- winequality[-itrain, ]

#Creamos el trainControl con 10 particiones
train_control <- trainControl(...)             # Use 10 partitions , CV

#Definimos los cp que va a utilizar el cross-validation
tune_grid = expand.grid(cp=c(0.1,0.01,0.001,0.0001))


#Entrenamos utilizando cross validation y rpart como modelo
validated_tree <- train(...,               # Required model parameters
                        data=...........,         # Data set
                        method="rpart",           # Model type(decision tree)
                        trControl= ....,          # Model control options
                        tuneGrid = ....,          # Required model parameters
                        maxdepth = 5,             # Additional parameters***
                        minbucket=5)     

#Vemos un resumen del modelo
summary(validated_tree)
validated_tree$coefnames
validated_tree$finalModel
prp(validated_tree$finalModel)

####### CHECK POINT 1  #######

#Hacemos un plot para ver la gráfica de CP vs. RMSE y determinar cuá es el cp óptimo
validated_tree$bestTune
plot(validated_tree) 

#Ahora vamos a comparar nuestro "modelo podado" (cp=0.01) con cross validation vs. 
#un modelo con un cp = 0.00001
#¿que resultado esperas?
#Comparad la predicción tanto sobre el test set como sobre el training set

#Entrenamos los modelos. 
#El modelo tree1 deberá tener un cp = 0.00001, máximo 30 niveles y mínimo 1 registro por nodo

tree1 <- rpart(alcohol ~ ., data=train, ...)

#Sin embargo, el modelo tree2 deberá tener como único requisito un cp = 0.01
tree2 <- rpart(alcohol ~ ., data=train, cp = 0.01)

#Creamos las nuevas columnas con las predicciones
test$y_pred_tree1 = predict(tree1, test[-c(11)])
train$y_pred_tree1 = predict(...., train[-c(11)])
test$y_pred_tree2 = predict(..., ....)
train$y_pred_tree2 = predict(..., ....)


####### CHECK POINT 2  #######


#Calculamos el mse para los datos de test
mse(test$alcohol, test$y_pred_tree1)
mse(test$alcohol, test$y_pred_tree2)

#Calculamos el mse para los datos de train
mse(train$alcohol, train$y_pred_tree1)
mse(train$alcohol, train$y_pred_tree2)

#¿Qué modelo es mejor prediciendo los datos de entrenamiento?

#¿Qué modelo es mejor prediciendo los datos de test?

#¿Qué modelo es mejor y por qué?





