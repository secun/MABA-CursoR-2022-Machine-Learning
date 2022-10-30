#Ejercicio 2.3

#Importamos las siguientes librerías

library(ggplot2)
library(dplyr)

remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Data <- read.csv("EjercicioFinal.csv")

#Variables del fichero de datos
head(Data)
summary(Data)
str(Data)
#Entendiendo el dataset
#* **Age**: edad en años del asegurado. Será una variable numérica continua con números enteros
#* **Sex**: sexo del asegurado y tomará 2 posibles valores (male y female). Como veis, no es necesario que tome valores 0 y 1
#* **BMI**: IMC del asegurado
#* **Children**: número de niños cubiertos por la póliza de seguros. Valor entre 0 y 5
#* **Smoker**: variable dicotómica sobre si es fumador o no (True/False)
#* **Region**: zona de residencia del asegurado
#* **Charges**: coste del asegurado para la compañía. Muy importante, esta será la variable que queremos predecir, es decir, será nuestra variable Y
#¿Cómo sabemos qué valores pueden tomar las variables categóricas? Podemos hacer uso del comando "table"
#Variables categoricas
table(Data$sex)
table(Data$smoker)
table(Data$region)

## EDA - Antes de comenzar 
# Creamos un scatterplot que muestre la relación entre edad/IMCA y el coste del seguro

ggplot(Data, aes(age,charges)) +
  geom_point()+
  ggtitle("Charges vs Age")

ggplot(Data, aes( bmi, charges)) +
  geom_point()+
  ggtitle("Charges vs BMI")


ggplot(Data, aes( bmi,age)) +
  geom_point()+
  ggtitle("Age vs BMI")

ggplot(Data, aes(age, charges, color=bmi)) +
  geom_point()+
  ggtitle("Charges vs Age / BMI")

#conclusion: Age vs charge parece existir cierta relaci? Charges vs BMi pudiera,quizas? 
#Pintando tres variables no queda claro

ggplot(Data, aes(age, charges, color=sex)) +
  geom_point()+
  ggtitle("Charges vs Age / Sex")

ggplot(Data, aes(age, charges, color=children)) +
  geom_point()+
  ggtitle("Charges vs Age / Children")

# Representamos una distribución en un histograma de cómo se distribuyen los registros. ¿Qué podemos decir acerca de este histograma?
hist(Data$charges)

# Scatterplot matrix - Otra forma de visualizar todo lo anterior es en forma de un scatterplot matrix. Esta representación es igual que las anteriores
# pero en el caso de tener muchos pares de variables no permite de un simple vistazo observar qué relaciones podemos encontrar
# entre los pares de variables

colors <- c('blue', 'red')[unclass(as.factor(Data$sex))]
pairs(Data[c(1,3,4,7)], col = colors)

colors <- c('blue', 'red')[unclass(as.factor(Data$smoker))]
pairs(Data[c(1,3,4,7)], col = colors)

#PREGUNTA: 
# ?Podemos dcir algo de las variable mas significativas?
#?Podemos decir algo ya acerca de los coeficientes beta? ¿Podemos descargar alguna variable?

## Linear Regression Model par apredecir la variable charges
set.seed(123)  #reproducibilidad
#Elegimos una "semilla". Esto lo que hará es que siempre obtendremos los mismos resultados a pesar de que haya métodos que dependan
#de la generación de números aleatorios

#Train and test
#Nos quedamos con el 80% de los registros tomados de forma aleatoria para nuestro set de entrenamientos
n_train <- round(0.8 * nrow(Data)) 
?sample
train_indices <- sample(1:nrow(Data), n_train)

#Creamos nuestro set de entrenamiento
Data_train <- Data[train_indices, ]
#Creamos nuestro set de test
Data_test <- Data[-train_indices, ]

#Creamos y entrenamos el primer modelo con la función lm, 
?lm

#Definimos la fórmula que tomará nuestra regresión lineal. La primera variable será la que queramos predecir, mientras
# que las otras se indicarán como sumas detrás del ~
formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")
model_0 <- lm(formula_0, data = Data_train)
#Vemos modelo
summary(model_0)
#Observad el one-hot encoding en el resultado del modelo



#Guardamos el valor de R2. Los "objetos" en R pueden tener muchos valores dentro. En nuestro caso, el "modelo" como resultado
#no es un número sino un "objeto" y tendrá muchos parámetros dentro a los que podremos acceder con el símbolo $
#Es importante entender que el resultado del modelo no es una "caja negra" sino que podremos acceder a él
r_sq_0 <- summary(model_0)$r.squared

#Bien, ya hemos entrenado el modelo (como vimos en la introducción, el objetivo de machine learning es entrenar un modelo con datos y luego hacer algo con él)
#Pues bien, nuestro modelo ya ha aprendido, ahora vamos a preguntarle y ver cuánto ha aprendido

#Realizamos predicciones para el dataset de test
prediction_0 <- predict(model_0, newdata = Data_test)

#Calculamos los residuos (diferencia entre valor real y la predicci?n)
residuals_0 <- Data_test$charges - prediction_0
qqnorm(residuals_0)
qqline(residuals_0, col = "steelblue", lwd = 2) #los residuos no siguen distribuci?n normal

#Calculamos el Root Mean Squared Error como medida de la cantidad de residuos
rmse_0 <- sqrt(mean(residuals_0^2))


#PREGUNTAS:
  ##¿Qué podemos decir acerca de este modelo? ¿Es bueno? ¿Funciona?
  ##¿Cómo podemos mejorar el modelo? ¿Podemos eliminar alguna variable sin perder información?

### Train and Test New Model
#Definimos una nueva fórmula para el nuevo modelo, eliminando variables menos significativas (sex)
formula_1 <- as.formula("charges ~ age + bmi + children + smoker")

#Entrenamos el modelo al igual que la vez anterior
model_1 <- lm(formula_1, data = Data_train)
summary(model_1)
r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)

residuals_1 <- Data_test$charges - prediction_1
qqnorm(residuals_1)
qqline(residuals_1, col = "steelblue", lwd = 2) #Poco cambio sobre el anterior

#Calculamos el Root Mean Squared Error como medida de la cantidad de residuos
rmse_1 <- sqrt(mean(residuals_1^2))

### Comparemos los modelos

#¿Qué podemos decir acerca de los residuos? ¿Cumplen la condición de normalidad?

print(paste0("R-squared for first model:", round(r_sq_0, 4)))
print(paste0("R-squared for new model: ", round(r_sq_1, 4)))
print(paste0("RMSE for first model: ", round(rmse_0, 2)))
print(paste0("RMSE for new model: ", round(rmse_1, 2)))

#¿Qué podemos decir acerca de estos modelos?
  #¿Cuál es mejor de los dos?
  #¿Ha sucedido algo al eliminar la variable sex?


# Model Performance
#Dibujamos la recta de regresión junto con los valores reales (predicción vs. valores reales)
Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

#Dibujamos los residuos
Data_test$residuals <- residuals_1

ggplot(data = Data_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")

#Veamos cómo se distribuyen los residuos
ggplot(Data_test, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "blue") +
  ggtitle("Histogram of residuals")

#Una vez que nuestro modelo ha aprendido, veamos cómo predice

#1. **Bob**: 19 years old, BMI 27.9, has no children, smokes, from northwest region.

#2. **Lisa**: 40 years old, BMI 50, 2 children, doesn't smoke, from southeast region.

#3. **John**: 30 years old. BMI 31.2, no children, doesn't smoke, from northeast region.

Bob <- data.frame(age = 19,
                  bmi = 27.9,
                  children = 0,
                  smoker = "yes",
                  region = "northwest")
print(paste0("El coste del seguro de Bob será: ", round(predict(model_1, Bob), 2)))

Lisa <- data.frame(age = 40,
                   bmi = 50,
                   children = 2,
                   smoker = "no",
                   region = "southeast")
print(paste0("El coste del seguro de Lisa será: ", round(predict(model_1, Lisa), 2)))

John <- data.frame(age = 30,
                   bmi = 31.2,
                   children = 0,
                   smoker = "no",
                   region = "northeast")
print(paste0("El coste del seguro de John será: ", round(predict(model_1, John), 2)))

