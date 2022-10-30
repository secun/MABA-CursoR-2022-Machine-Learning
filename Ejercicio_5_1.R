#Ejercicio 4.1

#Vamos a crear un árbol de decisión sobre un dataset de supervivientes del Titanic
#Este árbol nos va a permitir clasificar según las características de un pasajero
#si se salvará o no del accidente

#Importamos la libreria caret que contiene los principales modelos
library(caret)    #Classification and Regression Training
library(rpart)
library(rpart.plot)
#library(rnn)

# Asegurar que estamos en el directorio de trabajo correcto
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######  Importamos el dataset  ######
titanic <- read.csv("titanic.csv")
#Mostramos un resumen del dataset
summary(titanic)
str(titanic)

#Las variables que podemos encontrar son:
#survival: para cada pasajero indica si ha sobrevivido o no (1/0)
#pclass: clase en la que viajaba el pasajero 1 =1st, 2 = 2nd, 3 = 3rd
#sex: Sexo del pasajero
#Age: edad del pasajero
#sibsp: número de familiares a bordo
#parch: número de padres/hijos a bordo del titanic
#Ticket: número de billete
#fare: Tarifa
#cabin: Número de camarote
#embarked: Puerto en el que subió el pasajero (C = Cherbourg, Q = Queenstown, S = Southampton)

###### Data transformation #############
# Factorizar la variable Pclass: variable categórica ordinal (la clase 1 es mejor que la 2 y ésta a su vez que la 3)
titanic$Pclass <- ordered(titanic$Pclass,levels=c("3","2","1"))  

# Como los árboles de decisión no pueden trabajar con celdas vacías...
#Identificaos missing values in each column of data frame
summary(titanic) #177 NAs en Age
sapply(titanic, function(x) sum(is.na(x))) #17 NAs en Age
#Opcion 1 - Eliminar filas na.omit
#Opcion 2 - Rellenar dichos huecos: haciendo uso del algoritmo kNN que vimos en la clase anterior
impute <- preProcess(titanic[,c(6:8,10)],method=c("knnImpute")) #seleccion de columnas
titanic_imp <- predict(impute, titanic[,c(6:8,10)])     
#titanic <- cbind(titanic[,-c(6:8,10)], titanic_imp)   
titanic <- cbind(titanic[,-6], Age=titanic_imp[,1])   
sapply(titanic, function(x) sum(is.na(x))) #0 NAs en Age

###### Data preparation #############
#Hacemos split train-test
train_split_idx <- createDataPartition(titanic$Survived, p = 0.8, list = FALSE)
titanic_train <- titanic[train_split_idx, ]
titanic_test <- titanic[-train_split_idx, ]

###### Creando el árbol de decisión ######
options(repr.plot.width = 6, repr.plot.height = 5)
#Primero vamos a predecir basándonos únicamente en el sexo del pasajero #
?rpart # recursive partitioning and regression trees
gender_tree <- rpart(formula = Survived ~ Sex, 
                     data = titanic_train) 

#Dibujamos el árbol de decisión, 
prp(gender_tree,     
    space=4,         
    split.cex = 1.5,
    nn.border.col=0,
    fallen.leaves = TRUE,
    nn = TRUE,
    extra=101)
 
#El valor final indica probabilidad de supervivencia:
table(titanic_train$Sex,titanic_train$Survived)

#Hagamos lo mismo de nuevo pero en este caso añadiendo Pclass
class_tree <- rpart(Survived ~ Sex + Pclass,    
                    data = titanic_train)      

#Dibujamos el modelo
prp(class_tree,     
    space=4,         
    split.cex = 1.2,
    nn.border.col=0,
    fallen.leaves = TRUE,
    nn = TRUE,
    extra=101)


#En último lugar, hagamos lo mismo con todas las variables
complex_tree <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked,
                      cp = 0.001,                 # Set complexity parameter
                      data = titanic_train)       # Use the titanic training data

prp(complex_tree)
prp(complex_tree, extra =1)

#Como podemos ver, los árboles se pueden volver más complejos. Es por ello
#que la función rpart nos permite determinar el número máximo de niveles que tendrá el árbol
titanic_test$Survived = as.factor(titanic_test$Survived)
titanic_train$Survived = as.factor(titanic_train$Survived)
limited_complex_tree <- rpart(Survived ~ Sex + Pclass + Age + SibSp +Fare+Embarked,
                                 data = titanic_train,    # Use the titanic training data  
                                 cp = 0.001,              # Set complexity parameter
                                 maxdepth = 6,            # Set maximum tree depth
                                 minbucket = 5,           # Set min number of obs in leaf nodes
                                 method = "class")        # Return classifications instead of probs
                                

prp(limited_complex_tree)
prp(limited_complex_tree, 
                type = 4,
                extra =1)

######### Explicación de gráfico  ######
# haciendo tablas de contigencia 
table(titanic_train$Survived )
table(titanic_train$Survived,titanic_train$Sex)

#Hoja del árbol que merece la pena analizar
Survivors = titanic_train[titanic_train$Sex == "female" & titanic_train$Pclass!= "3", "Survived"]
table(Survivors)
summary(titanic_train$Age)
####### Interpretabilidad del modelo

#¡Genial! ¡Ya tenemos nuestro modelo! ¿Qué hacemos ahora? ¿Cómo sabemos si es bueno o malo?
#Si recordamos, al tratarse de un modelo de clasificación utilizaremos la matriz de confusión
#sobre el dataset de test

#Predecimos con nuestro árbol para cada pasajero si sobrevivirá o no
test_preds <- predict(limited_complex_tree,              
                      newdata=titanic_test,      
                      type="class") 

titanic_test$dt_pred= test_preds
table(titanic_test$dt_pred, titanic_test$Survived)
#confusionMatrix(titanic_test$dt_pred, titanic_test$Survived)

