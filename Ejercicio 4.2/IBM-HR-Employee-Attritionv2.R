############ Ejercicio 4.2 #############
library(caret)
library(ggplot2)
library(grid)
library(gridExtra)
library(devtools)
library(roxygen2)

# Asegurar que estamos en el directorio de trabajo correcto
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd() # ¿Me devuelve el directorio correcto?
# De lo contrario, modificar con comando setwd() o con el panel inferior derecha

# Load in the data
data.df = read.csv("IBM-HR-Employee-Attrition.csv")
colnames(data.df)[1] <- "Age"

# Visualización de datos
summary(data.df)
str(data.df)
# Eliminamos datos invariantes, variables independientes (no afecan al target), colineales múltiples, 
data <- subset(data.df, select = -c(EmployeeCount,StandardHours,Over18,EmployeeNumber) )
sum(is.na(data))

#Visualizacion de variables
agePlot <- ggplot(data.df,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
travelPlot <- ggplot(data.df,aes(BusinessTravel,fill=Attrition))+geom_bar()
genderPlot <- ggplot(data.df,aes(Gender,fill=Attrition))+geom_bar()
jobLevelPlot <- ggplot(data.df,aes(JobLevel,fill=Attrition))+geom_bar()
jobInvPlot <- ggplot(data.df,aes(JobInvolvement,fill=Attrition))+geom_bar()
marPlot <- ggplot(data.df,aes(MaritalStatus,fill=Attrition))+geom_bar()
numCompPlot <- ggplot(data.df,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
overTimePlot <- ggplot(data.df,aes(OverTime,fill=Attrition))+geom_bar()
perfPlot <- ggplot(data.df,aes(PerformanceRating,fill = Attrition))+geom_bar()
StockPlot <- ggplot(data.df,aes(StockOptionLevel,fill = Attrition))+geom_bar()


grid.arrange(agePlot,travelPlot,jobLevelPlot,genderPlot,jobInvPlot,marPlot, ncol=2,numCompPlot, overTimePlot, perfPlot, StockPlot,  top = "Fig 1")

# Factorización de valores categóricos
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$EducationField <- as.factor(data$EducationField)
data$Department <- as.factor(data$Department)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Gender <- as.factor(data$Gender)
data$JobRole <- as.factor(data$JobRole)
data$OverTime <- as.factor(data$OverTime)

prop.table(table(data$Attrition))

#reproducibilidad
set.seed(92783)

#Perform train / test split on the data
?createDataPartition
train_split_idx <- caret::createDataPartition(data$Attrition, p = 0.8, list = FALSE)
train <- data[train_split_idx, ]
test <- data[-train_split_idx, ]


############## Modelo 1 ###################
#Parámetros del sampling y algoritmo
?trainControl
fitControl1 <- trainControl(method = "cv", 
                            number = 10, # número de paquetes de muestra
                            classProbs = TRUE, # clasificación
                            # reduce la muestra de la clase mayoritaria y sintetiza nuevos puntos de datos en la clase minoritaria
                            sampling = "smote", 
                            summaryFunction = caret::twoClassSummary, # para optimizar métricas 
                            savePredictions = TRUE) # guardamos las predicciones

#Entrenamiento del modelo (paquete THEMIS?)
?train
fit_knn1 <- train(Attrition ~ ., 
                  data=train, # datos de entrenamiento
                  method="knn", # método de estimación
                  trControl = fitControl1,	# estructura de control
                  preProcess = c("range"),	# normalización de los datos
#                 metric = "Sens",    # métrica que se utilizará para seleccionar el modelo óptimo
                  metric = "Accuracy",    # métrica fines didacticos
                 tuneGrid = expand.grid(k = 1:50)) # valores a probar de k

# mostramos la gráfica K vs métrica (validación cruzada)
plot(fit_knn1) #Tiempo espera: 10 minutos
# mostramos mejor tuning
fit_knn1$bestTune

#Make predictions to expose class labels
preds1 <- predict(fit_knn1, newdata=test)

#Pintamos matriz de confusion
matrizconfusion1 =caret::confusionMatrix(as.factor(preds1), as.factor(test$Attrition),positive="Yes")

source("DrawConfusionMatrix.R", # Para poder visualizar la matriz de confusión
       encoding = 'UTF-8')      # Para interpretar bien las tildes

draw_2D_confusion_matrix(cm = matrizconfusion1, 
                         caption = "Matriz de confusión de modelo 1") 

############## Modelo 2 ###################
#Parámetros del sampling y algoritmo
#Entrenamiento
fitControl2 <- trainControl(method = "repeatedcv", # validaciones cruzadas con repetición
                            number = 5, 
                            repeats = 10,
                            classProbs = TRUE, 
                            # muestreo adicional que se realiza después del remuestreo 
                            # (normalmente para resolver los desequilibrios de clase).
                            # sub-conjunto aleatorio de todas las clases en el conjunto de entrenamiento 
                            # para que sus frecuencias de clase coincidan con la clase menos prevalente
                            sampling = "down", 
                            summaryFunction = twoClassSummary,
                            savePredictions = TRUE)


# proceso de entrenamiento (exactamente igual que en el modelo 1)
fit_knn2 <- caret::train(Attrition ~ ., 
                         data=train, # datos de entrenamiento
                         method="knn", # método de estimación
                         trControl = fitControl2,	# estructura de contorl
                         preProcess = c("range"),	# normalización de los datos
#                 metric = "Sens",    # métrica que se utilizará para seleccionar el modelo óptimo
                         metric = "Accuracy",    # métrica fines didacticos
                         tuneGrid = expand.grid(k = 1:50)) # valores a probar de k
#10 minutos de espera
# mostramos la gráfica K vs métrica (validación cruzada)
plot(fit_knn2) 
fit_knn2$bestTune
#Make predictions to expose class labels
preds2 <- predict(fit_knn2, newdata=test)

matrizconfusion2 =confusionMatrix(as.factor(preds2), as.factor(test$Attrition),positive="Yes")

source("DrawConfusionMatrix.R", # Para poder visualizar la matriz de confusión
       encoding = 'UTF-8')      # Para interpretar bien las tildes

draw_2D_confusion_matrix(cm = matrizconfusion2, 
                         caption = "Matriz de confusión de modelo 2") 

#Ejercicio: Comentar y comparar  matriz 1 vs matriz 2

############### curvas ROC PARA COMPARAR LOS MODELOS ###############
library(plyr)
library(pROC)

# ROC del primero model
rocs_fit1 <- llply(unique(fit_knn1$pred$obs), function(cls) {
  roc(response = fit_knn1$pred$obs==cls, predictor = fit_knn1$pred[,as.character(cls)])
})

# ROC del segundo model
rocs_fit2 <- llply(unique(fit_knn2$pred$obs), function(cls) {
  roc(response = fit_knn2$pred$obs==cls, predictor = fit_knn2$pred[,as.character(cls)])
})

# Mostramos las curvas ROC
plot(rocs_fit1[[1]],print.auc = TRUE, print.auc.y = 0.6, col = "red") 
plot(rocs_fit2[[2]],print.auc = TRUE, print.auc.y = 0.55, col = "blue", add=T, )

# curvas ROC PARA COMPARAR LOS MODELOS
######################################



