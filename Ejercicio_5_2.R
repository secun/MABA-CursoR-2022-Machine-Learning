#Ejercicio 5.2

#En esta ocasión, utilizaremos cross-validation para intentar encontrar
#un árbol que no sufra de overfitting pero con una calidad suficiente en función
#de la métrica que escojamos

#Elegimos una semilla para siempre obtener los mismos resultados aunque sean
#datos aleatorios
set.seed(12)
titanic_train$Survived <- as.factor(titanic_train$Survived)

#Utilizamos trainControl para crear el cross-validation
train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                              number = 10,             # Use 10 partitions
                              repeats = 2)             # Repeat 2 times

# Set required parameters for the model type we are using**
tune_grid = expand.grid(cp=c(0.1,0.01,0.001,0.0001))


# Use the train() function to create the model
validated_tree <- train(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked,
                        data=titanic_train,                 # Data set
                        method="rpart",                     # Model type(decision tree)
                        trControl= train_control,           # Model control options
                        tuneGrid = tune_grid,               # Required model parameters
                        maxdepth = 5,                       # Additional parameters***
                        minbucket=5)                          

validated_tree         # View a summary of the model

#Repito modelo para el CP mejor
best_tree <- rpart(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked,
                      cp = 0.01,                 # Set complexity parameter*
                      data = titanic_train)       # Use the titanic training data
test_preds <- predict(best_tree,              
                      newdata=titanic_test,      
                      type="class") 
#confusionMatrix(factor(test_preds), factor(titanic_test$Survived))
table(factor(test_preds), factor(titanic_test$Survived))

#Resultando un árbol sencillo 
prp(best_tree)
