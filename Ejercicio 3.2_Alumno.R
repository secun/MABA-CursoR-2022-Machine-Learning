# Práctica 3.2 de Machine Learning
# Modelos de clasificación -> Evaluación tests COVID

################################################################################

# Cargar librería para calcular matrices de confusión
library(caret) # Instalar si hace falta
#CARET: Classification and Regression Matrix
# Ejemplo de instalación
# install.packages('caret')

################################################################################

# Asegurar que estamos en el directorio de trabajo correcto
# Asegurar que estamos en el directorio de trabajo correcto
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd() # ¿Me devuelve el directorio correcto?
# De lo contrario, modificar con comando setwd() o con el panel inferior derecha

################################################################################

# Cargar datos y visualizar primeros registros del primer test

DatosTest1 <- read.csv('DatosTestCovid1.csv', sep=';')
DatosTest2 <- read.csv('DatosTestCovid2.csv', sep=';')
DatosTest3 <- read.csv('DatosTestCovid3.csv', sep=';')
DatosTest4 <- read.csv('DatosTestCovid4.csv', sep=';')

head(DatosTest1)

################################################################################

# Calcular categorías predichas usando un umbral del 50%

pUmbral <- 0.5

# Usar sentencia ifelse (como la fórmula SI de Excel)
# Se puede aplicar a toda la columna de las tablas

DatosTest1$CategoriaTest <- ifelse(DatosTest1$ProbabilidadTest > pUmbral, "P", "N")
DatosTest2$CategoriaTest <- ifelse(DatosTest2$ProbabilidadTest > pUmbral, "P", "N")
DatosTest3$CategoriaTest <- ifelse(DatosTest3$ProbabilidadTest > pUmbral, "P", "N")
DatosTest4$CategoriaTest <- ifelse(DatosTest4$ProbabilidadTest > pUmbral, "P", "N")

head(DatosTest1)

################################################################################

# Pasar las categorías reales y predichas a variable categórica usando as.factor

DatosTest1$CategoriaReal <- as.factor(DatosTest1$CategoriaReal)
DatosTest2$CategoriaReal <- as.factor(DatosTest2$CategoriaReal)
DatosTest3$CategoriaReal <- as.factor(DatosTest3$CategoriaReal)
DatosTest4$CategoriaReal <- as.factor(DatosTest4$CategoriaReal)

DatosTest1$CategoriaTest <- as.factor(DatosTest1$CategoriaTest)
DatosTest2$CategoriaTest <- as.factor(DatosTest2$CategoriaTest)
DatosTest3$CategoriaTest <- as.factor(DatosTest3$CategoriaTest)
DatosTest4$CategoriaTest <- as.factor(DatosTest4$CategoriaTest)

################################################################################

# Ahora calcular la matriz de confusión del primer test COVID

MatrizConfusion1 <- confusionMatrix(data = DatosTest1$CategoriaTest, 
                                    reference = DatosTest1$CategoriaReal, 
                                    positive = "P", mode = "everything")

MatrizConfusion1 # Información que da la función de R


################################################################################

# Usamos una función personalizada que nos ayudará a visualizar esto

source("DrawConfusionMatrix.R", # Para poder visualizar la matriz de confusión
       encoding = 'UTF-8')      # Para interpretar bien las tildes

draw_2D_confusion_matrix(cm = MatrizConfusion1, 
                         caption = "Matriz de confusión test 1") 
# draw_2D_confusion_matrix es una función personalizada (ver DrawConfusionMatrix.R)

# Ver el resultado en el panel inferior derecha (Plots)

################################################################################

# Hacer lo mismo para el test 2, test 3 y test 4 (rellenar huecos _)


MatrizConfusion2 <- confusionMatrix(data = DatosTest2$CategoriaTest, 
                                    reference = DatosTest2$CategoriaReal, 
                                    positive = "P", mode = "everything")

draw_2D_confusion_matrix(cm = MatrizConfusion2, 
                         caption = "Matriz de confusión test 2")

# Ir viendo resultados en el panel de Plots de abajo a la derecha
# Usar las flechas azules para visualizar las matrices de cada test

MatrizConfusion3 <- confusionMatrix(data = ________________________, 
                                    reference = ________________________, 
                                    positive = "P", mode = "everything")

draw_2D_confusion_matrix(cm = ________________, 
                         caption = "Matriz de confusión test 3")

MatrizConfusion4 <- confusionMatrix(data = ________________________, 
                                    reference = ________________________, 
                                    positive = "P", mode = "everything")

draw_2D_confusion_matrix(cm = ________________, 
                         caption = "Matriz de confusión test 4")

################################################################################

# Ahora es necesario elegir el test que más nos convenga según nuestro criterio
# Para ello, hay que comparar las métricas de cada test

# Usamos una función personalizada que nos ayudará a recopilar el valor
# de las métricas de cada test

source("ExtractInfoConfusionMatrix.R") # Para leer la matriz de confusión que da R

InfoTest1 <- extract_confusion_matrix(cm = MatrizConfusion1) # Devuelve una lista
InfoTest2 <- extract_confusion_matrix(cm = MatrizConfusion2)
InfoTest3 <- extract_confusion_matrix(cm = MatrizConfusion3)
InfoTest4 <- extract_confusion_matrix(cm = MatrizConfusion4)

# Organizar estas listas en una única tabla

TablaResultados <- rbind(as.data.frame(InfoTest1), # Convertir a tabla
                         as.data.frame(InfoTest2),
                         as.data.frame(InfoTest3),
                         as.data.frame(InfoTest4))

row.names(TablaResultados) <- c('Test1','Test2','Test3','Test4')

# Visualizar la tabla

TablaResultados

################################################################################

# Debemos escoger un criterio para poder escoger el test COVID que más nos convenga

# Resumen de los criterios:

Criterios = list(                                                  # Número de criterio
  Sensibilidad   = "En caso de estar infectado, quiero dar positivo.",        # 1
  Especificidad  = "En caso de estar sano, quiero dar negativo.",             # 2
  Precision      = "En caso de dar positivo, no quiero ser falso positivo.",  # 3
  ValPredNeg     = "En caso de dar negativo, no quiero ser falso negativo.",  # 4
  Exactitud      = "Quiero el test que más veces haya acertado en general.",  # 5
  ValorF1        = "Una mezcla ponderada entre la precisión y sensibilidad.") # 6 

# Nota: en distribuciones perfectamente equilibradas, el valor F1 es igual a la exactitud

# Escoger criterio (rellenar hueco _)

MiCriterio = _ # 1 = Sensibilidad, 2 = Especificidad, 3 = Precisión, etc.

# Buscar el test cuyo valor sea el máximo según el criterio escogido

# Primero busco qué fila contiene el máximo de la columna de mi criterio

NumTestCriterio = which(TablaResultados[,MiCriterio] == max(TablaResultados[, MiCriterio]))

# Ahora que tengo la fila, solo me hace falta saber el nombre del test
# que se corresponde con esta fila

NombreTestCriterio = row.names(TablaResultados)[NumTestCriterio]

# Interpretar texto y nombre del criterio

MiCriterio_texto = as.character(Criterios[MiCriterio])
MiCriterio_nombre = names(Criterios)[MiCriterio]

cat("Mi criterio es", MiCriterio_nombre, ", es decir,", MiCriterio_texto)

# El test que escojo según mi criterio es...

cat("Por ello, el test que más me conviene es el", NombreTestCriterio)