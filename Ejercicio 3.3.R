# Práctica 3.3 de Machine Learning
# Modelos de clasificación -> Evaluación tests COVID, segunda parte

################################################################################

# Cargar librerías (instalar las que haga falta)
library(ROCit) # Para calcular métricas en función del umbral de forma automática
library(caTools) # Para calcular curva ROC y área debajo de la curva
library(ggplot2) # Para visualizar gráficas
# Ejemplo de instalación
# install.packages('caTools')
#library(caTools) # Para visualizar gráficas
################################################################################

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

# En este punto pasaríamos las probabilidades predichas a clases predichas con un umbral, 
# pero el objetivo de esta sesión práctica es la valoración del umbral. 
# En primer lugar, se va a hacer una comparación entre el desempeño de los 4 test 
# por medio de la curva ROC. 

# Para calcular la curva ROC es necesario que R calcule las 
# métricas para todos los umbrales posibles entre 0 y 1.

# Primero hay que pasar las clases reales a tipo categórico

DatosTest1$CategoriaReal <- as.factor(DatosTest1$CategoriaReal)
DatosTest2$CategoriaReal <- as.factor(DatosTest2$CategoriaReal)
DatosTest3$CategoriaReal <- as.factor(DatosTest3$CategoriaReal)
DatosTest4$CategoriaReal <- as.factor(DatosTest4$CategoriaReal)

# Calcular métricas del primer test de forma automática para todos los umbrales
?measureit
TablaCalculos1 <- measureit(score = DatosTest1$ProbabilidadTest, 
                            class = DatosTest1$CategoriaReal, 
                            measure = c("TPR", "FPR", "SENS", "SPEC", "PREC", "NPV", "ACC", "FSCR"),
                            negref = "N")

# Es necesario convertir lo que devuelve measureit a formato DataFrame

TablaCalculos1 <- as.data.frame(do.call(cbind, TablaCalculos1))

# Visualizamos lo que se ha calculado con measureit para el primer test

head(round(TablaCalculos1, 3))

################################################################################

# Hacer lo mismo para los otros 3 test

TablaCalculos2 <- measureit(score = DatosTest2$ProbabilidadTest, 
                            class = DatosTest2$CategoriaReal, 
                            measure = c("TPR", "FPR", "SENS", "SPEC", "PREC", "NPV", "ACC", "FSCR"),
                            negref = "N")

TablaCalculos3 <- measureit(score = DatosTest3$ProbabilidadTest, 
                            class = DatosTest3$CategoriaReal, 
                            measure = c("TPR", "FPR", "SENS", "SPEC", "PREC", "NPV", "ACC", "FSCR"),
                            negref = "N")

TablaCalculos4 <- measureit(score = DatosTest4$ProbabilidadTest, 
                            class = DatosTest4$CategoriaReal, 
                            measure = c("TPR", "FPR", "SENS", "SPEC", "PREC", "NPV", "ACC", "FSCR"),
                            negref = "N")

TablaCalculos2 <- as.data.frame(do.call(cbind, TablaCalculos2))
TablaCalculos3 <- as.data.frame(do.call(cbind, TablaCalculos3))
TablaCalculos4 <- as.data.frame(do.call(cbind, TablaCalculos4))

# Visualizamos lo que se ha calculado con measureit para el segundo test

head(round(TablaCalculos2, 3))

################################################################################

# Ahora que tenemos todas las métricas para todos los umbrales posibles, podemos
# calcular la curva ROC. La curva ROC es una gráfica donde el eje X es 
# la tasa de falsos positivos (1 - especificidad) y el eje Y es 
# la tasa de verdaderos positivos (sensibilidad). 

# A continuación se visualizan las curvas ROC de los cuatro tests.
# eje x = Tasa de falsos positivos     (FPR - False Positive Rate)
# eje y = Tasa de verdaderos positivos (TPR - True Positive Rate)

# ggplot -> Primero las 4 líneas, luego los colores y la leyenda y por último el título

Grafica <- ggplot() +
  geom_line(aes(x = TablaCalculos1$FPR, y = TablaCalculos1$TPR, colour = "Test1"), size = 1.5, alpha = 0.5) +
  geom_line(aes(x = TablaCalculos2$FPR, y = TablaCalculos2$TPR, colour = "Test2"), size = 1.5, alpha = 0.5) +
  geom_line(aes(x = TablaCalculos3$FPR, y = TablaCalculos3$TPR, colour = "Test3"), size = 1.5, alpha = 0.5) +
  geom_line(aes(x = TablaCalculos4$FPR, y = TablaCalculos4$TPR, colour = "Test4"), size = 1.5, alpha = 0.5) +
  scale_colour_manual("", 
                      breaks = c("Test1", "Test2", "Test3", "Test4"),
                      values = c("green", "blue", "red", "yellow")) + 
  labs(x="Tasa falsos positivos", y="Tasa verdaderos positivos", title = "Curva ROC") +
  theme(plot.title = element_text(hjust = 0.5))

Grafica # Visualizar gráfica en el panel inferior derecha (Plots)

# Se recomienda usar la opción Zoom para visualizar mejor las curvas

################################################################################

# ¿Qué curva tiene mayor área por debajo?
# Nos vendría bien una función para calcularlo.

# Definimos una variable que contenga las áreas de los 4 test

Resultado <- data.frame(matrix(ncol = 1, nrow = 4))
colnames(Resultado) <- "AUC"

# Usamos la función colAUC para calcular el área

Resultado$AUC[1] <- colAUC(X = DatosTest1$ProbabilidadTest, y = DatosTest1$CategoriaReal)
Resultado$AUC[2] <- colAUC(X = DatosTest2$ProbabilidadTest, y = DatosTest2$CategoriaReal)
Resultado$AUC[3] <- colAUC(X = DatosTest3$ProbabilidadTest, y = DatosTest3$CategoriaReal)
Resultado$AUC[4] <- colAUC(X = DatosTest4$ProbabilidadTest, y = DatosTest4$CategoriaReal)

Resultado$AUC <- round(Resultado$AUC, 3)

# Escribirlo en la gráfica

Texto <- paste("Área test 1 = ", as.character(Resultado$AUC[1]), "\n",
               "Área test 2 = ", as.character(Resultado$AUC[2]), "\n",
               "Área test 3 = ", as.character(Resultado$AUC[3]), "\n",
               "Área test 4 = ", as.character(Resultado$AUC[4]))

Grafica <- Grafica + annotate("text", x=0.75, y=0.25, label = Texto)

Grafica # Mostrar gráfica

# ¿Qué test escogeríamos según el criterio de área debajo de la curva?

FilaTestMayorADC <- which(Resultado$AUC == max(Resultado$AUC))
ValorAreaTest <- round(max(Resultado$AUC),3)

cat("El test", FilaTestMayorADC, "es el que mayor ADC tiene:", ValorAreaTest)

################################################################################

# Ejercicios avanzados para valorar el umbral (para después de clase)

################################################################################

# Vamos a centrarnos en este test que ha dado el mayor ADC
# Escogemos TablaCalculosX (X = mejor test) y la almacenamos en TablaCalculos

assign("TablaCalculos", eval(as.name(paste("TablaCalculos", which(Resultado$AUC == max(Resultado$AUC)), sep=""))))

# Para evitar descuadres en la gráfica, sobrescribir el umbral Inf con 100%

TablaCalculos$Cutoff[1] <- 1

# Visualizar sensibilidad, especificidad, precisión, valor predictivo negativo y exactitud
# Usaremos ggplot como en el ejemplo de las curvas ROC

Grafica <- ggplot(TablaCalculos, aes(x=Cutoff)) +
  geom_line(aes(y = SENS, colour = "Sensibilidad"),    size = 1.5, alpha = 0.5) +
  geom_line(aes(y = SPEC, colour = "Especificidad"),   size = 1.5, alpha = 0.5) +
  geom_line(aes(y = PREC, colour = "Precisión"),       size = 1.5, alpha = 0.5) +
  geom_line(aes(y = NPV,  colour = "Val. Pred. Neg."), size = 1.5, alpha = 0.5) +
  geom_line(aes(y = ACC,  colour = "Exactitud"),       size = 1.5, alpha = 0.5) +
  scale_colour_manual("", 
                      breaks = c("Sensibilidad", "Especificidad", "Precisión", "Val. Pred. Neg.", "Exactitud"),
                      values = c("green", "blue", "red", "yellow", "cyan")) + 
  labs(x="Umbral", y="Métrica", title = "Efecto del umbral en las métricas") +
  theme(plot.title = element_text(hjust = 0.5))

Grafica # Mostrar gráfica (ignorar los Warning que salen)

################################################################################

# El problema es que el máximo valor de cada una de las métricas se obtiene con 
# umbral 0% o umbral 100%, tal y como se ve en la gráfica.

# Si para mí tiene el mismo coste un falso negativo que un falso positivo, 
# puedo recurrir al valor F1 (columna FSCR en TablaCalculos):

TablaCalculos$FSCR[1] <- 0 # Para sobrescribir el NaN que hay en umbral 100%

# Usamos ggplot de nuevo con solo el valor F1

Grafica <- ggplot(TablaCalculos, aes(x=Cutoff)) +
  geom_line(aes(y = FSCR),   size = 1.5, alpha = 0.5) +
  labs(x="Umbral", y="Valor F1", title = "Efecto del umbral en el valor F1") +
  theme(plot.title = element_text(hjust = 0.5))

# Obtener umbral para el cual el valor F1 es máximo

ValorMaximoF1 <- max(TablaCalculos$FSCR)
UmbralValorF1 <- TablaCalculos$Cutoff[which(TablaCalculos$FSCR==ValorMaximoF1)]

# Dibujar el umbral en la gráfica

Grafica + geom_vline(xintercept=UmbralValorF1, linetype="dashed", color="red")

# ¿Qué valor tiene este umbral y el valor F1?

cat("El umbral", round(UmbralValorF1, 3), "es el que mayor valor F1 tiene:", round(ValorMaximoF1, 3))

################################################################################

# Si para mí no tiene el mismo coste un falso negativo que un falso positivo,
# necesito definir una función de coste.

# En este caso hay muchos más negativos reales en el ensayo del test COVID.
# Por ello, vamos a suponer que un falso negativo penaliza más ya que el número
# de positivos reales es muy bajo.

# Escoger función de coste

FuncCoste = list(                 # Función de coste
  Func1  = "Coste = 50·FN   + 1·FP", # 1
  Func2  = "Coste = 100·FN  + 1·FP", # 2
  Func3  = "Coste = 200·FN  + 1·FP") # 3

# Rellenar hueco _ con la función de coste deseada

MiFuncCoste = _ 

# Calculamos costes para cada valor del umbral

CoefFN <- ifelse(MiFuncCoste==1, 50, ifelse(MiFuncCoste==2, 100, 200))
CoefFP <- 1

TablaCalculos$Coste <- TablaCalculos$FN * CoefFN + TablaCalculos$FP * CoefFP

# Hallamos en qué valor del umbral se encuentra el coste mínimo:

ValorMinimoCoste <- min(TablaCalculos$Coste)
UmbralCoste <- TablaCalculos$Cutoff[which(TablaCalculos$Coste==ValorMinimoCoste)]

# Visualizamos este valor de coste

Grafica <- ggplot(TablaCalculos, aes(x=Cutoff)) +
  geom_line(aes(y = Coste),   size = 1.5, alpha = 0.5) +
  labs(x="Umbral", y="Función de coste", title = "Efecto del umbral en mi func. de coste") +
  theme(plot.title = element_text(hjust = 0.5))

# Dibujar el umbral en la gráfica

Grafica + geom_vline(xintercept=UmbralCoste, linetype="dashed", color="red")

# ¿Qué umbral minimiza el coste?

MiFuncionCoste <- as.character(FuncCoste[MiFuncCoste])

cat("Con la función de coste '", MiFuncionCoste, "', el umbral que más me conviene es", round(UmbralCoste,3))
