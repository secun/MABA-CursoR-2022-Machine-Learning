# Práctica 3.1 de Machine Learning
# Modelos de clasificación

################################################################################

# Cargar librería para visualizar gráficas
library(ggplot2) # Instalar si hace falta
# Ejemplo de instalación
# install.packages('ggplot2')

################################################################################

# Asegurar que estamos en el directorio de trabajo correcto
remove(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd() # ¿Me devuelve el directorio correcto?
# De lo contrario, modificar con comando setwd() o con el panel inferior derecha

################################################################################

# Carga de datos y visualización de la tabla
Datos <- read.csv('Datos 3.1.csv', sep=';')
head(Datos)

################################################################################

# Pasar la variable EsHombre a variable categórica usando as.factor

Datos$EsHombre <- as.factor(Datos$EsHombre) # Para interpretar como categórica

# En R es posible modificar columnas enteras como en este ejemplo

################################################################################

# Visualizar un gráfico de dispersión de los datos con ggplot

Grafica <- ggplot(Datos, aes(x=Altura, y=Peso, color=EsHombre)) +
           geom_point(size=3) # grosor de puntos = 3

Grafica # Esto visualiza la gráfica en el panel inferior derecha (Plots)

################################################################################

# Definir el modelo de regresión logística:

Modelo <- glm(formula = EsHombre ~ Altura + Peso,
              family = binomial(logit),
              data = Datos)

# Visualizar información de ajuste:

summary(Modelo) 

# Extraer el valor de los coeficientes y del p-Valor

coef(summary(Modelo))[,1] # Coeficientes (Intercept es la constante)

coef(summary(Modelo))[,4] # p-Valor de los coeficientes

################################################################################

# Evaluar el modelo con datos propios. ¿El modelo me va a clasificar bien?
# Rellenar los huecos _

mi_EsHombre <- _  # 0 = mujer, 1 = hombre
mi_Altura <- ___  # en cm
mi_Peso <- __     # en Kg

################################################################################

# Establecemos un umbral del 50% para separar hombres y mujeres

pUmbral <- 0.5 # Por encima se considera hombre y por debajo mujer

# Organizo mis datos

mis_Datos <- list("Altura" = mi_Altura,
                  "Peso" = mi_Peso)

# Le pido al modelo que me calcule mi probabilidad de ser hombre

mi_Prob_EsHombre <- predict(Modelo, mis_Datos, type="response") 

# Vamos a ver si me ha clasificado bien comparando la probabilidad con el umbral
# Sentencia ifelse -> parecida a la fórmula SI de Excel
# ifelse(PRUEBA LÓGICA, VALOR SI VERDADERO, VALOR SI FALSO)

# Si soy hombre y mi probabilidad de ser hombre es mayor que 50% -> Bien
# Si soy mujer  y mi probabilidad de ser hombre es menor que 50% -> Bien
# En caso contrario -> Mal

Resultado = ifelse(mi_EsHombre && mi_Prob_EsHombre > pUmbral, "bien.",
            ifelse(!mi_EsHombre && mi_Prob_EsHombre < pUmbral, "bien.", "mal."))

# Vamos a ver qué ha salido:

cat("Mi probabilidad de ser hombre es de", as.character(round(mi_Prob_EsHombre*100,2)),
    "%, así que el modelo me ha clasificado", Resultado)

################################################################################

# Voy a ver dónde me ubico en el gráfico de dispersión (punto negro):

Grafica + geom_point(aes(x=mi_Altura, y=mi_Peso), colour="black", size = 4)

# Visualizar gráfica en panel inferior derecha (Plots)
