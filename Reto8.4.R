################## R Lab ################## 
################## Análisis de clusters ##################
#----------------------------------------------------------#
#----------------------------------------------------------#
####### Retos de clase - 8.4 Custering aglomerativo para empleados de Deloitte #################
# --- Inicio resolucion  ---
#Librerias
library(ggplot2) #libreria grafica

#Dados 5 empleados de Deloitte caracterizados por el siguiente dataframe
df_deloitte <- data.frame (
  ID  = c(1,2,3,4,5),
  Genero = c("F", "M","M","F","M"),
  Edad = c(24,51,52,33,42), # años
  Salario =c(23,64,97,55,45) #k€
)

#Transformación de datos.
#Explicar pq es necesario crear la variable dummy sobre genero.

#Explicar pq se hace una normalizacion de datos.
df_deloitteS =  data.frame(
  Edad =  (df_deloitte$Edad - min(df_deloitte$Edad)) / (max(df_deloitte$Edad) - min(df_deloitte$Edad)),
  Salario =  (df_deloitte$Salario - min(df_deloitte$Salario)) / (max(df_deloitte$Salario) - min(df_deloitte$Salario)),
  Genero = ifelse( df_deloitte$Genero == "M",1,0))

rownames (df_deloitteS)= df_deloitte$ID
#Visualizamos datos
ggplot(data=df_deloitteS, aes(x=Edad, y=Salario))+
  geom_point(aes(color=as.factor(Genero)))+
  labs(color = "Genero") +
  geom_text(aes(label=rownames(df_deloitteS)), nudge_x = 0.05)

#Calculamos distancia euclidea (diferencia entre observaciones) - 
#Hay normalización de datos en este caso? Tiene la variables Genero más peso que las otras variables?
dist12 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["2",])^2)) 
dist13 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["3",])^2)) 
dist14 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["4",])^2)) 
dist15 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["5",])^2)) 
dist23 =  sqrt(sum((df_deloitteS["2",] - df_deloitteS ["3",])^2)) 
dist24 =  sqrt(sum((df_deloitteS["2",] - df_deloitteS ["4",])^2)) 
dist25 =  sqrt(sum((df_deloitteS["2",] - df_deloitteS ["5",])^2)) 
dist34 =  sqrt(sum((df_deloitteS["3",] - df_deloitteS ["4",])^2)) 
dist35 =  sqrt(sum((df_deloitteS["3",] - df_deloitteS ["5",])^2)) 
dist45 =  sqrt(sum((df_deloitteS["4",] - df_deloitteS ["5",])^2)) 
#Valores de distancia (vease entorno de variables) sugieren que 2 y 5 son los mas cercanos
rm(list=ls(pattern = 'dist')) #limpiamos entorno de variables

#Definimos nuevo clusters, como una media de los dos cluster que lo forman
df_deloitteS["25",] = (df_deloitteS["2",] + df_deloitteS["5",]) / 2
df_deloitteS["25",]

#Visualizamos datos con el nuevo cluster
ggplot(data=df_deloitteS, aes(x=Edad, y=Salario))+
  geom_point(aes(color=as.factor(Genero)))+
  geom_text(aes(label=rownames(df_deloitteS)), nudge_x = 0.05)

#Calculamos distancia euclidea (diferencia entre observaciones)          
dist125 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["25",])^2)) 
dist325 =  sqrt(sum((df_deloitteS["3",] - df_deloitteS ["25",])^2)) 
dist425 =  sqrt(sum((df_deloitteS["4",] - df_deloitteS ["25",])^2)) 
dist13 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["3",])^2)) 
dist14 =  sqrt(sum((df_deloitteS["1",] - df_deloitteS ["4",])^2)) 
dist34 =  sqrt(sum((df_deloitteS["3",] - df_deloitteS ["4",])^2)) 
#Valores de distancia (ver valor de variables) sugieren que 14 son los mas cercanos
rm(list=ls(pattern = 'dist')) #limpiamos entorno de variables

#Definimos nuevo clusters, como una media de los dos cluster que lo forman
df_deloitteS["14",] = (df_deloitteS["1",] + df_deloitteS["4",]) / 2
df_deloitteS["14",]

#Visualizamos datos con el nuevo cluster
ggplot(data=df_deloitteS, aes(x=Edad, y=Salario))+
  geom_point(aes(color=as.factor(Genero)))+
  geom_text(aes(label=rownames(df_deloitteS)), nudge_x = 0.05)


#Calculamos distancia euclidea (diferencia entre observaciones)          
dist1425 =  sqrt(sum((df_deloitteS["14",] - df_deloitteS ["25",])^2)) 
dist314 =  sqrt(sum((df_deloitteS["3",] - df_deloitteS ["14",])^2)) 
dist325 =  sqrt(sum((df_deloitteS["3",] - df_deloitteS ["25",])^2))
#Valores de distancia (ver variables) sugieren que 325 son los mas cercanos
rm(list=ls(pattern = 'dist')) #limpiamos entorno de variables

#Definimos nuevo clusters, como una media de los dos cluster que lo forman
df_deloitteS["325",] = (df_deloitteS["3",] + df_deloitteS["25",]) / 2
df_deloitteS["325",]

#Visualizamos datos con el nuevo cluster
ggplot(data=df_deloitteS, aes(x=Edad, y=Salario))+
  geom_point(aes(color=as.factor(Genero)))+
  geom_text(aes(label=rownames(df_deloitteS)), nudge_x = 0.05)

###Este proceso de custering aglomerativo continuaría hasta que tuviésemos un unico cluster.

# --- Fin resolucion  ---
