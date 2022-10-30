#This is a famous dataset from the UC Irvine Machine Learning Repository (https://archive.ics.uci.edu). 
#It is included as the day dataset in regtools by permission of the data curator. 
#A detailed description of the data 
#is available on the UCI site, https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset.

#Carganos librería de datos 
library(regtools)
data(day1)

#For this data set. how can we predict the number of riders?
#Let's use the temperature as the predictor for number of riders

######### Number of neighbors in k-NN #############
#Predict ridership for a temperature of 28C
#Person on the street viewpoint: collect days where T close to 28, and calculate mean (its called k-NN model)
dists <- abs(day1$temp - 28) # distances of the temps to 28
do10 <- order(dists)[1:10] # which are the 10 closest?

dists[do10] #distancias en temperatura
day1$tot[do10] #numero de riders 

mean(day1$tot[do10]) #media de numero de riders (5400)
# In this case, this is 10-NN

#Bias vs variance: Why 10 neighbors? Is it enough? Is it to small?
#Small number may bring very much sampling variability (variance issue)
#Large number will introduce portions of data that may induce a systemic tendency to under or overpredict. (bias issue)
#Trade-off between variance and bias --> This is related to the volume of data too and the number of parameters
#Importance of choosing the right value of k (parameters). Overfitting (too small k) and underfitting (too large k).

# What's the right number of parameters then?
#From maths, k is aprox sqrt(n)
sqrt(nrow(day1))
#the number of nearest neighbors should be less than the square root of the number of data points.

######### Number of predictors/features #############
#A dataset is not large or small; its size n must be viewed relative to number of features p. 
#Overfitting can happen too when we use too may features.
#Not using one specific feature could lead to biased upwards or downwards (bias can be seen as a lack of detail then)
#Having one feature with small sample size can lead to variance due to that specific variable/feature.
#An embedding is the process where one feature is replaced by a better feature from outside (ZIP code by average temperature)

# What's the right number of feautures then?
#From maths, p is aprox sqrt(n)
sqrt(nrow(day1))
#the number of features should be less than the square root of the number of data points.

################ The regression function ############
#In example above we predicted the value of riders for a temperature of 28 C.
#Let's do the same for 20C, with 10 neighbors.
dists <- abs(day1$temp - 20) # distances of the temps to 28
do10 <- order(dists)[1:10] # which are the 10 closest?

dists[do10] #distancias en temperatura
day1$tot[do10] #numero de riders 

mean(day1$tot[do10]) #media de numero de riders (5169)

#Podríamos continuar este proceso, y obtener una función r(T) 

#We predict the ridership on a day of temperature t to be r(t), the mean ridership of all days of temperature t. 
#This function, r(t), is called the regression function of ridership on temperature.
#La función tiene tantos argumentos como features.
#La función se entrena sobre el conjuntos de datos training set  
#La función nos da un estimador del numero de riders en base a una muestra


