#Instalar 3 paquetes a la vez
install.packages(c("modeest", "raster", "moments"))

# Cargar el dataframe que vamos a utilizar
df <- read_csv("data/raw/dailyActivity_merged.csv")
View(df)

summary(df$SedentaryMinutes)
str(df$LoggedActivitiesDistance)

library(modeest) #moda
library(raster) #quantiles, coeficiente de variacion (vc)
library(moments) #asimetría, curtosis

X = df$TotalSteps

#### Medidas de Centralización
mean(X) #sum(X)/length(X) # Otra manera de obtener el promedio
median(X)
mfv(X) #mfv es la moda
quantile(X)

### Medidas de Dispersión
var(X) #varianza es cuanto los valores se desplazan con base a la media, siempre será un numero positivo porque estos valores se elevan al cuadrado
sd(X) #desviación tipica se vuelve a dimensión original se quita el cuadrado que hemos usado en la variaza. 
cv(X) #coeficiente de variación


### Medidas de Asimetría
skewness(X) #Aimetría
kurtosis(X)

par(mfrow = c(1,1))
hist(X)



# Otros ejemplos
install.packages("palmerpeguins")
library(palmerpenguins)
data(package = 'palmerpenguins')

df <- penguins
head(df)

### ESTADISTICOS
mean(df)
max(df$bill_length_mm)
median(df$bill_length_mm)
min(df$bill_length_mm)
range(df$bill_length_mm)
quantile(df$bill_length_mm)
IQR(df$bill_length_mm)
var(df$bill_length_mm)
sd(df$bill_length_mm)