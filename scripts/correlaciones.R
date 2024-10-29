# Función ipak para instalar y cargar varios paquetes a la vez
ipak <- function(pkg) {
  # Esta línea identifica los paquetes en pkg que aún no están instalados en el sistema, guardando esos nombres en new.pkg.
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  # Si new.pkg tiene elementos (es decir, si hay paquetes que no están instalados), se instala cada paquete de new.pkg con la función install.packages(), incluyendo sus dependencias.
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Crear la lista de los paquetes a utilizar
packages <- c("foreign", "apaTables", "PerformanceAnalytics", "psych", "corrr", "readr", "dplyr", "ggplot2", "corrplot", "reshape2")

# Instalar y cargar los paquetes del listado anterior
ipak(packages)

# Cargar el dataframe que vamos a utilizar
df <- read_csv("data/raw/dailyActivity_merged.csv")
View(df)

# Crear matriz de correlaciones solo con columnas numéricas
cor_matrix <- cor(df[sapply(df, is.numeric)])
View(cor_matrix)

# Crear matriz de correlaciones sin NA
df2 <- cor_matrix[complete.cases(cor_matrix), ]

# Volver a crear la matriz de correlaciones
cor(df2)

# Crea un objeto que sea la propia matriz de correlaciones
correlac <- cor(df2)

# Crear directamente la tabla de correlaciones en APA en formato word
apa.cor.table(df2, filename = "dailyActivity.doc", table.number = 2, show.conf.interval = FALSE, landscape = TRUE)

# figura de correlaciones con historgrama, diagrama de puntos
pairs.panels(df2, pch = 20, stars = TRUE, main = "Daily Activity of R")



# Red de correlaciones
df2 %>%
  correlate() %>%
  network_plot()

# Matriz de Correlación (Método de Pearson): Solamente es usada para variables numéricas
# library(ggplot2)
# libray(corrplot)

df3 <- df2 # creo una copia del dataframe para trabajar con el proximo ejemplo de matriz de correlaciones
df3.cor <- cor(df3, method = "pearson") # Otros métodos de correlación son los de Spearman y Kendall
df3.cor <- round(df3.cor, 2)
corrplot(df3.cor)

# Otro modelo de grafica de Matriz de Correlación
corrplot(df3.cor, 
         method = "shad", 
         shade.col = NA, 
         tl.col = "black", 
         tl.srt = 90)

# Definir una paleta de color personalizada
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Ordenar de forma que los valores mas correlacionados estén mas cerca
corrplot(df3.cor, 
         method = "shad", 
         shade.col = NA, 
         tl.col = "black", 
         tl.srt = 90,
         col = col(200),
         addCoef.col = "black",
         order = "AOE")

# Generar la gráfica de la matriz de correlación
corrplot(df3.cor, 
         method = "shade",             # Método para aplicar el sombreado
         shade.col = NA,                # Sin color de sombra adicional
         tl.col = "black",              # Color de los nombres de las variables
         tl.srt = 45,                   # Rotación del texto
         col = col(200),                # Paleta de colores personalizada
         addCoef.col = "black",         # Mostrar coeficientes en color negro
         order = "AOE",                 # Ordenar por el método AOE
         type = "full",                 # Mostrar la matriz completa
         diag = FALSE)                  # Ocultar la diagonal principal

#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "shade", 
         shade.col = NA, 
         tl.col = "black", 
         tl.srt = 90,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "lower")


#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "shade", 
         shade.col = NA, 
         tl.col = "black", 
         tl.srt = 45,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "upper")


#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "shade", 
         shade.col = NA, 
         tl.col = "black", 
         tl.srt = 90,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "upper",
         diag = F)


#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "shade", 
         shade.col = NA, 
         tl.col = "black", 
         tl.srt = 90,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "upper",
         diag = F,
         addshade = "all")

#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "shade", 
         tl.col = "black", 
         tl.srt = 45,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "upper",
         diag = F,
         addshade = "all")

#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "circle", 
         tl.col = "black", 
         tl.srt = 45,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "upper",
         diag = F,
         addshade = "all")

#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "pie", 
         tl.col = "black", 
         tl.srt = 45,
         col = col(200),
         addCoef.col = "black",
         order = "AOE",
         type = "upper",
         diag = F,
         addshade = "all")

# Cambiando el orden para Componente Principal
#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "color", 
         tl.col = "black", 
         tl.srt = 45,
         col = col(200),
         addCoef.col = "black",
         order = "FPC",
         type = "upper",
         diag = F,
         addshade = "all")


# Cambiando el orden para Clustering
#Pinta solo un lado de la correlación
corrplot(df3.cor, 
         method = "color", 
         tl.col = "black", 
         tl.srt = 45,
         col = col(200),
         addCoef.col = "black",
         order = "hclust",
         type = "upper",
         diag = F,
         addshade = "all")

# 
# library(reshape2)
df3.melted <- melt(df3.cor)
head(df3.cor)
head(df3.melted)

ggplot(data = df3.melted, 
       aes(x=Var1, y=Var2, fill=value))+
  geom_tile()

