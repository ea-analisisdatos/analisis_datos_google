# Cargar el dataframe que vamos a utilizar
df <- read_csv("data/raw/dailyActivity_merged.csv")
View(df)

# Promedio de cada variable individualmente
mean(df$TotalSteps)
mean(df$TotalDistance)

# Calcular el promedio de todas las columnas numéricas al mismo tiempo y redondear a 2 decimales
#options(scipen = 999)  # Desactiva la notación científica
#View(df)                # Visualiza el dataframe sin notación científica
#options(scipen = 0)  # Restaura la notación científica a su configuración predeterminada


# Paso 1: Seleccionar solo las columnas numéricas del dataframe
df_numeric <- df[sapply(df, is.numeric)]
# Paso 2: Calcular el promedio de cada columna numérica
means <- sapply(df_numeric, mean, na.rm = TRUE)
# Paso 3: Redondear los promedios a 2 decimales
means_rounded <- round(means, 2)
# Ver el resultado final
means_rounded

# Código en una unica linea
# sapply(df, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 2) else NA)

###### eliminando formato cientifico de los numeros muy grandes
# Paso 1: Seleccionar solo las columnas numéricas del dataframe
#df_numeric <- df[sapply(df, is.numeric)]
# Paso 2: Calcular el promedio de cada columna numérica
#means <- sapply(df_numeric, mean, na.rm = TRUE)
# Paso 3: Redondear los promedios a 2 decimales y convertirlos a formato sin notación científica
#means_rounded <- format(round(means, 2), nsmall = 2, scientific = FALSE)
# Ver el resultado final
#means_rounded

# Otro ejemplo mas sención para sacar el promedio de las columnas. No puede haber variables categoricas y ni valores NA.
sapply(df_numeric, function(x) mean(x))
