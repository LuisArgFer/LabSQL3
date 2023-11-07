# INTEGRANTES LAB3
# Jorge Arturo Morera Salazar
# Luis Andrés Arguedas Fernández

library(dplyr)
library(ggplot2)
library(visdat) 
library(naniar)
library(dplyr)
library(ggplot2)
library(stringr)
library(visdat) 
library(hexbin)
library(ggplot2)

install.packages("dplyr")
install.packages("visdat")
install.packages("naniar")
install.packages("stringr")

datos<-read.csv(file = "files/estadisticas_vinos.csv",header = T,sep = ",",encoding = "UTF-8")

#Con el comando head podemos ver las columnas que tiene nuestro archivo  y algunos de los datos
#con los con cuenta
head(datos)
#Este comando nos dice el tipo de datos que tenemos en este caso nuestro tipo de datos es data.frame
class(datos)
#Con este comando podemos ver más claro las columnas que tiene nuestro archivo
names(datos)
#Con este comando podemos ver las columnas y el tipo de dato con el que cuenta
str(datos)
#Dim nos deja ver la dimension de todos los datos que tenemos en nuestro csv
dim(datos)
#Con esto podemos ver el nombre de las columnas de nuestro csv
colnames(datos)

#Con este comando asignamos los nombres que queremos para nuestras columnas
colnames(datos)<-c("Fila","Pais","Designacion","Puntos","Precio","Provincia","Region","Variedad","Bodega")

#Con view podemos ver todos los datos más claramente en una tabla
View(datos)

#Con los siguietes comando tenemos un resumen estadítico de las columnas numericas de nuestro csv
#en este caso las columnas son puntos y Precio
summary(datos)

# Este comando nos dice los valores nulos por columna donde nos dice cuales son con un true
is.na(datos)

# Este nos ayuda a calcular los valores nulos por cada columna
sapply(datos, function(x) sum(is.na(x)) / length(x) * 100)

# Crear un boxplot para identificar valores atípicos estos son los que podemos ver fuera de nuestos
# grafica son los valores atíficos
ggplot(datos, aes(y = Puntos)) +
  geom_boxplot()

#Mi compañero de trabajo y yo decidimos que podiamos eliminar los datos na, ya que sólo eran 6
# y no iban a afectar en nuestros datos

delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}
  
# Calcular estadísticas descriptivas para la variable "Puntos"
mean(datos$Puntos)  # Media
median(datos$Puntos)  # Mediana
sd(datos$Puntos)  # Desviación estándar

# Imputación por la mediana en la variable "Precio"
datos$Precio[is.na(datos$Precio)] <- median(datos$Precio, na.rm = TRUE)

# Este gráfico de barras mostrará la distribución de vinos por país.
ggplot(datos, aes(x = Pais)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de Vinos por País", x = "Frecuencia", y = "País") +
  coord_flip()

#compara las variables "Precio" y "Puntos" para visualizar la relación entre el precio y la calificación de los vinos
ggplot(datos, aes(x = Precio, y = Puntos)) +
  geom_point(color = "blue") +
  labs(title = "Relación entre Precio y Puntos de Vinos", x = "Precio", y = "Puntos")

#Este gráfico de caja te permitirá comparar los puntajes de vinos por país.
ggplot(datos, aes(x = Pais, y = Puntos)) +
  geom_boxplot(fill = "red") +
  labs(title = "Puntajes de Vinos por País", x = "País", y = "Puntos")

#Este análisis univariable proporciona estadísticas descriptivas y un histograma de la variable "Precio"
# Estadísticas descriptivas para la variable "Precio"
summary(datos$Precio)

# Histograma de la variable "Precio"
ggplot(datos, aes(x = Precio)) +
  geom_histogram(binwidth = 10, fill = "green") +
  labs(title = "Distribución de Precios de Vinos", x = "Precio", y = "Frecuencia")

#Este análisis bivariable muestra la relación entre los puntajes y los precios de los vinos .
ggplot(datos, aes(x = Puntos, y = Precio)) +
  geom_point(color = "purple") +
  labs(title = "Relación entre Puntaje y Precio de Vinos", x = "Puntos", y = "Precio")
