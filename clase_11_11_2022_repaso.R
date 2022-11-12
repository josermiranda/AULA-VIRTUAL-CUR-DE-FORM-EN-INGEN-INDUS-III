
install.packages(c("tidyverse", "visdat", "dlookr", "funModeling", "flextable", "inspectdf", "qqplotr", "ggpmisc", "PerformanceAnalytics", "corrplot"))



library(tidyverse)
library(visdat)
library(dlookr)
library(funModeling)
library(flextable) 
library(inspectdf) 
library(qqplotr) 
library(ggpmisc)
library(PerformanceAnalytics)
library(corrplot)


data("mtcars")
dim(mtcars)
str(mtcars)
mtcars$vs<-factor(mtcars$vs) #transforma la columna  vs en factor #
mtcars$am<-factor(mtcars$am)  #transforma la columna  am en factor #
str(mtcars)
visdat::vis_dat(mtcars,sort_type = FALSE) # ayuda a explorar la estructura de la clase de datos y la ausencia de valores.  No ordena las columnas según el tipo de datos en los vectores

visdat::vis_miss(mtcars) # Gráfico personalizado de los datos que faltan.

diagnose(mtcars) %>% flextable() #Entrega información, que permite determinar la calidad de la variables.

#análisis de variables categóricas o enteras

funModeling::freq(mtcars$vs)

funModeling::freq(mtcars$am)

diagnose_category(mtcars) %>% flextable() #Muestra un análisis de las variables categóricas


##analisis de variables numericas

summary(mtcars)

summary(mtcars$mpg)



