mtcars$mpg

hist(mtcars$mpg, breaks="Sturges")

#importante#
#cuando la media aritmetica es mayor que la mediana  la curva tiene sesgo hacia la derecha
#cuando la mediana es mayor a la media aritmetica la cuerva tine sesgo a la izquierda #



skewness(mtcars$mpg)
# si el valor de skewness  es 0 indica que la curva es normal perfecta, es decir la moda= media= mediana#
#si el valor es mayor a cero significa que la cuerva esta sesgada o es asimetrica  hacia la derecha#
#si el valor es menor a cero significa que  la cuerva esta sesgada o es asimetrica  hacia la izquierda#

hist(mtcars$hp, breaks="Sturges")

#  la moda , la mediana y la media  es donde esta la mayor cantidad de datos concentradosm o en el intervalo de frecuencioa es la barra que muestra el mayor frecuencia absoluta#  

skewness(mtcars$hp)


#Estudio de normalidad en R

plot(mtcars$mpg, dnorm(mtcars$mpg, mean = mean(mtcars$mpg), sd = sd(mtcars$mpg)), ylab = "", lwd = 2, col = "red")

#Añadimos la media
abline(v=mean(mtcars$mpg))

# Programa en R que diseña una tabla de distribución de frecuencias
# Fecha creación: 22-04-2022
# Autora: Francisca Novoa
# Instalar paquetes
install.packages("fdth")


#Invocar librerías
library(fdth)

sueldos <-c(119,125,126,128,132,135,135,135,136,138,138,140,140,142,142,144,144,145,145,146,146,147,147,148,149,150,150,152,153,154,156,157,158,161,163,164,165,168,173,176)

#Calculamos la tabla de distribución de frecuencias.
dist <- fdt(sueldos,breaks="Sturges")
# la distribucion es distinta ala realizada en excel ya que no de les ingreso los datos de configiracion y R utilizo los valoresv por defecto

#agregando datos de minimo y maximo y amplitu#
dist_1 <- fdt(sueldos,breaks="Sturges", start= 119, end=179, h=10)



# Tablas de distribución de frecuencias

# mpg
min<-min(mtcars$mpg)
min
max<-max(mtcars$mpg)
max

rango<-abs(min-max)
rango


intervalos<- 1+3.33*log10(length(mtcars$mpg))
intervalos


Amplitud <- round(rango/round(intervalos,0),0)
Amplitud


Tabla<-fdt(mtcars$mpg,breaks="Sturges",start = min, end= max , by = Amplitud)
Tabla

#f = frecuencia absoluta 
#rf =frecuencuia relativa
#cf = frecuencia absoluta acomulada

# disp

min<-min(mtcars$disp)
min
max<-max(mtcars$disp)
max
rango<-abs(min-max)
rango
intervalos<- 1+3.33*log10(length(mtcars$disp))
intervalos
Amplitud <- round(rango/round(intervalos,0),0)
Amplitud
Tabla_frecuencias<-fdt(mtcars$disp,breaks="Sturges",start = min, end= max , by = Amplitud)
Tabla_frecuencias





