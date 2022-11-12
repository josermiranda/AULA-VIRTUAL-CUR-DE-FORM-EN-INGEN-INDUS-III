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








