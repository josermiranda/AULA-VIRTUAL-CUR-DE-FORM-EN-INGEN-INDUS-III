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


## Distribución empírica acumulada

#En estadística , una función de distribución empírica (comúnmente también llamada función de distribución acumulativa empírica, eCDF) es la función de distribución asociada con la medida empírica de una muestra . Esta función de distribución acumulativa es una función escalonada que salta 1 / n en cada uno de los n puntos de datos. Su valor en cualquier valor especificado de la variable medida es la fracción de observaciones de la variable medida que son menores o iguales al valor especificado.
#La función de distribución empírica es una estimación de la función de distribución acumulada que generó los puntos en la muestra. Converge con probabilidad 1 a esa distribución subyacente, según el teorema de Glivenko-Cantelli . Existen varios resultados para cuantificar la tasa de convergencia de la función de distribución empírica con la función de distribución acumulativa subyacente.
#En teoría de la probabilidad, una medida empírica es una medida aleatoria procedente de una realización particular de una secuencia (finita) de variables aleatorias

#va observando la varicion acumulada a traves de la curva

plot(mtcars$mpg, pnorm(mtcars$mpg, mean = mean(mtcars$mpg), sd = sd(mtcars$mpg)), ylab = "", lwd = 2, col = "red")

#Añadimos la media    
abline(v=mean(mtcars$mpg))

#Funcion escalonada de distribucion de probabilidad

plot(qnorm, pnorm(-4), pnorm(4), lwd = 2, xlab = "p", ylab = "Q(p)")


#Funcion de distribucion emprica acumulada

ggplot(data=mtcars, aes(x=mpg)) + 
  stat_ecdf(geom = "step")+
  labs(x = "MPG", y = "F(X)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

  

ggplot(data=mtcars, aes(x=hp)) + 
  stat_ecdf(geom = "step")+
  labs(x = "HP", y = "F(X)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)



#Grafica de cuantil -cuantil
##Q-Q plot

#En estadística, un gráfico de Q-Q (quantile-quantile) es un gráfico de probabilidad, que es un método gráfico para comparar dos distribuciones de probabilidad al trazar sus cuantiles uno contra el otro.En este caso, lo ideal es que los puntos se acerquen a una recta diagonal.

ggplot(data = mtcars, mapping = aes(sample = mpg)) +
  stat_qq_line()+
  stat_qq_band()+
  stat_qq_point()+
  labs(x = "Q-Normal", y = "Q-MPG")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)


#Cuantiles más frecuentes

#Vamos a mostrar cuales son los cuantiles más utilizados en estadística. La mayoría de ellos son de uso habitual para poder analizar de forma detallada la distribución de los datos. Además, otra de sus utilidades es separar los datos en grupos, pudiendo elegir los más altos o los más bajos. En el ejemplo veremos esto con mayor detalle.
#	Cuartil: Separa los valores en cuatro grupos iguales y existen tres cuartiles. Es el más frecuente. El cuartil uno (Q1) son los datos menores y el tres (Q3) los mayores. Por otro lado, el cuartil dos (Q2) se corresponde con la mediana (Me) que es un estadístico de posición que divide la distribución de los datos a la mitad. Los valores del cuantil serían 0.25 (Q1), 0.5(Q2) y 0.75 (Q3).
#	Quintil: Similar al anterior, es menos frecuente y divide los datos en cinco partes iguales. Por tanto, hay cuatro quintiles. Los valores del cuantil en este caso serían 0.20, 0.40, 0.60, 0.80.
#	Decil: En este caso se dividen en diez partes y, por tanto, hay nueve deciles. Una vez más, este tampoco es demasiado frecuente. Sus valores serían de 0.1 a 0.9.
#	Percentiles: Estamos ante una variante en que la distribución se divide en cien partes iguales. Puede ser de interés para muestras muy numerosas. Sus valores van de 0.01 a 0.99.


mtcars_stand <- apply(mtcars[,-c(8:9,12)], 2, scale)

qq_mpg<-quantile(mtcars_stand[,1],probs = seq(0,1,0.01)) # calcula el percentil desde 0 a 100 con in incremento de 0.01


qq_hp<-quantile(mtcars_stand[,4],probs = seq(0,1,0.01)) # calcula el percentil desde 0 a 100 con in incremento de 0.01




#Generamos el dataframe de los dos percentiles
data_qq<-data.frame(qq_mpg,qq_hp)


#Graficamos nuevamente par ver mejor la distribución

#png("qqplot.png",width = 4, height =4,units = "in",pointsize = 16,res = 300)

ggplot(data_qq,aes(x=qq_mpg,y=qq_hp))+
  geom_point()+
  geom_abline(intercept=0, slope=1)+
  labs(x = "Q-MPG", y = "Q-HP")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)




#Formulacion de Hipotesis

##Comparar distribuciones

#Ho: la variable mtcars$mpg No  esta distribuida normalmente

#H1: La variable esta distribuida normalmente 


#El punto de comparacion esta dado por la variable Delta = 0,05 
#ESTO QUIERE DECIR QUE EL VALOR DE PRECISION P-VALUE TE ARROJA EL TEST DE NORMALIDAD ES MAYOR O IGUAL AL DELTA
#LA VARIABLE ESTA DISTRIBUIDA nORMALMENTE DE LO CONTRARIO NO LO ESTA
#aplico
shapiro.test(mtcars$mpg)

# se rechaza la hipotesisnula H0 , puesto que la variable mtcars$mpg esta distribuida normalmente 
# por lo tanto se cimprueba la hipotesis alternativa  que dice  la variable mtcars$mpg esta distribuida normalmente 


#segundo caso HP

#H:0 la variable mtcars$mpg No  esta distribuida normalmente

shapiro.test(mtcars$hp)

#Se comprueba la hipotesis nula  puesto quec la variable no esta distribuida  normalmente . Con esto 
#se rechaza la hipotesis alternativa H1 , que dice " la variable mtcars $hp esta distribuida normalmente.


# tabla de p-value para todas las variables 

normality(mtcars) %>% flextable()


ks.test(mtcars_stand[,1],mtcars_stand[,4])

ks.test(mtcars$mpg,mtcars$hp)



##Detección de valores atípicos

# tabla que muestra la cantidad de valores atipicos  

diagnose_outlier(mtcars) %>% flextable()


#Diagrama de cajas  para detectar valores atipicos  
ggplot(mtcars, aes(y=hp)) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "", y = "HP")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)


ggplot(mtcars, aes(y=hp, x=vs)) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "vs", y = "HP")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)



#Diagrama de dispersión 
#png("scat_plot.png",width = 4, height =4,units = "in",pointsize = 16,res = 300)


ggplot(mtcars,aes(x=mpg,y=hp))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(x = "MPG", y = "HP")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)


##Coeficientes de correlación

#covarianza

# covarianza mayor a cero , dependencia  directa positiva ( a grandes valores de x corresponde grandes valores de y)
# Covarianza = 0  no existencia de una relacion lineal entre las dos variables 
# Covarianza menor a 0  Dependencia inversa o negativa  ( a grandes valores de x corresponde pequeños  valores de y)

cov(mtcars$mpg,mtcars$hp) 



#correlación de Pearson
cor(mtcars$mpg,mtcars$hp) 

cor(mtcars$mpg,mtcars$qsec)

cor.test(mtcars$mpg,mtcars$hp)

#correlación de Spearman automático

cor(mtcars$mpg,mtcars$hp,method = "spearman")


cor.test(mtcars$mpg,mtcars$qsec,method = "spearman")


##Tau de kendall
cor(mtcars$mpg,mtcars$hp,method = "kendall")



#matriz de correlación
options(digits = 2)
cor_matrix<-cor(mtcars[,-c(8,9,12)],method = "spearman")
cor_matrix




library(corrplot)
#png("cor_plot.png",width = 4, height =4,units = "in",pointsize = 16,res = 300)


corrplot(cor_matrix, method="ellipse",type = "upper")
#dev.off()

#
#Interpretación del valor de correlación. 
#Correlacón 			Interpretación 
#0.9 a 1.0 (-0.9 a -1.0) 	muy fuerte 
#0.7 a 0.9 (-0.7 a -0.9) 	fuerte 
#0.5 a 0.7 (-0.5 a -0.7) 	moderada 
#0.3 a 0.5 (-0.3 a -0.5) 	débil 
#0 a 0.3 (0 a -0.3) 		despreciable
#



library(PerformanceAnalytics)
chart.Correlation(mtcars[,c(1:7)], histogram = TRUE, pch = 19,method="spearman")

install.packages("DataExplorer")
library(DataExplorer)
create_report(mtcars)





