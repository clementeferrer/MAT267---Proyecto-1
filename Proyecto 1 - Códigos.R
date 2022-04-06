#Instalación de paquetes
install.packages("ggplot2")
install.packages("fpp")
install.packages("rmaf")
install.packages("forecast")
install.packages("devtools")
devtools::install_github("goodekat/ggResidpanel")

#Librerias
library(fpp)
library(extrafont)
library(forecast)
library(ggplot2)
library(rmaf)
library(ggResidpanel)

#PREGUNTA 1

#1.A

#Obtener los datos y calculaR media y varianza
datos=scan("https://www.stat.pitt.edu/stoffer/tsa2/data/flu.dat.txt")
mean(datos) #obtenemos 0.2918623
var(datos) #obtenemos 0.01578173

#Transformar los datos a serie temporal
serie=ts(datos,start=c(1968,1),end=c(1978,12),frequency=12)

#Calculamos el lambda apropiado para filtrar con BoxCox
lambda = BoxCox.lambda(serie,lower=0)

#Calculamos la media y la varianza de la transformación
mean(serie2) #Obtenemos -2.842304
var(serie2) #Obtenemos 1.127498

#Graficamos ambos para observar las diferencias en las series:

autoplot(serie1) +
  geom_line(color = "firebrick") +
  labs(x = "Tiempo", y = "Muerte por cada 10000 personas",
       title = "Serie sin transformar")

autoplot(serie2) +
  geom_line(color = "firebrick") +
  labs(x = "Tiempo", y = "Muerte por cada 10000 personas",
       title = "Serie transformada")

#-------------------------------------------------------------------------#

#1.B

#Calculamos los RMSE
datos = scan("https://www.stat.pitt.edu/stoffer/tsa2/data/flu.dat.txt")
vec <- 1:132
RMSE = c()
real = data.frame(vec, datos)
for (i in 1:30){
  model <- lm(datos ~ poly(vec, i, raw = TRUE),
              data = real)
  RMSE <- c(RMSE, sqrt(mean(model$residuals^2)))
}
p <- 1:30
RMSE
plot(p, RMSE)

#Calculamos el modelo polinomico regresivo para p=50
datos = scan("https://www.stat.pitt.edu/stoffer/tsa2/data/flu.dat.txt")
vec <- 1:132
model <- lm(datos ~ poly(vec, 50, raw = TRUE),
            data = data.frame(vec,datos))
summary(model)

#-------------------------------------------------------------------------#

#1.C

#Graficar
ggplot(frame, aes(x=x,y=y)) +  geom_line(color = "firebrick",size=1) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,50,raw=TRUE), size=1)+ 
  xlab("Tiempo")+ylab("Muertes por cada 10000 personas")

#-------------------------------------------------------------------------#

#1.D

#Descomponemos
descompuesto=decompose(serie)
autoplot(descompuesto) +
  geom_line(color = "firebrick") +
  labs(x = "Año", y = "Flu",
       title = "Serie descomposición")

#-------------------------------------------------------------------------#

#1.E

#holtwinters
holt=HoltWinters(serie)
plot(holt)

#-------------------------------------------------------------------------#

#PREGUNTA 2

#2.A

#Ploteo gráfico
gtemp = scan("C:/Users/ccfer/Desktop/globtemp.dat")
x = gtemp[45:142] #Datos desde 1900-1997
t = 1900:1997
df <- data.frame(t,x)

theme_set(theme_bw())
ggplot(df, aes(x = t, y = x)) + geom_line(color = "firebrick") +
labs(x = "Año", y = "Desviación de temperatura (°C)",
title = "Annual average global surface temperature deviations from 1900 to 1997",
caption = "Data: Shumway and Stoffer (2000)") + 
theme(plot.title = element_text(lineheight = .8, size = 12))

#-------------------------------------------------------------------------#

#2.B

#Crear conjunto de entrenamiento y testeo
gtemp.train = gtemp[45:124]
gtemp.test = gtemp[125:142]

#Diferenciar serie
gtemp.dif.train <- diff(gtemp.train)
gtemp.dif.test <- diff(gtemp.test)

#Identificar Alpha óptimo
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(gtemp.dif.train, alpha = alpha[i], h = 18)
  RMSE[i] <- accuracy(fit, gtemp.dif.test)[2,2]
}

# Convertir a Data Frame
alpha.fit <- data.frame(alpha, RMSE)

# RMSE vs. alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_point(size = 0.5, color = "blue") + 
  labs(x = "Alpha", y = "RMSE",
 title = "Alpha v/s RMSE")

y_min <- min(alpha.fit[,"RMSE"])
x_val_associated <- alpha.fit[alpha.fit$RMSE == y_min, "alpha"]

#Suavizamiento exponencial simple
ses.gtemp <-ses(x, h = 20, alpha = x_val_associated)

#Ploteo
autoplot(ses.gtemp) +
  geom_line(color = "firebrick") +
  labs(x = "Año", y = "Desviación de temperatura (°C)",
       title = "Predicción usando suavizamiento exponencial simple") +
  scale_x_continuous(labels=function(x)x+1900)

#-------------------------------------------------------------------------#

#2.C

#Diferenciada y SES con mejor alpha
ses.gtemp.dif <-ses(gtemp.dif, h = 20,alpha = .16)

autoplot(ses.gtemp.dif) +
  geom_line(color = "firebrick") +
  labs(x = "Año", y = "Desviación de temperatura (°C)",
       title = "Predicción usando SES a la serie diferenciada") +
  scale_x_continuous(labels=function(x)x+1900)

#-------------------------------------------------------------------------#

#2.D

# Serie sin tendencia
gtemp.dif <- diff(x)
autoplot(ts(gtemp.dif)) +
  geom_line(color = "firebrick") +
  labs(x = "Año", y = "Desviación de temperatura (°C)",
       title = "Serie sin tendencia") +
  scale_x_continuous(labels=function(x)x+1900)

#-------------------------------------------------------------------------#

#2.E

#Modelo de regresión lineal
gtemp.linear <- lm(x ~ t)
summary(gtemp.linear) 
res <- resid(gtemp.linear)
resid_panel(gtemp.linear)
plot(density(res))

ggplot(df, aes(x = t, y = x)) + geom_line(color = "firebrick") +
labs(x = "Año", y = "Desviación de temperatura (°C)",
title = "Modelo de regresión lineal ajustado") + 
theme(plot.title = element_text(lineheight = .8, size = 12)) +
stat_smooth(method = "lm", col = "red")

#-------------------------------------------------------------------------#

#2.F

#Descomposición manual
globtemp = ts(scan("C:/Users/ccfer/Desktop/globtemp.dat"), start=1856)
gtemp = window(globtemp, start=1900, end = 1997)
decomp1 <- ma.filter(gtemp, seasonal = TRUE, period = 1)


#-------------------------------------------------------------------------#

#Sitios utilizados
#https://otexts.com/fpp2/ts-objects.html
#https://www.linkedin.com/pulse/data-seasonal-trend-random-decomposition-using-r-gordon-tang-1c
#https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
