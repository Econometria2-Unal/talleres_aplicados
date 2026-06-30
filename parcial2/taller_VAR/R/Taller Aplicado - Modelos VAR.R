# Taller Aplicado - Modelos VAR


# 0. Preliminares ---- ----------------------------------------------------


# Limpie el entorno de trabajo 
rm(list = ls())

# Paquetes requeridos 

## Procesamiento de datos
library(tidyverse)    # Paquete que incluye ggplot2 y dplyr
library(readxl)        # Para leer archivos xlsx

## Bibliotecas para el trabajo de series de tiempo 
library(vars)         # Para usar modelos VAR
library(urca)         # Para realizar pruebas de raíz unitaria

## Librerias que complementan la libreria de graficación ggplot2 (Se usan en la graficación de las IRF)
library(ggfortify)
library(gridExtra)

# Define el directorio de trabajo sobre el cuál va a trabajar 
setwd("")

# Importe la base de datos desde Excel 
datos = read_xlsx("")


# 1. Exploración previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------


## 1) Un shock en "y" afecta de manera inmediata/contemporánea (es decir, desde el periodo t que ocurre el shock) tanto a "x" como a sí mismo ("y"). 
## 2) Mientras que un shock en "x" únicamente afecta de manera inmediata/contemporánea (es decir, desde el periodo t que ocurre el shock) a sí mismo ("x")
##    y es en periodos posteriores a t que afecta la variable y.

# Use la información anterior, para organizar de manera correcta las variables que conforman la base de datos que importó en R, 
# de tal forma que éstas queden en el orden correcto para calcular las funciones impulso respuesta ortogonalizadas usando la 
# descomposición de Cholesky: 
datos =  %>% 
  dplyr::select()

# verifique que el orden de sus variables en su base de datos haya quedado en el orden correcto
glimpse()

# Transforme la base de datos que importó en un objeto de serie de tiempo "ts"
series = ts(, start=c(,), frequency=)

# Grafique las series de tiempo
y_plot = autoplot(, size=1,ts.colour="lightblue", 
                  xlab="",ylab="", main="Variable y")

x_plot = autoplot(, size=1,ts.colour="royalblue", 
                  xlab="",ylab="", main="Variable x")

x11();grid.arrange(y_plot, x_plot,ncol=2)

# Realice las pruebas de Dickey-Fuller aumentadas ¿Son las series de tiempo estacionarias? 
# Para los parámetros de la prueba de Dickey-FUller, use las siguientes especificaciones: " lags=3, selectlags = "AIC",type="none" "

## Prueba de ADF para la priemra serie de tiempo
adf1= ur.df(, lags=3, selectlags = "AIC",type="none")
summary(adf1) 

adf2= ur.df(, lags=3,selectlags = "AIC",type="none")
summary(adf2) 


# 2. Metodología Box-Jenkins para series multivariadas -----------------------


# 2.1 Identificación del modelo ----


# Selección de rezagos para un VAR
VARselect(, lag.max=6,type = "none", season = NULL)


# 2.2 Estimación del modelo ----


VAR_estimado =  VAR(, p=, type="none", season=NULL)  

# ¿Cuáles son las raíces del polinomio asociado a la parte autoregresiva del modelo VAR det(I - A(L)), donde A(L) denota la parte autoregresiva del VAR? 
# Ojo, tenga mucho cuidado con lo que se le está preguntando!!!
roots()

# ¿Cuáles son los coeficientes del modelo?

# Nota: Bcoef y Acoef difererirían si por ejemplo el VAR tuviera términos determínisticos (e.g. constantes, tendencias lineales determínisticas y demás)

## Para visualizar todos los coeficientes 
Bcoef()

## Para visualizar solo los coeficientes asosicados a los rezagos del modelo VAR
Acoef()

# Resumen completo de los resultados de la estimación del VAR
summary()


# 2.3 Validación de supuestos del modelo ----


# Test de no correlación serial 
P.10=serial.test(, lags.pt = 10, type = "PT.asymptotic"); P.10 

# Test de heterocedasticidad
arch.test(, lags.multi = 12, multivariate.only = TRUE) 

# Test de normalidad (Jarque-Bera para series multivariadas)
normality.test() 


# 2.4 Uso del modelo: Pronóstico y funciones impulso respueta ----


# 2.4.1 Pronóstico ----


x11()
predict(, n.ahead = , ci = 0.95) 
autoplot(predict(, n.ahead = , ci = 0.95)) 


# 2.4.2 Funciones Impulso Respuesta (IRF) ----


# Número de pasos adelante
pasos_adelante = 0:

# Función que me permite calcular y graficar las funciones impulso respuesta (NOTA: POR NADA DEL MUNDO MODIFIQUE ESTA FUNCIÓN!!!)
# A cada función impulso respuesta le asigno una gráfica
impulso_respuesta = function(var, impulso, respuesta, pasos_adelante, ortog, 
                             int_conf, titulo){
  
  "Calcula las funciones impulso respuesta ortogonalizadas y no ortogonalizadas 
  y devuelve una grafíca IRF o OIRF dependiendo la especificación"
  
  # Cáclulo de la función impulso respuesta
  total_pasos_futuros = length(pasos_adelante) - 1
  IRF = irf(var, impulse=impulso, response=respuesta, n.ahead = total_pasos_futuros, 
            ortho=ortog, ci = int_conf)
  IRF_data_frame = data.frame(IRF$irf,IRF$Lower,IRF$Upper, pasos_adelante)
  # Gráfica de la función impulso respuesta
  graph = IRF_data_frame %>% 
    ggplot(aes(x=IRF_data_frame[,4], y=IRF_data_frame[,1], ymin=IRF_data_frame[,2], 
               ymax=IRF_data_frame[,3] )) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(titulo)+
    ylab("")+
    xlab("Pasos adelante") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))    
  return(graph)
}

# IRF de las variables del sistema ante distintos choques exógenos.

x.x = impulso_respuesta(VAR_estimado, "x", "x", pasos_adelante, ortog = F,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de x")
x.y = impulso_respuesta(VAR_estimado, "x", "y", pasos_adelante, ortog = F,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de y")
y.x = impulso_respuesta(VAR_estimado, "y", "x", pasos_adelante, ortog = F,
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de x")
y.y = impulso_respuesta(VAR_estimado, "y", "y", pasos_adelante, ortog = F, 
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de y")

x11()
grid.arrange(x.x, x.y, y.x, y.y,ncol=2)

# IRF Ortogonalizadas. 

x.x = impulso_respuesta(VAR_estimado, "x", "x", pasos_adelante, ortog = T,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de x")
x.y = impulso_respuesta(VAR_estimado, "x", "y", pasos_adelante, ortog = T,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de y")
y.x = impulso_respuesta(VAR_estimado, "y", "x", pasos_adelante, ortog = T,
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de x")
y.y = impulso_respuesta(VAR_estimado, "y", "y", pasos_adelante, ortog = T, 
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de y")

x11()
grid.arrange(x.x, x.y, y.x, y.y,ncol=2)

# Matrices asociadas a las funciones impulso respuesta 
Phi(VAR_estimador, nstep=10)

# Matrices asociadas a las funciones impulso respuesta ortogonalizadas
Phi(VAR_estimado, nstep=10)