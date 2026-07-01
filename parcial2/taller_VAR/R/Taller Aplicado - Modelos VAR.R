# Taller Aplicado - Modelos VAR

# Nota: Tips practicos en R
## Para limpiar el entorno de trabajo se puede correr: rm(list = ls())
## Para cerrar todas las graficas actualmente abiertas se puede correr: dev.off()
## Para resetear R se puede usar las teclas: Ctrl + Shift + F10

# 0. Preliminares ---- ----------------------------------------------------

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

# Cargue las funciones auxiliares para graficar las IRF
source("funciones_auxiliares_graficacion_VAR.R", encoding = "UTF-8")

# Importe la base de datos desde Excel 
datos = read_xlsx("")


# 1. Exploración previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------


# Ahora bien, asuma lo siguiente que la variable "y" es la "variable más exógena" de las dos variables que se van a modelar, es decir: 

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


# Pronóstico
predict(, n.ahead = , ci = 0.95) 

# Gráfica del pronóstico
x11()
autoplot(predict(, n.ahead = , ci = 0.95)) 


# 2.4.2 Funciones Impulso Respuesta (IRF) ----

# Matrices asociadas a las funciones impulso respuesta 
Phi(, nstep=)

# Matrices asociadas a las funciones impulso respuesta ortogonalizadas
Psi(, nstep=)

# Parametros de las graficas de las IRF
variables = colnames(series)
pasos_adelante = 0:
int_conf_irf = 0.95
semilla_irf = 202601
repeticiones_bootstrap_irf = 100 # Bootstrappings empleados para construir los IC de las IRF

# La funcion graficar_grilla_irf() calcula el objeto irf() una sola vez
# y luego crea cada panel con programacion funcional.

# Graficacion de las IRF no ortogonalizadas

# IRF de las variables del sistema ante distintos choques exogenos.
irf_no_ortog = graficar_grilla_irf(
  VAR_estimado,
  variables,
  pasos_adelante,
  ortog = FALSE,
  int_conf = int_conf_irf,
  prefijo_titulo = "Impulso",
  semilla = semilla_irf,
  runs = repeticiones_bootstrap_irf
)

x11()
# Grilla de IRF: columnas = impulsos; filas = respuestas.
grid.arrange(grobs = irf_no_ortog$graficas,
             layout_matrix = matrix(seq_along(irf_no_ortog$graficas),
                                    nrow = length(variables), byrow = TRUE))

# Graficacion de las IRF ortogonalizadas

# Usamos los mismos pasos adelante, intervalo de confianza y semilla.
# IRF ortogonalizadas de las variables del sistema ante distintos choques exogenos.
irf_ortog = graficar_grilla_irf(
  VAR_estimado,
  variables,
  pasos_adelante,
  ortog = TRUE,
  int_conf = int_conf_irf,
  prefijo_titulo = "Impulso ortogonal",
  semilla = semilla_irf,
  runs = repeticiones_bootstrap_irf
)

x11()
# Grilla de OIRF: columnas = impulsos; filas = respuestas.
grid.arrange(grobs = irf_ortog$graficas,
             layout_matrix = matrix(seq_along(irf_ortog$graficas),
                                    nrow = length(variables), byrow = TRUE))