# Taller Aplicado - Modelos VECM
# Escenario 1: Series I(1) No Cointegradas

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
library(urca)         # Para realizar pruebas de raíz unitaria y el test de Johansen
library(tsDyn)        # Funcionalidades adicionales para el análisis de series de tiempo 


## Librerias que complementan la libreria de graficación ggplot2 (Se usan en la graficación de las IRF)
library(ggfortify)
library(gridExtra)

# Define el directorio de trabajo sobre el cuál va a trabajar 
setwd("C:/Users/gcrp9/Documents/Unal/Academic/Profesor/Econometria II/Becas_asistente_docente/Recursos/talleres_aplicados/parcial2/taller_VECM/R")

# Importacion de funciones auxiliares para las graficas
source("funciones_auxiliares_VECM.R", encoding = "UTF-8")

# Importe la base de datos desde Excel 
datos_escenario1 = read_xlsx("datos_escenario1_I(1)_no_cointegradas.xlsx")


# 1. Escenario 1: Series I(1)  -----------------------------------------------------------


# 1.1 Escenario 1: Exploración previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------

# Inspeccione sus datos
glimpse(datos_escenario1)

# Transforme la base de datos que importó en un objeto de serie de tiempo "ts"
series_esecenario1 = ts(datos_escenario1, start=c(1900,1), frequency=4)

# Grafique las series de tiempo
y_plot = autoplot(series_esecenario1[,1], size=1,ts.colour="lightblue", 
                  xlab="",ylab="", main="Variable y")

x_plot = autoplot(series_esecenario1[,2], size=1,ts.colour="royalblue", 
                  xlab="",ylab="", main="Variable x")

x11();grid.arrange(y_plot, x_plot,ncol=2)

# Realice las pruebas de Dickey-Fuller aumentadas ¿Son las series de tiempo estacionarias? 
# Para los parámetros de la prueba de Dickey-FUller, use las siguientes especificaciones: " lags=3, selectlags = "AIC",type="none" "

## Prueba de ADF para la priemra serie de tiempo
adf1= ur.df(series_esecenario1[,1], lags=3, selectlags = "AIC",type="none")
summary(adf1) 

adf2= ur.df(, lags=3,selectlags = "AIC",type="none")
summary(adf2) 


# 1.2 Escenario 1: Identificación del número de rezagos apropiado p del VAR(p) y de su reparametrización VECM(p-1) -----------------------

# 1.2.1 Escenario 1: Identificación del número de rezagos apropiado p del VAR(p) ----


# Selección de rezagos para un VAR
VARselect(series_esecenario1, lag.max=6,type = "none", season = NULL)


# 1.2.2 Escenario 1: Estimación del modelo VAR ----


# Nota: La razón por la que se estima el modelo VAR(p), se debe a que sus errores, si se escogió el rezago correcto del VAR, serán ruido blanco, 
#       Independientemente de si las series del VAR son I(0) o I(1)
#       Los errores del VAR(p) serán exactamente los mismos que los de su reparametrización en forma de un VECM(p-1), por lo que si los residuales
#       del modelo VAR que se estimará satisfacen los supuestos del modelo, entonces los residuales del VECM(p-1) también lo harán. 
VAR_estimado1 =  VAR(series_esecenario1, p=2, type="none", season=NULL)  

# Resumen completo de los resultados de la estimación del VAR
summary(VAR_estimado1)

# 1.2.3 Escenario 1: Validación de supuestos del modelo ----


# Test de no correlación serial 
P.10=serial.test(VAR_estimado1, lags.pt = 10, type = "PT.asymptotic"); P.10 

# Test de heterocedasticidad
arch.test(VAR_estimado1, lags.multi = 12, multivariate.only = TRUE) 

# Test de normalidad (Jarque-Bera para series multivariadas)
normality.test(VAR_estimado1) 

# 1.2.4 Escenario 1: Realización del Test de Johansen, para determinar el rango de la matriz Pi ----


# Criterio del valor propio máximo
test_valor_propio_maximo = ca.jo(series_esecenario1, ecdet = , type = "eigen", K = 2, spec = "transitory")
summary(test_valor_propio_maximo) 

# Criterio de la traza 
test_traza = ca.jo(series_esecenario1, ecdet = , type = "trace", K = 2, spec = "transitory")
summary(test_traza) 

# 1.2.5 Escenario 1: Dado que las series son I(1) y no están cointegradas, se estima un modelo VAR(1) en diferencias ----

# Estimación del modelo VAR(1) en diferencias
VAR_diff_estimado1 = VAR(diff(series_esecenario1), p=1, type="none", season=NULL)  

# Resumen completo de los resultados de la estimación del VAR(1) en diferencias
summary(VAR_diff_estimado1)

# Validación de supuestos del modelo VAR(1) en diferencias

# Test de no correlación serial 
P.10=serial.test(VAR_diff_estimado1, lags.pt = 10, type = "PT.asymptotic"); P.10 

# Test de heterocedasticidad
arch.test(VAR_diff_estimado1, lags.multi = 12, multivariate.only = TRUE) 

# Test de normalidad (Jarque-Bera para series multivariadas)
normality.test(VAR_diff_estimado1) 

# 1.2.6 Escenario 1: Pronósticos del modelo VAR(1) en diferencias ----


pronosticos_escenarios1 = predict(VAR_diff_estimado1, n.ahead=12, boots = T); pronosticos_escenarios1
x11(); plot(pronosticos_escenarios1)

# 1.2.7 Escenario 1: Funciones Impulso Respuesta (IRF) ----

# Matrices asociadas a las funciones impulso respuesta
Phi(VAR_diff_estimado1, nstep=10)

# Matrices asociadas a las funciones impulso respuesta ortogonalizadas
Psi(VAR_diff_estimado1, nstep=10)

# Parametros de las graficas de las IRFs
variables_irf = c("x", "y")
pasos_adelante = 0:24
int_conf_irf = 0.95
semilla_irf = 202601
repeticiones_bootstrap_irf = 100 # Bootstrappings empleados para construir los IC de las IRFs

# La funcion graficar_grilla_irf() calcula el objeto irf() una sola vez
# y luego crea cada panel con programacion funcional.

# IRF No Ortogonalizadas.

# IRF de las variables del sistema ante distintos choques exogenos.
irf_no_ortog = graficar_grilla_irf(
  VAR_diff_estimado1,
  variables_irf,
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
                                    nrow = length(variables_irf), byrow = TRUE))

# IRF Ortogonalizadas.

irf_ortog = graficar_grilla_irf(
  VAR_diff_estimado1,
  variables_irf,
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
                                    nrow = length(variables_irf), byrow = TRUE))
