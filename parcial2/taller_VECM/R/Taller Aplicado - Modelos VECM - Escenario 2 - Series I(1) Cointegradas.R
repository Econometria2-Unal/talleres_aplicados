# Taller Aplicado - Modelos VECM
# Escenario 2: Series I(1) Cointegradas

# 0. Preliminares ---- ----------------------------------------------------


# Limpie el entorno de trabajo 
rm(list = ls())

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
setwd("")

# Importe la base de datos desde Excel 
datos_escenario2 = read_xlsx("")


# 1. Escenario 2: Series I(1)  -----------------------------------------------------------


# 1.1 Escenario 2: Exploración previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------

# Inspeccione sus datos
glimpse()

# Transforme la base de datos que importó en un objeto de serie de tiempo "ts"
series_esecenario2 = ts(, start=c(,), frequency=4)

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


# 1.2 Escenario 2: Identificación del número de rezagos apropiado p del VAR(p) y de su reparametrización VECM(p-1) -----------------------

# 1.2.1 Escenario 2: Identificación del número de rezagos apropiado p del VAR(p) ----


# Selección de rezagos para un VAR
VARselect(, lag.max=6,type = "none", season = NULL)


# 1.2.2 Escenario 2: Estimación del modelo VAR ----


# Nota: La razón por la que se estima el modelo VAR(p), se debe a que sus errores, si se escogió el rezago correcto del VAR, serán ruido blanco, 
#       Independientemente de si las series del VAR son I(0) o I(1)
#       Los errores del VAR(p) serán exactamente los mismos que los de su reparametrización en forma de un VECM(p-1), por lo que si los residuales
#       del modelo VAR que se estimará satisfacen los supuestos del modelo, entonces los residuales del VECM(p-1) también lo harán. 
VAR_estimado2 =  VAR(, p=2, type="none", season=NULL) 

# Resumen completo de los resultados de la estimación del VAR
summary()

# 1.2.3 Escenario 2: Validación de supuestos del modelo ----


# Test de no correlación serial 
P.10=serial.test(, lags.pt = 10, type = "PT.asymptotic"); P.10 

# Test de heterocedasticidad
arch.test(, lags.multi = 12, multivariate.only = TRUE) 

# Test de normalidad (Jarque-Bera para series multivariadas)
normality.test() 

# 1.2.4 Escenario 2: Realización del Test de Johansen, para determinar el rango de la matriz Pi ----


# Criterio del valor propio máximo
test_valor_propio_maximo = ca.jo(, ecdet = , type = , K = , spec = "transitory")
summary(test_valor_propio_maximo) 

# Criterio de la traza 
test_traza = ca.jo(, ecdet = , type = , K = , spec = "transitory")
summary(test_traza) 

# 1.2.5 Escenario 2: Dado que las series son I(1) y están cointegradas, se estima un modelo VECM(1) ----

# Se estima el modelo VECM. Para estimar el modelo puede proveer ya sea el resultado del test de Johansen 
# obtenido por la prueba del valor propio máximo o por la prueba de la traza. Acá estamos usando
# el resultado obtenido por la prueba del valor propio
VECM_escenario2 = cajorls(, r=); VECM_escenario2

# Vector de cointegración normalizado
coefB()

# Coeficientes de velocidad de ajuste
coefA()

# 1.2.6 Escenario 2: Reparametrización del VECM como un VAR en niveles ----

# Para la reparametrización del VECM(1) en un VAR(2) en niveles. Para realizar la reparametrización 
# al modelo VAR puede proveer ya sea el resultado del test de Johansen 
# obtenido por la prueba del valor propio máximo o por la prueba de la traza. Acá estamos usando
# el resultado obtenido por la prueba del valor propio
VAR_reparametrizado_escenario2 = vec2var(, r = ) 

# Validación de supuestos del modelo VAR(2) reparametrizado 

# Test de no correlación serial 
P.10=serial.test(, lags.pt = 10, type = "PT.asymptotic"); P.10 

# Test de heterocedasticidad
arch.test(, lags.multi = 12, multivariate.only = TRUE) 

# Test de normalidad (Jarque-Bera para series multivariadas)
normality.test() 

# 1.2.7 Escenario 2: Pronósticos del modelo VAR(2) reparametrizado ----


pronosticos_escenarios2 = predict(, n.ahead=, boots = T); pronosticos_escenarios2
x11(); plot(pronosticos_escenarios2)

# 1.2.8 Escenario 2: Funciones Impulso Respuesta (IRF) ----


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

x.x = impulso_respuesta(VAR_reparametrizado_escenario2, "x", "x", pasos_adelante, ortog = F,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de x")
x.y = impulso_respuesta(VAR_reparametrizado_escenario2, "x", "y", pasos_adelante, ortog = F,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de y")
y.x = impulso_respuesta(VAR_reparametrizado_escenario2, "y", "x", pasos_adelante, ortog = F,
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de x")
y.y = impulso_respuesta(VAR_reparametrizado_escenario2, "y", "y", pasos_adelante, ortog = F, 
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de y")

x11()
grid.arrange(x.x, x.y, y.x, y.y,ncol=2)

# IRF Ortogonalizadas. 

x.x = impulso_respuesta(VAR_reparametrizado_escenario2, "x", "x", pasos_adelante, ortog = T,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de x")
x.y = impulso_respuesta(VAR_reparametrizado_escenario2, "x", "y", pasos_adelante, ortog = T,
                        int_conf = 0.95, titulo = "Impulso de x - respuesta de y")
y.x = impulso_respuesta(VAR_reparametrizado_escenario2, "y", "x", pasos_adelante, ortog = T,
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de x")
y.y = impulso_respuesta(VAR_reparametrizado_escenario2, "y", "y", pasos_adelante, ortog = T, 
                        int_conf = 0.95, titulo = "Impulso de y - respuesta de y")

x11()
grid.arrange(x.x, x.y, y.x, y.y,ncol=2)

# Matrices asociadas a las funciones impulso respuesta 
Phi(VAR_reparametrizado_escenario2, nstep=10)

# Matrices asociadas a las funciones impulso respuesta ortogonalizadas
Phi(VAR_reparametrizado_escenario2, nstep=10)
