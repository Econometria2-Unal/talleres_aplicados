# Taller Aplicado - Modelos VAR 

# Nota: Tips practicos en Python
## Para limpiar el entorno en IPython/Jupyter se puede correr: "%reset -f"
## Para cerrar todas las graficas actualmente abiertas se puede correr: plt.close("all")
## En VS Code o Spyder, los bloques marcados con "# %%" se ejecutan por celdas.


# %% 0. Preliminares ---- ----------------------------------------------------

# Paquetes requeridos

## Trabajar con rutas absolutas y recrear el comportamiento de setwd() de R
from pathlib import Path
import os

## Librería para trabajar vectores y matrices en python
import numpy as np

## Procesamiento de datos
import pandas as pd

## Librería de graficación en python
import matplotlib.pyplot as plt

## Bibliotecas para el trabajo de series de tiempo en python
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller

# Define el directorio de trabajo sobre el cual va a trabajar
project_path = Path(r"")

os.chdir(project_path)
directorio_raiz = os.getcwd()

## Librerias que complementan la libreria de graficacion matplotlib
## (Se usan en la graficacion de las IRF)
from funciones_auxiliares_graficacion_VAR import (
    configurar_entorno_graficas,
    graficar_grilla_irf,
    graficar_pronostico_var,
    graficar_ts,
    imprimir_adf,
    imprimir_matrices_acof,
    imprimir_seleccion_rezagos,
    mostrar_graficas,
    predecir_var,
    prueba_arch_por_ecuacion,
    prueba_normalidad_por_ecuacion,
)

configurar_entorno_graficas(max_columns=30)

# Importe la base de datos desde Excel
datos = pd.read_excel("")


# %% 1. Exploracion previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------


# Ahora bien, asuma lo siguiente que la variable "y" es la "variable mas exogena" de las dos variables que se van a modelar, es decir:

## 1) Un shock en "y" afecta de manera inmediata/contemporanea (es decir, desde el periodo t que ocurre el shock) tanto a "x" como a si mismo ("y").
## 2) Mientras que un shock en "x" unicamente afecta de manera inmediata/contemporanea (es decir, desde el periodo t que ocurre el shock) a si mismo ("x")
##    y es en periodos posteriores a t que afecta la variable y.

# Use la informacion anterior, para organizar de manera correcta las variables que conforman la base de datos que importo en Python,
# de tal forma que estas queden en el orden correcto para calcular las funciones impulso respuesta ortogonalizadas usando la
# descomposicion de Cholesky:
datos = .loc[:, ["", ""]]

# verifique que el orden de sus variables en su base de datos haya quedado en el orden correcto
.info()
print( .head())

# Transforme la base de datos que importo en un objeto de serie de tiempo trimestral

# Cree el índice temporal 
tiempo = pd.period_range(
    start="",
    periods=len(),
    freq="",
    name="tiempo",
)

# Cree el pandas dataframe con índice temporal
series = pd.DataFrame(
    .to_numpy(),
    index=tiempo,
    columns= .columns,
)

# Grafique las series de tiempo
fig_series, axes_series = plt.subplots(1, 2, figsize=(10, 4))

graficar_ts(
    .iloc[:, 0],
    titulo="Variable y",
    color="lightblue",
    ax=axes_series[0],
)

graficar_ts(
    .iloc[:, 1],
    titulo="Variable x",
    color="royalblue",
    ax=axes_series[1],
)

fig_series.tight_layout()
mostrar_graficas()

# %% Realice las pruebas de Dickey-Fuller aumentadas ¿Son las series de tiempo estacionarias?
# Para los parametros de la prueba de Dickey-Fuller, use las siguientes especificaciones: " maxlag=3, autolag='AIC', regression='n' "

## Prueba de ADF para la priemra serie de tiempo
adf1 = adfuller( .iloc[:, 0], maxlag=3, autolag="AIC", regression="n")
imprimir_adf(adf1, .columns[0]) 

## Prueba de ADF para la segunda serie de tiempo
adf2 = adfuller( .iloc[:, 1], maxlag=3, autolag="AIC", regression="n")
imprimir_adf(adf2, .columns[1]) 


# %% 2. Metodologia Box-Jenkins para series multivariadas -----------------------


# %% 2.1 Identificacion del modelo ----


# Para crear el objeto VAR con statsmodels quitamos el indice temporal y
# dejamos solo los datos. El indice trimestral se conserva en series para
# graficas y tablas.
series_modelo = series.reset_index(drop=True)
modelo_var = VAR()

# Seleccion de rezagos para un VAR
seleccion_rezagos = .select_order(maxlags=6, trend="n")
imprimir_seleccion_rezagos(
    seleccion_rezagos,
    "Seleccion de rezagos para un VAR sin terminos deterministicos",
    incluir_rezago_cero=False,
)


# %% 2.2 Estimacion del modelo ----


VAR_estimado = .fit(, trend="n")

# ¿Cuales son las raices del polinomio asociado a la parte autoregresiva del modelo VAR det(I - A(L)), donde A(L) denota la parte autoregresiva del VAR?
# Ojo, tenga mucho cuidado con lo que se le esta preguntando!!!
# Nota: En statsmodels las raices reportadas por roots deben quedar por fuera
#       del circulo unitario. Adicionalmente, is_stable() revisa que los valores
#       propios de la matriz de compania queden dentro del circulo unitario.
raices_var = pd.DataFrame(
    {
        "raiz": .roots,
        "modulo": np.abs(VAR_estimado.roots),
    }
)
print(raices_var)
print("El proceso es estable:", VAR_estimado.is_stable(verbose=True))

# ¿Cuales son los coeficientes del modelo?

# Nota: Bcoef y Acoef difererian si por ejemplo el VAR tuviera terminos determinisitcos (e.g. constantes, tendencias lineales deterministicas y demas)

## Para visualizar todos los coeficientes
Bcoef_VAR_estimado = .params.T
print(Bcoef_VAR_estimado)

## Para visualizar solo los coeficientes asosicados a los rezagos del modelo VAR
variables = list(series.columns)
imprimir_matrices_acof( , variables)

# Resumen completo de los resultados de la estimacion del VAR
print( .summary())


# %% 2.3 Validacion de supuestos del modelo ----


# Residuales del modelo VAR estimado
residuales_var = pd.DataFrame(
    np.asarray( .resid),
    index=series.index[ .k_ar :],
    columns=variables,
)

# Test de no correlacion serial
P_10 = .test_whiteness(nlags=10, adjusted=False)
print(P_10.summary())

# Test de heterocedasticidad
# statsmodels no tiene un equivalente directo a arch.test() multivariado de
# vars en R. Por tanto, se construye una funcion que permite hacer un arch.test()
# univariado para cada uno de los residuales de la regresion, uno por cada
# variable del VAR.
arch_12 = prueba_arch_por_ecuacion(residuales_var, lags=12, variables=variables)
print(arch_12)

# Test de normalidad (Jarque-Bera para series multivariadas)
normalidad_var =  .test_normality()
print(normalidad_var.summary())

normalidad_univariada = prueba_normalidad_por_ecuacion(
    residuales_var,
    variables=variables,
)
print(normalidad_univariada)


# %% 2.4 Uso del modelo: Pronostico y funciones impulso respueta ----


# %% 2.4.1 Pronostico ----

# Pronóstico
pronostico_var = predecir_var(
    ,
    n_ahead=,
    ci=0.95,
    indice=series.index,
    variables=variables,
)
print(pronostico_var)

# Gráfica del pronóstico
fig_pronostico, axes_pronostico = graficar_pronostico_var(
    pronostico_var["pronostico"],
    pronostico_var["inferior"],
    pronostico_var["superior"],
)
fig_pronostico.suptitle("Pronostico VAR", fontsize=11)
fig_pronostico.tight_layout()
mostrar_graficas()


# %% 2.4.2 Funciones Impulso Respuesta (IRF) ----

# Matrices asociadas a las funciones impulso respuesta
print( .irf().irfs)

# Matrices asociadas a las funciones impulso respuesta ortogonalizadas
print( .irf().orth_irfs)

# Cargue las funciones auxiliares para graficar las IRF
# En Python estas funciones se importaron al inicio desde
# funciones_auxiliares_graficacion_VAR.py.

# Parametros de las graficas de las IRF
variables = list(series.columns)
pasos_adelante = np.arange(0, )
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
    ortog=False,
    int_conf=int_conf_irf,
    prefijo_titulo="Impulso",
    semilla=semilla_irf,
    runs=repeticiones_bootstrap_irf,
)

# Grilla de IRF: columnas = impulsos; filas = respuestas.
mostrar_graficas()

# Graficacion de las IRF ortogonalizadas

# Usamos los mismos pasos adelante, intervalo de confianza y semilla.
# IRF ortogonalizadas de las variables del sistema ante distintos choques exogenos.
irf_ortog = graficar_grilla_irf(
    VAR_estimado,
    variables,
    pasos_adelante,
    ortog=True,
    int_conf=int_conf_irf,
    prefijo_titulo="Impulso ortogonal",
    semilla=semilla_irf,
    runs=repeticiones_bootstrap_irf,
)

# Grilla de OIRF: columnas = impulsos; filas = respuestas.
mostrar_graficas()

# %%
