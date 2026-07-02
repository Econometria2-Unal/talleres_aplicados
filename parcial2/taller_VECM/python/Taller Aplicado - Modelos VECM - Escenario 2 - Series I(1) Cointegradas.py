# Taller Aplicado - Modelos VECM
# Escenario 2: Series I(1) Cointegradas

# Nota: Tips practicos en Python
## Para limpiar el entorno en IPython/Jupyter se puede correr: "%reset -f"
## Para cerrar todas las graficas actualmente abiertas: "plt.close('all')"
## En VS Code o Spyder, los bloques marcados con "# %%" se ejecutan por celdas.


# %% 0. Preliminares ---- ----------------------------------------------------

# Paquetes requeridos

## Trabajar con rutas absolutas en python
from pathlib import Path
import os

## Procesamiento de datos
import numpy as np
import pandas as pd

## Bibliotecas para el trabajo de series de tiempo
import matplotlib.pyplot as plt
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.vector_ar.vecm import VECM, coint_johansen

# Define el directorio de trabajo sobre el cual va a trabajar
project_path = Path(
    r""
)
os.chdir(project_path)

from funciones_auxiliares_VECM import (
    configurar_entorno_graficas,
    extraer_matrices_vecm,
    graficar_grilla_irf,
    graficar_pronostico_vecm,
    graficar_ts,
    imprimir_adf,
    imprimir_matrices_var_reparametrizado,
    imprimir_seleccion_rezagos,
    imprimir_tabla_johansen,
    mostrar_graficas,
    predecir_vecm,
    prueba_arch_por_ecuacion,
    prueba_normalidad_por_ecuacion,
)

# Para configurar las caracteristicas de las graficas
configurar_entorno_graficas(max_columns=30)

# Importe la base de datos desde Excel
datos_escenario2 = pd.read_excel("")


# %% 1. Escenario 2: Series I(1)  -----------------------------------------------------------


# %% 1.1 Escenario 2: Exploracion previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------

# Inspeccione sus datos
.info()
print(.head())

# Transforme la base de datos que importo en un objeto de serie de tiempo
# de pandas, usando un indice temporal trimestral.
tiempo = pd.period_range(
    start="",
    periods=len(),
    freq="",
    name="tiempo",
)
series_escenario2 = pd.DataFrame(
    .to_numpy(),
    index=tiempo,
    columns= .columns,
)

# Grafique las series de tiempo
fig_series, axes_series = plt.subplots(1, 2, figsize=(10, 4))

graficar_ts( ["y"], "Variable y", "lightblue", ax=axes_series[0])
graficar_ts( ["x"], "Variable x", "royalblue", ax=axes_series[1])

fig_series.tight_layout()
mostrar_graficas()

# Realice las pruebas de Dickey-Fuller aumentadas. Son las series de tiempo estacionarias?
# Para los parametros de la prueba de Dickey-Fuller, use las siguientes especificaciones:
# maxlag=3, autolag="AIC", regression="n".

## Prueba de ADF para la primera serie de tiempo
adf1 = adfuller(
    .iloc[:, 0],
    maxlag=3,
    autolag="AIC",
    regression="n",
)
imprimir_adf(adf1, .columns[0])

adf2 = adfuller(
    .iloc[:, 1],
    maxlag=3,
    autolag="AIC",
    regression="n",
)
imprimir_adf(adf2, .columns[1])


# %% 1.2 Escenario 2: Identificacion del numero de rezagos apropiado p del VAR(p) y de su reparametrizacion VECM(p-1) -----------------------

# Nombre de las series de tiempo individuales
variables = list(.columns)

# Para crear el objeto VAR con statsmodels quitamos el indice temporal y
# dejamos solo los datos. El indice trimestral se conserva en series para
# graficas y tablas.
Y_modelo = .reset_index(drop=True)

# %% 1.2.1 Escenario 2: Identificacion del numero de rezagos apropiado p del VAR(p) ----

# Creación del objeto de tipo statsmodels.tsa.vector_ar.var_model.VAR 
modelo_var_escenario2 = VAR()

# Seleccion de rezagos para un VAR
seleccion_rezagos_escenario2 = .select_order(
    maxlags=6,
    trend="n",
)
imprimir_seleccion_rezagos(
    seleccion_rezagos_escenario2,
    "Seleccion de rezagos para un VAR sin terminos deterministas",
    incluir_rezago_cero=False,
)


# %% 1.2.2 Escenario 2: Estimacion del modelo VAR ----


# Nota: La razon por la que se estima el modelo VAR(p), se debe a que sus errores,
#       si se escogio el rezago correcto del VAR, seran ruido blanco,
#       Independientemente de si las series del VAR son I(0) o I(1)
#       Los errores del VAR(p) seran exactamente los mismos que los de su
#       reparametrizacion en forma de un VECM(p-1), por lo que si los residuales
#       del modelo VAR que se estimara satisfacen los supuestos del modelo,
#       entonces los residuales del VECM(p-1) tambien lo haran.
VAR_estimado2 = .fit(2, trend="n")

# Resumen completo de los resultados de la estimacion del VAR
print(.summary())


# %% 1.2.3 Escenario 2: Validacion de supuestos del modelo ----


residuales_VAR_estimado2 = pd.DataFrame(
    np.asarray(.resid),
    index=series_escenario2.index[ .k_ar :],
    columns=variables,
)

# Test de no correlacion serial
P_10 = .test_whiteness(nlags=10, adjusted=False)
print(P_10.summary())

# Test de heterocedasticidad
arch_VAR_estimado2 = prueba_arch_por_ecuacion(
    residuales_VAR_estimado2,
    lags=12,
    variables=variables,
)

# Test de normalidad (Jarque-Bera para series multivariadas)
normalidad_VAR_estimado2 = .test_normality()
print(normalidad_VAR_estimado2.summary())

normalidad_univariada_VAR_estimado2 = prueba_normalidad_por_ecuacion(
    residuales_VAR_estimado2,
    variables=variables,
)


# %% 1.2.4 Escenario 2: Realizacion del Test de Johansen, para determinar el rango de la matriz Pi ----


# En statsmodels se usa k_ar_diff = p - 1, donde p es el numero de rezagos del
# VAR en niveles usado en el test de Johansen.
k_ar_diff = 2 - 1

# Prueba de Johansen
johansen_test = coint_johansen(
    ,
    det_order= ,
    k_ar_diff= ,
)

# Criterio del valor propio máximo
tabla_valor_propio_maximo = imprimir_tabla_johansen(
    ,
    tipo="eigen",
    titulo="Johansen con constante en la relación de cointegración - criterio del valor propio maximo",
)

# Criterio de la traza
tabla_traza = imprimir_tabla_johansen(
    ,
    tipo="trace",
    titulo="Johansen con constante en la relación de cointegración - criterio de la traza",
)


# %% 1.2.5 Escenario 2: Dado que las series son I(1) y estan cointegradas, se estima un modelo VECM(1) ----

# Creación del objeto de tipo statsmodels.tsa.vector_ar.vecm.VECM
# 
# Se especifica el parámetro coint_rank=1 de acuerdo con el resultado del test de
# Johansen.
VECM_escenario2 = VECM(
    ,
    k_ar_diff= ,
    coint_rank= ,
    deterministic= ,
)

# Estimación del modelo VEC 
VECM_escenario2_fit = .fit()
print(.summary())

# Vector de cointegracion normalizado
matrices_vecm_escenario2 = extraer_matrices_vecm(
    ,
    variables=variables,
)
print("\nVector de cointegracion normalizado (beta):")
print( ["beta"])

# Coeficientes de velocidad de ajuste
print("\nCoeficientes de velocidad de ajuste (alpha):")
print( ["alpha"])


# %% 1.2.6 Escenario 2: Reparametrizacion del VECM como un VAR en niveles ----

# Para la reparametrizacion del VECM(1) en un VAR(2) en niveles, statsmodels
# guarda la representacion directamente en el resultado estimado del VECM.
matrices_var_reparametrizado_escenario2 = imprimir_matrices_var_reparametrizado(
    ,
    variables=variables,
)

# Validacion de supuestos del modelo VAR(2) reparametrizado
residuales_VECM_escenario2 = pd.DataFrame(
    np.asarray( .resid),
    index=series_escenario2.index[ .k_ar :],
    columns=variables,
)

# Test de no correlacion serial
P_10_VECM = .test_whiteness(nlags=10)
print(P_10_VECM.summary())

# Test de heterocedasticidad
arch_VECM_escenario2 = prueba_arch_por_ecuacion(
    residuales_VECM_escenario2,
    lags=12,
    variables=variables,
)

# Test de normalidad (Jarque-Bera para series multivariadas)
normalidad_VECM_escenario2 = .test_normality()
print(normalidad_VECM_escenario2.summary())

normalidad_univariada_VECM_escenario2 = prueba_normalidad_por_ecuacion(
    residuales_VECM_escenario2,
    variables=variables,
)


# %% 1.2.7 Escenario 2: Pronosticos del modelo VAR(2) reparametrizado ----


# Pronóstico
pronosticos_escenarios2 = predecir_vecm(
    ,
    n_ahead=,
    ci=0.95,
    indice=series_escenario2.index,
    variables=variables,
)
print(pronosticos_escenarios2)

# Gráfica del pronóstico
fig_pronostico, axes_pronostico = graficar_pronostico_vecm(
    pronosticos_escenarios2["pronostico"],
    pronosticos_escenarios2["inferior"],
    pronosticos_escenarios2["superior"],
)
fig_pronostico.suptitle("Pronostico VECM reparametrizado", fontsize=11)
fig_pronostico.tight_layout()
mostrar_graficas()


# %% 1.2.8 Escenario 2: Funciones Impulso Respuesta (IRF) ----

# Matrices asociadas a las funciones impulso respuesta
matrices_phi = .irf().irfs
print("\nMatrices Phi asociadas a las funciones impulso respuesta")
print(matrices_phi)

# Matrices asociadas a las funciones impulso respuesta ortogonalizadas
matrices_psi = .irf().orth_irfs
print("\nMatrices Psi asociadas a las funciones impulso respuesta ortogonalizadas")
print(matrices_psi)

# Parametros de las graficas de las IRFs
variables_irf = ["x", "y"]
pasos_adelante = np.arange(0, )
int_conf_irf = 0.95
semilla_irf = 202601
repeticiones_bootstrap_irf = 100 # Bootstrappings empleados para construir los IC de las IRFs

# La funcion graficar_grilla_irf() calcula el objeto irf() una sola vez
# y luego crea cada panel con programacion funcional.

# IRF No Ortogonalizadas.

# IRF de las variables del sistema ante distintos choques exogenos.
irf_no_ortog = graficar_grilla_irf(
    VECM_escenario2_fit,
    variables_irf,
    pasos_adelante,
    ortog=False,
    int_conf=int_conf_irf,
    prefijo_titulo="Impulso",
    semilla=semilla_irf,
    runs=repeticiones_bootstrap_irf,
)

# Grilla de IRF: columnas = impulsos; filas = respuestas.
mostrar_graficas()

# IRF Ortogonalizadas.

irf_ortog = graficar_grilla_irf(
    VECM_escenario2_fit,
    variables_irf,
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
