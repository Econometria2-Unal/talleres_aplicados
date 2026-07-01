# Taller Aplicado - Modelos VECM (Completamente Desarrollado)
# Escenario 1: Series I(1) No Cointegradas

# Nota: Tips practicos en Python
## Para limpiar el entorno en IPython/Jupyter se puede correr: "%reset -f"
## Para cerrar todas las graficas actualmente abiertas: "plt.close('all')"
## En VS Code o Spyder, los bloques marcados con "# %%" se ejecutan por celdas.


# %% 0. Preliminares ---- ----------------------------------------------------

# Paquetes requeridos

## Trabajar con rutas absolutas en python
from pathlib import Path
import os
import sys

## Procesamiento de datos
import numpy as np
import pandas as pd

## Bibliotecas para el trabajo de series de tiempo
import matplotlib.pyplot as plt
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.vector_ar.vecm import coint_johansen

# Define el directorio de trabajo sobre el cual va a trabajar
project_path = Path(
    r""
)
os.chdir(project_path)

from funciones_auxiliares_VECM import (
    configurar_entorno_graficas,
    graficar_ts,
    imprimir_adf,
    imprimir_seleccion_rezagos,
    imprimir_tabla_johansen,
    mostrar_graficas,
    prueba_arch_por_ecuacion,
    prueba_normalidad_por_ecuacion,
)
from funciones_auxiliares_graficacion_VAR import (
    graficar_grilla_irf as graficar_grilla_irf_var,
    graficar_pronostico_var,
    predecir_var,
)

# Para configurar las caracteristicas de las graficas
configurar_entorno_graficas(max_columns=30)

# Importe la base de datos desde Excel
datos_escenario1 = pd.read_excel("")


# %% 1. Escenario 1: Series I(1)  -----------------------------------------------------------


# %% 1.1 Escenario 1: Exploracion previa de las series de tiempo, antes de realizar el modelamiento -----------------------------------------------------------

# Inspeccione sus datos
 .info()
print( .head())

# Transforme la base de datos que importo en un objeto de serie de tiempo
# de pandas, usando un indice temporal trimestral.
tiempo = pd.period_range(
    start="",
    periods=len(),
    freq="",
    name="tiempo",
)
series_escenario1 = pd.DataFrame(
    .to_numpy(),
    index=tiempo,
    columns=datos_escenario1.columns,
)

# Grafique las series de tiempo
fig_series, axes_series = plt.subplots(1, 2, figsize=(10, 4))

graficar_ts(series_escenario1["y"], "Variable y", "lightblue", ax=axes_series[0])
graficar_ts(series_escenario1["x"], "Variable x", "royalblue", ax=axes_series[1])

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
imprimir_adf(adf2, .columns[0])


# %% 1.2 Escenario 1: Identificacion del numero de rezagos apropiado p del VAR(p) y de su reparametrizacion VECM(p-1) -----------------------

# Nombre de las series de tiempo individuales
variables = list(.columns)

# Para crear el objeto VAR con statsmodels quitamos el indice temporal y
# dejamos solo los datos. El indice trimestral se conserva en series para
# graficas y tablas.
Y_modelo = .reset_index(drop=True)

# %% 1.2.1 Escenario 1: Identificacion del numero de rezagos apropiado p del VAR(p) ----

# Creación del objeto de tipo statsmodels.tsa.vector_ar.var_model.VAR 
modelo_var_escenario1 = VAR()

# Seleccion de rezagos para un VAR
seleccion_rezagos_escenario1 = .select_order(
    maxlags=6,
    trend="n",
)
imprimir_seleccion_rezagos(
    seleccion_rezagos_escenario1,
    "Seleccion de rezagos para un VAR sin terminos deterministas",
    incluir_rezago_cero=False,
)


# %% 1.2.2 Escenario 1: Estimacion del modelo VAR ----


# Nota: La razon por la que se estima el modelo VAR(p), se debe a que sus errores,
#       si se escogio el rezago correcto del VAR, seran ruido blanco,
#       Independientemente de si las series del VAR son I(0) o I(1)
#       Los errores del VAR(p) seran exactamente los mismos que los de su
#       reparametrizacion en forma de un VECM(p-1), por lo que si los residuales
#       del modelo VAR que se estimara satisfacen los supuestos del modelo,
#       entonces los residuales del VECM(p-1) tambien lo haran.
VAR_estimado1 = .fit(2, trend="n")

# Resumen completo de los resultados de la estimacion del VAR
print(.summary())


# %% 1.2.3 Escenario 1: Validacion de supuestos del modelo ----


residuales_VAR_estimado1 = pd.DataFrame(
    np.asarray( .resid),
    index=series_escenario1.index[ .k_ar :],
    columns=variables,
)

# Test de no correlacion serial
P_10 = .test_whiteness(nlags=10, adjusted=False)
print(P_10.summary())

# Test de heterocedasticidad
arch_VAR_estimado1 = prueba_arch_por_ecuacion(
    residuales_VAR_estimado1,
    lags=12,
    variables=variables,
)

# Test de normalidad (Jarque-Bera para series multivariadas)
normalidad_VAR_estimado1 = .test_normality()
print(normalidad_VAR_estimado1.summary())

normalidad_univariada_VAR_estimado1 = prueba_normalidad_por_ecuacion(
    residuales_VAR_estimado1,
    variables=variables,
)


# %% 1.2.4 Escenario 1: Realizacion del Test de Johansen, para determinar el rango de la matriz Pi ----


# En statsmodels se usa k_ar_diff = p - 1, donde p es el numero de rezagos del
# VAR en niveles usado en el test de Johansen.
k_ar_diff = 2 - 1

# Prueba de Johansen
johansen_test = coint_johansen(
    ,
    det_order=,
    k_ar_diff=k_ar_diff,
)

# Criterio del valor propio máximo
tabla_valor_propio_maximo = imprimir_tabla_johansen(
    ,
    tipo="eigen",
    titulo="Johansen sin constante - criterio del valor propio maximo",
)

# Criterio de la traza
tabla_traza = imprimir_tabla_johansen(
    ,
    tipo="trace",
    titulo="Johansen sin constante - criterio de la traza",
)


# %% 1.2.5 Escenario 1: Dado que las series son I(1) y no estan cointegradas, se estima un modelo VAR(1) en diferencias ----

# Estimacion del modelo VAR(1) en diferencias
diff_series_escenario1 = .diff().dropna()

modelo_var_diff_escenario1 = VAR(diff_series_escenario1.reset_index(drop=True))
VAR_diff_estimado1 = modelo_var_diff_escenario1.fit(1, trend="n")

# Resumen completo de los resultados de la estimacion del VAR(1) en diferencias
print(.summary())

# Validacion de supuestos del modelo VAR(1) en diferencias
residuales_VAR_diff_estimado1 = pd.DataFrame(
    np.asarray(.resid),
    index=diff_series_escenario1.index[VAR_diff_estimado1.k_ar :],
    columns=variables,
)

# Test de no correlacion serial
P_10_diff = .test_whiteness(nlags=10, adjusted=False)
print(P_10_diff.summary())

# Test de heterocedasticidad
arch_VAR_diff_estimado1 = prueba_arch_por_ecuacion(
    residuales_VAR_diff_estimado1,
    lags=12,
    variables=variables,
)

# Test de normalidad (Jarque-Bera para series multivariadas)
normalidad_VAR_diff_estimado1 = .test_normality()
print(normalidad_VAR_diff_estimado1.summary())

normalidad_univariada_VAR_diff_estimado1 = prueba_normalidad_por_ecuacion(
    residuales_VAR_diff_estimado1,
    variables=variables,
)


# %% 1.2.6 Escenario 1: Pronosticos del modelo VAR(1) en diferencias ----


pronosticos_escenarios1 = predecir_var(
    ,
    n_ahead=,
    ci=0.95,
    indice=diff_series_escenario1.index,
    variables=variables,
)
print(pronosticos_escenarios1)

fig_pronostico, axes_pronostico = graficar_pronostico_var(
    pronosticos_escenarios1["pronostico"],
    pronosticos_escenarios1["inferior"],
    pronosticos_escenarios1["superior"],
)
fig_pronostico.suptitle("Pronostico VAR(1) en diferencias", fontsize=11)
fig_pronostico.tight_layout()
mostrar_graficas()


# %% 1.2.7 Escenario 1: Funciones Impulso Respuesta (IRF) ----

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
irf_no_ortog = graficar_grilla_irf_var(
    VAR_diff_estimado1,
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

irf_ortog = graficar_grilla_irf_var(
    VAR_diff_estimado1,
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