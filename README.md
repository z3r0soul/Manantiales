# Manantiales

Análisis estadístico exploratorio del inventario geoquímico de manantiales y
fumarolas de Colombia (~320 puntos de afloramiento de agua subterránea).
El proyecto caracteriza el estado fisicoquímico de estos sistemas
(pH, temperatura, conductividad, sodio, calcio) mediante estadística
descriptiva clásica y robusta, estimación de densidad kernel, análisis de
distancias/clustering, y visualización, como base para una etapa posterior
de imputación, normalización y modelado predictivo.

## Estructura del proyecto

```
Manantiales/
├── main.R                          # Script principal: orquesta todo el pipeline
├── code/
│   ├── funciones/
│   │   └── funciones.r             # Funciones auxiliares compartidas
│   ├── setup.R                     # Carga de librerías y configuración global
│   ├── carga_datos.R               # Lectura del CSV crudo
│   ├── limpieza.R                  # Tratamiento de NA, valores erróneos (-999, ceros, negativos)
│   ├── integridad_datos.R          # Verificación de calidad/consistencia de los datos
│   ├── estadistica_descriptiva.R   # Medidas de tendencia central, dispersión y forma
│   ├── histogramas.R               # Histogramas por variable numérica
│   ├── barras_y_frecuencias.R      # Gráficos de barras y tablas de frecuencia (categóricas)
│   ├── boxplots.R                  # Boxplots comparativos
│   ├── dispersion_y_correlacion.R  # Scatterplots, correlaciones y matriz de dispersión (ggpairs)
│   ├── kernel.R                    # Estimación de densidad kernel (bandwidth y funciones kernel)
│   ├── tallo_y_hoja.R              # Diagramas de tallo y hoja
│   ├── distancias.R                # Distancias, dendrogramas y clustering jerárquico
│   └── similaridad.R               # Medidas de similaridad entre observaciones
├── data/
│   └── raw/
│       └── Manantial_limpio.csv    # Dataset crudo (valores faltantes codificados como -999)
├── output/                         # Gráficos exportados (creada automáticamente)
└── README.md
```

## Requisitos

- R >= 4.2
- RStudio (recomendado, para usar `here()` correctamente vía `.Rproj`)

Paquetes de R utilizados en el proyecto:

```r
install.packages(c(
  "here", "tidyverse",     # dplyr, readr, ggplot2, tidyr, etc.
  "GGally",                # ggpairs (matriz de dispersión)
  "patchwork",             # composición de gráficos
  "cluster"                # distancias / clustering
))
```

> Si `main.R` detecta que falta un paquete, instálalo manualmente con
> `install.packages("nombre_paquete")` antes de volver a correr el script.

## Cómo ejecutar

1. Clona el repositorio y ábrelo como proyecto de RStudio (si existe un
   `.Rproj`, ábrelo directamente; si no, asegúrate de que el directorio de
   trabajo sea la raíz del repositorio para que `here()` funcione bien).
2. Abre `main.R`.
3. Ejecuta con **Source** (o `Ctrl+Shift+S` / botón "Source") para correr
   todo el pipeline de una sola vez, en este orden:

   `funciones.r → setup.R → carga_datos.R → limpieza.R → integridad_datos.R →
   estadistica_descriptiva.R → histogramas.R → barras_y_frecuencias.R →
   boxplots.R → dispersion_y_correlacion.R → kernel.R → tallo_y_hoja.R →
   distancias.R → similaridad.R`

4. Los gráficos se muestran en el panel de **Plots** de RStudio y (según
   configuración de cada script) se guardan también en `output/`.

## Datos

- **Fuente:** `data/raw/Manantial_limpio.csv`
- **Codificación de faltantes:** el valor `-999` representa un dato faltante
  y se convierte a `NA` en `carga_datos.R`.
- **Variables numéricas principales:** pH (`PH_LABORATORIO`), temperatura
  (`TEMPERATUR`), conductividad eléctrica (`CONDUCTIVIDAD`), sodio (`SODIO`),
  calcio (`CALCIO`).
- **Variables categóricas principales:** clasificación geoquímica del agua
  (Bicarbonatada, Sulfatada, etc.) y olor percibido.
- **Limpieza aplicada:** se identificaron y trataron como `NA` valores
  imposibles (pH = 0, temperatura = 0 °C, concentraciones negativas de
  algunos iones), documentados en `limpieza.R` e `integridad_datos.R`.

## Notas / problemas conocidos

- **Gráfico `matriz_diag` (`ggpairs`) en blanco al correr `main.R` completo:**
  es un problema conocido de RStudio (`display list redraw incomplete`), no
  del código. Solución: **Tools → Global Options → General → Graphics →
  Backend → AGG o Cairo**, reiniciar la sesión de R (`Session → Restart R`)
  y volver a correr `main.R`.

## Autor

Santiago Vargas Gomez
