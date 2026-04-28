# ANÁLISIS EXPLORATORIO — MANANTIALES COLOMBIANOS
# OBJETIVOS:
# 1. Describir la distribución de las variables fisicoquímicas
#    principales usando histogramas y distribución acumulada.
# 2. Identificar el tipo de agua más frecuente en Colombia
#    usando diagramas de barras y torta.
# 3. Comparar el olor del agua según su clasificación química.
# 4. Explorar la relación entre temperatura y pH del manantial.
# 5. Identificar qué variables tienen más datos faltantes
#    antes de la limpieza.


# Instalar paquetes si no están instalados
if (!require("here")) install.packages("here")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")

# Cargar librerías
library(here)
library(ggplot2)
library(readr)
library(moments)
library(dplyr) # %>%, mutate, filter, count
library(lubridate) # year()
library(stringr) # str_trim, str_squish, str_to_title

# Carga de funciones personalizadas
source(here("code", "funciones.r"))


# 1. Carga de datos desde el archivo CSV, identificando valores erróneos y faltanates como NA
# El CSV tiene valores -999 que significan "dato faltante".
# read_csv2 ya los convierte a NA gracias a na = c(..., "-999").

datos <- read_csv2(
    here("data", "raw", "Manantial_limpio.csv"),
    locale = locale(encoding = "UTF-8"),
    na = c("", "NA", "-999")
)

# 2. Extracción del AÑO y transformación de la columna FECHA
# Aquí extraemos el AÑO de la columna FECHA y lo guardamos en una nueva columna llamada ANIO.
# La columna FECHA se convierte a formato de fecha y se extrae el año.
# as.Date(FECHA, format = "%m/%d/%Y") convierte la columna FECHA a formato de fecha
# year(FECHA) extrae el año de la columna FECHA
# as.character() convierte el año a texto (Categorización de ANIO)

datos <- datos %>%
    mutate(
        FECHA = as.Date(FECHA, format = "%m/%d/%Y"),
        ANIO  = as.character(year(FECHA))
    )


# 3.  Con tidyverse, mutate se utiliza para crear una nueva columna a partir de otra ya existente.
# En este caso, se crea la columna CLASIFICACION_LIMPIA a partir de la columna CLASIFICACIÓN y
# la columna OLOR_LIMPIO a partir de la columna OLOR.
datos <- datos %>%
    mutate(
        CLASIFICACION_LIMPIA = normalizar(CLASIFICACIÓN),
        OLOR_LIMPIO          = normalizar(OLOR)
    )

# Revisamos cuántas filas y columnas tenemos (Solo es un debuggeo)
cat("Filas:", nrow(datos), "| Columnas:", ncol(datos), "\n")

# ESTADÍSTICAS DESCRIPTIVAS
# Para el objetivo #1

# ── Ahora aplicamos todo a cada variable ──────────────────────

# pH — va de 0 a 14. Mínimo de 0.0 es sospechoso, el rango lo revela.
cat("── pH ──\n")
mean(datos$PH_LABORATORIO, na.rm = TRUE) # media
mean(datos$PH_LABORATORIO, na.rm = TRUE, trim = 0.05) # media recortada 5%
median(datos$PH_LABORATORIO, na.rm = TRUE) # mediana
moda(datos$PH_LABORATORIO) # moda
sd(datos$PH_LABORATORIO, na.rm = TRUE) # desv. estándar muestral
sd_pobl(datos$PH_LABORATORIO) # desv. estándar poblacional
meda(datos$PH_LABORATORIO) # MEDA
skewness(na.omit(datos$PH_LABORATORIO)) # asimetría
kurtosis(na.omit(datos$PH_LABORATORIO)) # curtosis
max(datos$PH_LABORATORIO, na.rm = TRUE) - min(datos$PH_LABORATORIO, na.rm = TRUE) # rango

# Temperatura — rango esperado 0-94°C, valores en 0 son sospechosos
cat("── Temperatura ──\n")
mean(datos$TEMPERATUR, na.rm = TRUE)
mean(datos$TEMPERATUR, na.rm = TRUE, trim = 0.05)
median(datos$TEMPERATUR, na.rm = TRUE)
moda(datos$TEMPERATUR)
sd(datos$TEMPERATUR, na.rm = TRUE)
sd_pobl(datos$TEMPERATUR)
meda(datos$TEMPERATUR)
skewness(na.omit(datos$TEMPERATUR))
kurtosis(na.omit(datos$TEMPERATUR))
max(datos$TEMPERATUR, na.rm = TRUE) - min(datos$TEMPERATUR, na.rm = TRUE)

# Conductividad — rango enorme (0 a 278,000): media recortada muy útil aquí
cat("── Conductividad ──\n")
mean(datos$CONDUCTIVIDAD, na.rm = TRUE)
mean(datos$CONDUCTIVIDAD, na.rm = TRUE, trim = 0.05)
median(datos$CONDUCTIVIDAD, na.rm = TRUE)
moda(datos$CONDUCTIVIDAD)
sd(datos$CONDUCTIVIDAD, na.rm = TRUE)
sd_pobl(datos$CONDUCTIVIDAD)
meda(datos$CONDUCTIVIDAD)
skewness(na.omit(datos$CONDUCTIVIDAD))
kurtosis(na.omit(datos$CONDUCTIVIDAD))
max(datos$CONDUCTIVIDAD, na.rm = TRUE) - min(datos$CONDUCTIVIDAD, na.rm = TRUE)

# Calcio
cat("── Calcio ──\n")
mean(datos$CALCIO, na.rm = TRUE)
mean(datos$CALCIO, na.rm = TRUE, trim = 0.05)
median(datos$CALCIO, na.rm = TRUE)
moda(datos$CALCIO)
sd(datos$CALCIO, na.rm = TRUE)
sd_pobl(datos$CALCIO)
meda(datos$CALCIO)
skewness(na.omit(datos$CALCIO))
kurtosis(na.omit(datos$CALCIO))
max(datos$CALCIO, na.rm = TRUE) - min(datos$CALCIO, na.rm = TRUE)

# Sodio — media (706) muy lejana a mediana (171): caso ideal para media recortada
cat("── Sodio ──\n")
mean(datos$SODIO, na.rm = TRUE)
mean(datos$SODIO, na.rm = TRUE, trim = 0.05)
median(datos$SODIO, na.rm = TRUE)
moda(datos$SODIO)
sd(datos$SODIO, na.rm = TRUE)
sd_pobl(datos$SODIO)
meda(datos$SODIO)
skewness(na.omit(datos$SODIO))
kurtosis(na.omit(datos$SODIO))
max(datos$SODIO, na.rm = TRUE) - min(datos$SODIO, na.rm = TRUE)

# GRÁFICO 1: HISTOGRAMA DE pH
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: El pH es numérica continua → histograma.
# Queremos ver si los manantiales son mayormente ácidos, neutros o básicos.
# bins = 9 porque tenemos 320 filas → regla de Sturges: 1 + log2(320) ≈ 9

ggplot(datos, aes(x = PH_LABORATORIO)) +
    geom_histogram(bins = 9, fill = "#402816", color = "white") +
    geom_vline(
        xintercept = mean(datos$PH_LABORATORIO, na.rm = TRUE),
        color = "#25a2d3", linetype = "dashed"
    ) +
    geom_vline(
        xintercept = median(datos$PH_LABORATORIO, na.rm = TRUE),
        color = "#ff3ace52", linetype = "dotted"
    ) +
    labs(
        title = "Distribución del pH en manantiales colombianos",
        subtitle = " Rosa = Media (6.33)  |  Azul = Mediana (6.51)",
        x = "pH (laboratorio)",
        y = "Número de manantiales"
    )
# CONCLUSIÓN: La mayoría de manantiales son ligeramente ácidos (pH entre 5 y 7).
# La media menor que la mediana indica que algunos pozos muy ácidos
# arrastran la media hacia abajo.


# GRÁFICO 2: HISTOGRAMA DE TEMPERATURA
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: Temperatura es numérica continua → histograma.
# Nos permite clasificar si el agua es fría (<20°C) o termal (>20°C).

ggplot(datos, aes(x = TEMPERATUR)) +
    geom_histogram(bins = 9, fill = "#8bbee0", color = "white") +
    geom_vline(
        xintercept = mean(datos$TEMPERATUR, na.rm = TRUE),
        color = "firebrick", linetype = "dashed"
    ) +
    geom_vline(
        xintercept = median(datos$TEMPERATUR, na.rm = TRUE),
        color = "darkorange", linetype = "dotted"
    ) +
    labs(
        title = "Distribución de temperatura del agua",
        subtitle = "Rojo = Media (41.75°C)  |  Naranja = Mediana (39°C)",
        x = "Temperatura (°C)",
        y = "Número de manantiales"
    )
# CONCLUSIÓN: La mayoría del agua sale entre 30°C y 60°C → aguas termales.
# La cola a la derecha (valores >80°C) explica que la media supere a la mediana.


# GRÁFICO 3: HISTOGRAMA DE CONDUCTIVIDAD
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: Conductividad es numérica continua → histograma.
# Usamos escala logarítmica porque el rango es enorme (0 a 278,000 µS/cm).
# Sin log, el 95% de los datos se aplasta en la izquierda y no se ve nada.

ggplot(datos, aes(x = CONDUCTIVIDAD)) +
    geom_histogram(bins = 9, fill = "darkgreen", color = "white") +
    scale_x_log10() + # escala logarítmica en el eje X
    labs(
        title = "Distribución de conductividad eléctrica (escala log)",
        subtitle = "Escala logarítmica usada por el rango extremo de valores",
        x = "Conductividad (µS/cm) — escala log",
        y = "Número de manantiales"
    )
# CONCLUSIÓN: La mayoría de manantiales tiene conductividad entre 100 y 10,000 µS/cm.
# Unos pocos superan los 100,000, indicando agua muy salina (probablemente fumarolas).


# GRÁFICO 4: DISTRIBUCIÓN ACUMULADA (ECDF) — pH
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: El ECDF para pH nos permite afirmar cosas concretas.
# Por ejemplo: "el X% de los manantiales tiene pH menor a 7 (son ácidos)".
# Es el gráfico más adecuado para hacer afirmaciones de este tipo.

ggplot(datos, aes(x = PH_LABORATORIO)) +
    stat_ecdf(color = "steelblue", linewidth = 1) +
    geom_vline(xintercept = 7, color = "gray40", linetype = "dashed") +
    labs(
        title = "Distribución acumulada del pH",
        subtitle = "¿Qué % de manantiales tiene pH menor a un valor dado?",
        x = "pH",
        y = "Proporción acumulada de manantiales"
    )
# CONCLUSIÓN: El punto donde la curva cruza x=7 nos dice qué proporción
# de manantiales es ácida. Si la curva llega a 0.75 en pH=7,
# entonces el 75% de los manantiales son ácidos.


# GRÁFICO 5: DISTRIBUCIÓN ACUMULADA — Temperatura
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: Permite afirmar qué porcentaje del agua
# supera los 50°C (umbral de agua termal de alta temperatura).

ggplot(datos, aes(x = TEMPERATUR)) +
    stat_ecdf(color = "tomato", linewidth = 1) +
    geom_vline(xintercept = 50, color = "gray40", linetype = "dashed") +
    labs(
        title = "Distribución acumulada de temperatura",
        subtitle = "Línea gris = 50°C (umbral agua termal de alta temperatura)",
        x = "Temperatura (°C)",
        y = "Proporción acumulada de manantiales"
    )

# ── BARRAS — 1 variable categórica ────────────────────────────
# Usamos las columnas LIMPIAS para que las variantes normalizadas se agrupen.

ggplot(datos, aes(x = CLASIFICACION_LIMPIA, fill = CLASIFICACION_LIMPIA)) +
    geom_bar() +
    guides(fill = "none") + # la leyenda es redundante con el eje
    coord_flip() +
    labs(title = "Tipos de agua", x = NULL, y = "Cantidad de Manantiales")

ggplot(datos, aes(x = OLOR_LIMPIO, fill = OLOR_LIMPIO)) +
    geom_bar() +
    guides(fill = "none") +
    coord_flip() +
    labs(title = "Olor del agua", x = NULL, y = "Cantidad de Manantiales")

ggplot(datos, aes(x = ANIO)) +
    geom_bar(fill = "#00a81e") +
    coord_flip() +
    labs(title = "Mediciones por año", x = NULL, y = "Cantidad de Manantiales")


# ── BARRAS — 2 variables categóricas ──────────────────────────
# fill = segunda variable  →  barras lado a lado con position = "dodge"

datos %>%
    filter(!is.na(CLASIFICACION_LIMPIA), !is.na(OLOR_LIMPIO)) %>%
    ggplot(aes(x = CLASIFICACION_LIMPIA, fill = OLOR_LIMPIO)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(
        title    = "Proporción de olor según el tipo de agua",
        subtitle = "Cada barra representa el 100% de los manantiales de ese tipo",
        x        = NULL,
        y        = "Proporción",
        fill     = "Olor"
    )


# ── PIE CHART ─────────────────────────────────────────────────
# NA no es categoría real: se filtra ANTES de pasar a ggplot.

olor_tabla <- table(na.omit(datos$OLOR_LIMPIO))
olor_pct <- round(prop.table(olor_tabla) * 100, 1)
olor_labels <- paste0(names(olor_tabla), "\n", olor_pct, "%")
pie(olor_tabla, labels = olor_labels, main = "Distribución porcentual
 del olor del agua")

clasif_tabla <- table(na.omit(datos$CLASIFICACION_LIMPIA))
clasif_pct <- round(prop.table(clasif_tabla) * 100, 1)
clasif_labels <- paste0(names(clasif_tabla), "\n", clasif_pct, "%")
pie(clasif_tabla, labels = clasif_labels, main = "Proporción de tipos de agua")


# ── DISPERSIÓN — 2 variables numéricas ────────────────────────
# Dos numéricas → geom_point para ver si hay relación entre ellas

ggplot(datos, aes(x = TEMPERATUR, y = PH_LABORATORIO)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
    labs(
        title = "¿El agua más caliente es más ácida?",
        x = "Temperatura (°C)", y = "pH"
    )

ggplot(datos, aes(x = SODIO, y = CONDUCTIVIDAD)) +
    geom_point(alpha = 0.4, color = "darkorange") +
    geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
        title = "Sodio vs Conductividad (escala log)",
        x = "Sodio (mg/L)", y = "Conductividad (µS/cm)"
    )
