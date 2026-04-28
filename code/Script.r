# ==============================================================
# ANÁLISIS EXPLORATORIO — MANANTIALES COLOMBIANOS
# ==============================================================
# OBJETIVOS:
# 1. Describir la distribución de las variables fisicoquímicas
#    principales usando histogramas y distribución acumulada.
# 2. Identificar el tipo de agua más frecuente en Colombia
#    usando diagramas de barras y torta.
# 3. Comparar el olor del agua según su clasificación química.
# 4. Explorar la relación entre temperatura y pH del manantial.
# 5. Identificar qué variables tienen más datos faltantes
#    antes de la limpieza.
# ==============================================================


# Carga de librerías
install.packages("here")
library(here)
library(ggplot2)
library(readr)


# Carga y limpieza de datos

datos <- read_csv2(
    here("data", "raw", "Manantial_limpio.csv"),
    locale = locale(encoding = "UTF-8"),
    na = c("", "NA", "-999")
)

# El CSV tiene valores -999 que significan "dato faltante".
# read_csv2 ya los convierte a NA gracias a na = c(..., "-999").

# Extraemos el AÑO desde la columna FECHA
datos <- datos %>%
    mutate(
        FECHA = as.Date(FECHA, format = "%m/%d/%Y"),
        ANIO  = as.character(year(FECHA))
    )

# CLASIFICACIÓN tiene errores de escritura (mayúsculas/minúsculas mezcladas
# y espacios inconsistentes alrededor de guiones, ej: "Clorurada-Sulfatada"
# vs "Clorurada - Sulfatada").
# Pasos de limpieza:
#   1. str_trim()   → elimina espacios al inicio y al final
#   2. str_squish() → colapsa espacios internos múltiples en uno
#   3. gsub()       → estandariza los separadores: siempre " - " (con espacios)
#   4. str_to_title() → unifica mayúsculas/minúsculas

normalizar <- function(x) {
    x <- str_trim(x) # quita espacios inicio/fin
    x <- str_squish(x) # colapsa espacios internos
    # Normaliza cualquier variante de guion (con o sin espacios) a " - "
    x <- gsub("\\s*-\\s*", " - ", x)
    # Normaliza barras "/" igual: " / "
    x <- gsub("\\s*/\\s*", " / ", x)
    x <- str_to_title(x) # Title Case
    return(x)
}

datos <- datos %>%
    mutate(
        CLASIFICACION_LIMPIA = normalizar(CLASIFICACIÓN),
        OLOR_LIMPIO          = normalizar(OLOR)
    )

# Revisamos cuántas filas y columnas tenemos
cat("Filas:", nrow(datos), "| Columnas:", ncol(datos), "\n")


# Función para hallar la moda

moda <- function(x) {
    tabla <- table(x)
    max_freq <- max(tabla)
    as.numeric(names(tabla[tabla == max_freq]))
}


# ESTADÍSTICAS DESCRIPTIVAS
# Para el objetivo #1

# --- pH ---
# El pH mide la acidez. Va de 0 (muy ácido) a 14 (muy básico). 7 es neutro.
# La media está por debajo de la mediana (6.33 vs 6.51),
# lo que sugiere que hay algunos valores muy bajos (ácidos) que la jalan.
mean(datos$PH_LABORATORIO, na.rm = TRUE) # na.rm=true : ignora los n/a
median(datos$PH_LABORATORIO, na.rm = TRUE)
moda(datos$PH_LABORATORIO)
sd(datos$PH_LABORATORIO, na.rm = TRUE) # desv. muestral
sqrt(mean((datos$PH_LABORATORIO - mean(datos$PH_LABORATORIO, na.rm = TRUE))^2, na.rm = TRUE)) # desv. poblacional

# --- Temperatura ---
# Temperatura del agua en °C al salir del manantial.
# La media (41.75°C) > mediana (39°C): algunos manantiales muy calientes
# jalan la media hacia arriba → distribución con cola a la derecha.

# Se identificaron valores de 0°C en la columna TEMPERATUR, los cuales
# fueron reemplazados por NA.
temp <- datos$TEMPERATUR

# Reemplazar 0 por NA
temp[temp == 0] <- NA
mean(temp, na.rm = TRUE)
median(temp, na.rm = TRUE)
moda(temp)
sd(temp, na.rm = TRUE)
sqrt(mean((temp - mean(temp, na.rm = TRUE))^2, na.rm = TRUE))

# --- Conductividad ---
# Sales disueltas totales en µS/cm. Rango enorme: 0 a 278,000.
# Media (4375) >> Mediana (1360): hay muy pocos manantiales
# con salinidad extrema que distorsionan fuertemente la media.
mean(datos$CONDUCTIVIDAD, na.rm = TRUE)
median(datos$CONDUCTIVIDAD, na.rm = TRUE)
moda(datos$CONDUCTIVIDAD)
sd(datos$CONDUCTIVIDAD, na.rm = TRUE)
sqrt(mean((datos$CONDUCTIVIDAD - mean(datos$CONDUCTIVIDAD, na.rm = TRUE))^2, na.rm = TRUE))

# --- Calcio ---
mean(datos$CALCIO, na.rm = TRUE)
median(datos$CALCIO, na.rm = TRUE)
moda(datos$CALCIO)
sd(datos$CALCIO, na.rm = TRUE)
sqrt(mean((datos$CALCIO - mean(datos$CALCIO, na.rm = TRUE))^2, na.rm = TRUE))

# --- Sodio ---
mean(datos$SODIO, na.rm = TRUE)
median(datos$SODIO, na.rm = TRUE)
moda(datos$SODIO)
sd(datos$SODIO, na.rm = TRUE)
sqrt(mean((datos$SODIO - mean(datos$SODIO, na.rm = TRUE))^2, na.rm = TRUE))


# ==============================================================
# GRÁFICO 1: HISTOGRAMA DE pH
# (Objetivo 1)
# ==============================================================
# POR QUÉ ESTE GRÁFICO: El pH es numérica continua → histograma.
# Queremos ver si los manantiales son mayormente ácidos, neutros o básicos.
# bins = 9 porque tenemos 320 filas → regla de Sturges: 1 + log2(320) ≈ 9

ggplot(datos, aes(x = PH_LABORATORIO)) +
    geom_histogram(bins = 9, fill = "#805233", color = "white") +
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
        subtitle = "Cyan = Media (6.33)  |  Azul = Mediana (6.51)",
        x = "pH (laboratorio)",
        y = "Número de manantiales"
    )
# CONCLUSIÓN: La mayoría de manantiales son ligeramente ácidos (pH entre 5 y 7).
# La media menor que la mediana indica que algunos pozos muy ácidos
# arrastran la media hacia abajo.


# ==============================================================
# GRÁFICO 2: HISTOGRAMA DE TEMPERATURA
# (Objetivo 1)
# ==============================================================
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


# ==============================================================
# GRÁFICO 3: HISTOGRAMA DE CONDUCTIVIDAD
# (Objetivo 1)
# ==============================================================
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


# ==============================================================
# GRÁFICO 4: DISTRIBUCIÓN ACUMULADA (ECDF) — pH
# (Objetivo 1)
# ==============================================================
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


# ==============================================================
# GRÁFICO 5: DISTRIBUCIÓN ACUMULADA — Temperatura
# (Objetivo 1)
# ==============================================================
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
    guides(fill = "none") +   # la leyenda es redundante con el eje
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

ggplot(datos, aes(x = CLASIFICACION_LIMPIA, fill = OLOR_LIMPIO)) +
    geom_bar(position = "dodge") +
    coord_flip() +
    labs(
        title = "Tipo de agua según su olor",
        x = NULL, y = "Frecuencia", fill = "Olor"
    )


# ── PIE CHART ─────────────────────────────────────────────────
# NA no es categoría real: se filtra ANTES de pasar a ggplot.

datos %>%
    filter(!is.na(OLOR_LIMPIO)) %>%
    ggplot(aes(x = "", fill = OLOR_LIMPIO)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = "Distribución porcentual del olor del agua", fill = "Olor")

datos %>%
    filter(!is.na(CLASIFICACION_LIMPIA)) %>%
    ggplot(aes(x = "", fill = CLASIFICACION_LIMPIA)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = "Proporción de tipos de agua", fill = "Clasificación")


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
