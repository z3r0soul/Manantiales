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

# ==============================================================
# Instalar paquetes si no están instalados
# ==============================================================
if (!require("here")) install.packages("here")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")

# Cargar librerías
library(patchwork)
library(here)
library(scales)
library(ggplot2)
library(readr)
library(moments)
library(dplyr) # %>%, mutate, filter, count
library(lubridate) # year()
library(stringr) # str_trim, str_squish, str_to_title

# Carga de funciones personalizadas
source(here("code", "funciones.r"))

# =============================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
# =============================================================

# Carga de datos desde el archivo CSV, identificando valores erróneos y faltantes como NA
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

# 4. Categorizar pH en tres grupos con significado geoquímico:
# Ácido < 6.5 | Neutro 6.5–7.5 | Básico > 7.5
# Justificación: el punto de corte 6.5/7.5 es el estándar OMS
# para calidad del agua
datos <- datos %>%
  mutate(
    PH_CAT = case_when(
      PH_LABORATORIO < 6.5  ~ "Ácido",
      PH_LABORATORIO <= 7.5 ~ "Neutro",
      PH_LABORATORIO > 7.5  ~ "Básico",
      TRUE ~ NA_character_
    ),
    PH_CAT = factor(PH_CAT, levels = c("Ácido", "Neutro", "Básico"))
  )

# 5. Temperatura categorizada
# Al igual que PH_CAT, convertir temperatura continua
# en categorías permite análisis bivariado y comunica mejor
# el significado geotérmico que un histograma solo.
# Umbrales: fría <20°C (agua superficial), tibia 20–50°C
# (geotérmica baja), termal >50°C (geotérmica alta / fumarola).

datos <- datos %>%
  mutate(
    TEMP_CAT = case_when(
      TEMPERATUR < 20  ~ "Fría",
      TEMPERATUR <= 50 ~ "Tibia",
      TEMPERATUR > 50  ~ "Termal",
      TRUE ~ NA_character_
    ),
    TEMP_CAT = factor(TEMP_CAT, levels = c("Fría", "Tibia", "Termal"))
  )

# Revisamos cuántas filas y columnas tenemos (Solo es un debuggeo)
cat("Filas:", nrow(datos), "| Columnas:", ncol(datos), "\n")

# =============================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS
# =============================================================
# Se calculan: media, media recortada (5%), mediana, moda,
# desviación estándar muestral y poblacional, varianza muestral,
# MEDA, MAD, coeficiente de variación (CV), asimetría, curtosis y rango.

# pH — va de 0 a 14. Mínimo de 0.0 es sospechoso, el rango lo revela  .
cat("── pH ──\n")
mean(datos$PH_LABORATORIO, na.rm = TRUE) # media
mean(datos$PH_LABORATORIO, na.rm = TRUE, trim = 0.05) # media recortada 5%
median(datos$PH_LABORATORIO, na.rm = TRUE) # mediana
moda(datos$PH_LABORATORIO) # moda
sd(datos$PH_LABORATORIO, na.rm = TRUE) # desv. estándar muestral
meda(datos$PH_LABORATORIO) # MEDA
mad(datos$PH_LABORATORIO, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$PH_LABORATORIO)
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
meda(datos$TEMPERATUR)
mad(datos$TEMPERATUR, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$TEMPERATUR)
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
meda(datos$CONDUCTIVIDAD)
mad(datos$CONDUCTIVIDAD, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$CONDUCTIVIDAD)
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
meda(datos$CALCIO)
mad(datos$CALCIO, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$CALCIO)
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
meda(datos$SODIO)
mad(datos$SODIO, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$CONDUCTIVIDAD)
skewness(na.omit(datos$SODIO))
kurtosis(na.omit(datos$SODIO))
max(datos$SODIO, na.rm = TRUE) - min(datos$SODIO, na.rm = TRUE)



# =============================================================
# TABLAS CRUZADAS BIVARIADAS
# =============================================================

# PH_CAT x TEMP_CAT (pH vs temperatura categóricas)
# Justificación: si el agua termal tiende a ser más ácida,
# se confirma la relación entre temperatura y acidez que
# también muestra el gráfico de dispersión.

datos_pt <- datos %>% filter(!is.na(PH_CAT), 
                             !is.na(TEMP_CAT)
                             )
tabla_pt <- table(pH = datos_pt$PH_CAT, 
                  Temperatura = datos_pt$TEMP_CAT
                  )

cat("\n══ PH_CAT × TEMP_CAT — frecuencias ABSOLUTAS ══\n")
print(tabla_pt)

cat("\n  Proporciones por FILA (% dentro de cada categoría de pH):\n")
print(round(prop.table(tabla_pt, margin = 1) * 100, 1))

cat("\n  Proporciones por COLUMNA (% dentro de cada categoría de temperatura):\n")
print(round(prop.table(tabla_pt, margin = 2) * 100, 1))


# Se reportan frecuencias absolutas y proporciones por fila y columna.
# Por fila: qué % de cada tipo de agua tiene cada pH.
# Por columna: qué % de cada categoría de pH corresponde a cada tipo.

# Tabla cruzada: PH_CAT × CLASIFICACIÓN 
# Justificación: permite ver si el tipo de agua está asociado
# con la acidez del manantial. La Sulfatada debería tener más ácidos
# por la oxidación de sulfuros; la Bicarbonatada, más neutros.
top3 <- c("Bicarbonatada", "Clorurada", "Sulfatada")
datos_top3 <- datos %>%
  filter(CLASIFICACION_LIMPIA %in% top3, !is.na(PH_CAT))

tabla_cruzada <- table(
  pH          = datos_top3$PH_CAT,
  Clasificacion = datos_top3$CLASIFICACION_LIMPIA
)

cat("\n══ Tabla cruzada: pH × Clasificación — frecuencias absolutas ══\n")
print(tabla_cruzada)

cat("\n  Proporciones por fila (% dentro de cada categoría de pH):\n")
print(round(prop.table(tabla_cruzada, margin = 1) * 100, 1))

cat("\n  Proporciones por columna (% dentro de cada tipo de agua):\n")
print(round(prop.table(tabla_cruzada, margin = 2) * 100, 1))

# ── Tabla cruzada: PH_CAT × OLOR ─────────────────────────────
# Justificación: el H2S baja el pH — esperamos que "Ácido" tenga
# mayor proporción de olor a H2S y "Fuerte" que los otros grupos.
datos_olor <- datos %>% filter(!is.na(PH_CAT), !is.na(OLOR_LIMPIO))

tabla_ph_olor <- table(
  pH   = datos_olor$PH_CAT,
  Olor = datos_olor$OLOR_LIMPIO
)

cat("\n══ Tabla cruzada: pH × Olor — frecuencias absolutas ══\n")
print(tabla_ph_olor)

cat("\n  Proporciones por fila (% dentro de cada categoría de pH):\n")
print(round(prop.table(tabla_ph_olor, margin = 1) * 100, 1))

cat("\n  Proporciones por columna (% dentro de cada olor):\n")
print(round(prop.table(tabla_ph_olor, margin = 2) * 100, 1))

# =============================================================
# DIAGRAMA DE TALLO Y HOJAS
# =============================================================
# El diagrama de tallo y hojas muestra la distribución completa
# de los datos sin perder valores individuales.
# Se aplica a pH (escala 0–10) y temperatura (0–94°C).
# Para pH se usa escala ×10 para mostrar un decimal.

cat("\n══ Diagrama de tallo y hojas — pH (unidades enteras) ══\n")
ph_clean <- datos$PH_LABORATORIO[!is.na(datos$PH_LABORATORIO) & datos$PH_LABORATORIO > 0]
stem(ph_clean, scale = 2)

cat("\n══ Diagrama de tallo y hojas — Temperatura ══\n")
temp_clean <- datos$TEMPERATUR[!is.na(datos$TEMPERATUR) & datos$TEMPERATUR > 0]
stem(temp_clean, scale = 1)

# ========================
# HISTOGRAMAS
# ========================

# GRÁFICO 1: HISTOGRAMA DE pH
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: El pH es numérica continua → histograma.
# Queremos ver si los manantiales son mayormente ácidos, neutros o básicos.
# bins = 9 porque tenemos 320 filas → regla de Sturges: 1 + log2(320) ≈ 9

p1 <- ggplot(datos, aes(x = PH_LABORATORIO)) +
    geom_histogram(bins = 9, fill = "#402816", color = "white") +
    geom_vline(
        xintercept = mean(datos$PH_LABORATORIO, na.rm = TRUE),
        color = "#25a2d3", linetype = "dashed"
    ) +
    geom_vline(
        xintercept = median(datos$PH_LABORATORIO, na.rm = TRUE),
        color = "#f2fd4ede", linetype = "dashed"
    ) +
    labs(
        title = "Distribución del pH en manantiales colombianos",
        subtitle = "Azul = Media (6.33)  |  Amarillo = Mediana (6.51)",
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

p2 <- ggplot(datos, aes(x = TEMPERATUR)) +
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

p3 <- ggplot(datos, aes(x = CONDUCTIVIDAD)) +
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

# GRAFICO 3.1: HISTOGRAMA DE LAS CONCENTRACIONES DE SODIO
p4 <- ggplot(datos, aes(x = SODIO)) +
    geom_histogram(bins = 9, fill = "#a82339", color = "white") +
    scale_x_log10() + # escala logarítmica en el eje X
    labs(
        title = "Distribución de las concentraciones de sodio",
        subtitle = "Rojo = Media (706.78)  |  Naranja = Mediana (170.8)",
        x = "Concentración de sodio (mg/L)",
        y = "Número de manantiales"
    )

# GRAFICO 3.2: HISTOGRAMA DE LAS CONCENTRACIONES DE CALCIO
p5 <- ggplot(datos, aes(x = CALCIO)) +
    geom_histogram(bins = 9, fill = "#a82339", color = "white") + # escala logarítmica en el eje X
    labs(
        x = "Concentración de calcio (mg/L)",
        y = "Número de manantiales"
    )

# Juntar los gráficos de histograma de pH, Temperatura, Conductividad, Sodio y Calcio
(p1 | p2) / (p3 | p4 | p5)


# GRÁFICO 4: DISTRIBUCIÓN ACUMULADA (ECDF) — pH
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: El ECDF para pH nos permite afirmar cosas concretas.
# Por ejemplo: "el X% de los manantiales tiene pH menor a 7 (son ácidos)".
# Es el gráfico más adecuado para hacer afirmaciones de este tipo.
p6 <- ggplot(datos, aes(x = PH_LABORATORIO)) +
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

p7 <- ggplot(datos, aes(x = TEMPERATUR)) +
    stat_ecdf(color = "tomato", linewidth = 1) +
    geom_vline(xintercept = 50, color = "gray40", linetype = "dashed") +
    labs(
        title = "Distribución acumulada de temperatura",
        subtitle = "Línea gris = 50°C (umbral agua termal de alta temperatura)",
        x = "Temperatura (°C)",
        y = "Proporción acumulada de manantiales"
    )

# Juntar los gráficos de ECDF de pH y Temperatura
(p6 | p7)

# ==============================================
# DIAGRAMAS DE BARRAS (1 variable)
# ==============================================
# Usamos las columnas LIMPIAS para que las variantes normalizadas se agrupen.

# Clasificacion del agua
ggplot(datos %>% filter(!is.na(CLASIFICACION_LIMPIA)), aes(x = CLASIFICACION_LIMPIA, fill =  CLASIFICACION_LIMPIA)) +
  geom_bar() +
  guides(fill = "none") +
  coord_flip() +
  labs(
    title = "Clasificacion del agua segun la cantidad de manantiales",
    x = "Clasificacion del agua",
    y = "Cantidad de manantiales"
  )

# pH categorizado — nueva variable derivada
# Justificación: PH_CAT convierte el pH continuo en tres grupos
# geoquímicamente significativos y permite comparaciones directas
# con variables categóricas como Clasificación.
ggplot(datos %>% filter(!is.na(PH_CAT)), aes(x = PH_CAT, fill = PH_CAT)) +
  geom_bar() +
  scale_fill_manual(values = c("Ácido" = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  guides(fill = "none") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 4) +
  labs(title = "Categorías de pH en manantiales colombianos",
       subtitle = "Ácido < 6.5  |  Neutro 6.5–7.5  |  Básico > 7.5  (n = 320)",
       x = "Categoría de pH", y = "Número de manantiales")

# Grafico de olor por cantidad de manantiales
ggplot (datos %>% filter(!is.na(OLOR_LIMPIO)), aes( x = OLOR_LIMPIO, fill = OLOR_LIMPIO)) + 
  geom_bar() +
  guides(fill = "none") +
  labs(
    title = "Olor del agua en la cantidad de manantiales",
    x = "Tipo de Olor",
    y = "Cantidad de manantiales"
  )

# Grafico de mediciones por anio
p10 <- ggplot(datos, aes(x = ANIO)) +
    geom_bar(fill = "#00a81e") +
    coord_flip() +
    labs(title = "Mediciones por año", x = NULL, y = "Cantidad de Manantiales")

# Grafico de temperatura categorizada
ggplot(datos %>% filter(!is.na(TEMP_CAT)), aes(x = TEMP_CAT, fill = TEMP_CAT)) +
  geom_bar() +
  scale_fill_manual(values = c("Fría"   = "#4393c3",
                               "Tibia"  = "#f4a582",
                               "Termal" = "#d6604d")) +
  guides(fill = "none") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 4) +
  labs(title = "Categorías de temperatura en manantiales colombianos",
       subtitle = "Fría <20°C  |  Tibia 20–50°C  |  Termal >50°C  (n = 320)",
       x = "Categoría de temperatura", y = "Número de manantiales")



# =============================================================
# DIAGRAMAS DE BARRAS — 2 VARIABLES
# =============================================================

#Diagrama de pH vs Temperatura
datos %>%
  filter(!is.na(PH_CAT), !is.na(TEMP_CAT)) %>%
  ggplot(aes(x = TEMP_CAT, fill = PH_CAT)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Ácido"  = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  labs(title = "Categoría de pH según temperatura del agua",
       subtitle = "Cada barra = 100% de los manantiales de esa temperatura",
       x = "Temperatura", y = "Proporción", fill = "pH")

# PH_CAT x CLASIFICACIÓN 
# Este gráfico responde: ¿el tipo de agua está asociado con la acidez?
# Solo top 3 clasificaciones para legibilidad.
datos %>%
  filter(CLASIFICACION_LIMPIA %in% top3, !is.na(PH_CAT)) %>%
  ggplot(aes(x = CLASIFICACION_LIMPIA, fill = PH_CAT)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Ácido"  = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  labs(title = "Categoría de pH según tipo de agua",
       subtitle = "Cada barra = 100% de los manantiales de ese tipo",
       x = NULL, y = "Proporción", fill = "Categoría pH")
# RESULTADO ESPERADO: Sulfatada debería tener mayor proporción de
# Ácido porque sus aguas provienen de oxidación de sulfuros.
# Bicarbonatada debería mostrar más Neutro/Básico.

# PH_CAT x OLOR 
# Justificación: el H2S disuelto reduce el pH.
# Si los manantiales con olor a H2S tienen más categoría Ácido,
# se confirma la relación entre origen volcánico y acidez.
datos %>%
  filter(!is.na(PH_CAT), !is.na(OLOR_LIMPIO)) %>%
  ggplot(aes(x = OLOR_LIMPIO, fill = PH_CAT)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Ácido"  = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  coord_flip() +
  labs(title = "Categoría de pH según olor del agua",
       subtitle = "Cada barra = 100% de los manantiales con ese olor",
       x = NULL, y = "Proporción", fill = "Categoría pH")



# ── GRÁFICO DE PASTEL ──────────────────────────────────────────
# NA no es categoría real: se filtra ANTES de pasar a ggplot.

# Gráficos de pastel para CLASIFICACION_LIMPIA y OLOR_LIMPIO
par(mfrow = c(2, 1))

olor_tabla <- table(datos$OLOR_LIMPIO)
olor_pct <- round(prop.table(olor_tabla) * 100, 1)
olor_labels <- paste0(names(olor_tabla), "\n", olor_pct, "%")
pie(olor_tabla, labels = olor_labels, main = "Distribución porcentual
 del olor del agua")

clasif_tabla <- table(datos$CLASIFICACION_LIMPIA)
clasif_pct <- round(prop.table(clasif_tabla) * 100, 1)
clasif_labels <- paste0(names(clasif_tabla), "\n", clasif_pct, "%")
pie(clasif_tabla, labels = clasif_labels, main = "Proporción de tipos de agua")
