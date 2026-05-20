# =============================================================
# ARCHIVO ENCARGADO DEL MANEJO DE NA,
# FACTORES, RENOMBRAMIENTOS, FILTRADO, TIPOS DE VARIABLES
# DENTRO DE LOS DATOS
# =============================================================

# SECCIÓN 1: Creación de columnas normalizadas y manejo de tipo especial de variable (Fecha)

# Extracción del AÑO y transformación de la columna FECHA
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

# Con tidyverse, mutate se utiliza para crear una nueva columna a partir de otra ya existente.
# En este caso, se crea la columna CLASIFICACION_LIMPIA a partir de la columna CLASIFICACIÓN y
# la columna OLOR_LIMPIO a partir de la columna OLOR.

datos <- datos %>%
  mutate(
    CLASIFICACION_LIMPIA = normalizar(CLASIFICACIÓN),
    OLOR_LIMPIO          = normalizar(OLOR)
  )

# Categorizar pH en tres grupos con significado geoquímico:
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

# Temperatura categorizada
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

# SECCION 2: Filtrado de variables (Exclusión de valores NA e inválidos)

# 1. Para el diagrama de hoja y tallo se requiere
# limpiar los datos del pH y la Temperatura, entonces: 

# Limpieza del pH:

ph_limpio <- datos$PH_LABORATORIO[
  !is.na(datos$PH_LABORATORIO) & datos$PH_LABORATORIO > 0
]

# Limpieza de la Temperatura:

temp_limpia <- datos$TEMPERATUR[
  !is.na(datos$TEMPERATUR) & datos$TEMPERATUR > 0
]

# 2. Limpiar los datos para las tablas de frecuencias univariadas

# Limpieza de los datos de la Clasificación del agua 
# Se utiliza CLASIFICACION_LIMPIA porque está normalizada

datos_clasif <- datos %>% filter(!is.na(CLASIFICACION_LIMPIA))

# Limpieza de los datos del Olor percibido en el agua de los manantiales 

datos_olor <- datos %>% filter(!is.na(OLOR_LIMPIO))

# Limpieza de los datos de pH
# Utilizamos los datos que ya categorizamos (PH_CAT)

datos_ph <- datos %>% filter(!is.na(PH_CAT))

# Limpieza de los datos de Temperatura
# Utilizamos los datos que ya categorizamos (TEMP_CAT)

datos_temp <- datos %>% filter(!is.na(TEMP_CAT))

# 3. Limpiar los datos para las tablas de frecuencias bivariadas

# Limpieza de datos pH vs Temperatura

datos_pt <- datos %>% filter(!is.na(PH_CAT), 
                             !is.na(TEMP_CAT)
)

# Filtrado de datos más significativos para la Clasificación del agua

# Creación de vector con las clasificaciones con mayor cantidad de datos
top3 <- c("Bicarbonatada", "Clorurada", "Sulfatada")

datos_top3 <- datos %>%
  filter(CLASIFICACION_LIMPIA %in% top3, !is.na(PH_CAT))

# Filtrado de datos para la tabla pH vs Olor

datos_olor <- datos %>% filter(!is.na(PH_CAT),
                               !is.na(OLOR_LIMPIO))
