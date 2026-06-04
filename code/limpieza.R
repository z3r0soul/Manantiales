# =============================================================
# ARCHIVO ENCARGADO DEL MANEJO DE NA,
# FACTORES, RENOMBRAMIENTOS, FILTRADO, TIPOS DE VARIABLES
# DENTRO DE LOS DATOS
# =============================================================

# SECCIÓN 1: Creación de columnas normalizadas y manejo de tipo especial de variable (Fecha)

datos <- datos %>%
  mutate(
    # 1. Extracción del AÑO y transformación de la columna FECHA
    # Aquí extraemos el AÑO de la columna FECHA y lo guardamos en una nueva columna llamada ANIO.
    # La columna FECHA se convierte a formato de fecha y se extrae el año.
    # as.Date(FECHA, format = "%m/%d/%Y") convierte la columna FECHA a formato de fecha
    # year(FECHA) extrae el año de la columna FECHA
    # as.character() convierte el año a texto (Categorización de ANIO)
    
    FECHA = as.Date(FECHA, format = "%m/%d/%Y"),
    ANIO  = as.character(year(FECHA)),
    
    # 2. Con tidyverse, mutate se utiliza para crear una nueva columna a partir de otra ya existente.
    # En este caso, se crea la columna CLASIFICACION_LIMPIA a partir de la columna CLASIFICACIÓN y
    # la columna OLOR_LIMPIO a partir de la columna OLOR.
    
    CLASIFICACION_LIMPIA = normalizar(CLASIFICACIÓN),
    OLOR_LIMPIO          = normalizar(OLOR),
    
    # 3. Se tratan como NA a un conjunto de datos atípicos,
    # posiblemente estos mismos son resultado de mediciones erróneas
    # 3.1 No es probable que un manantial llegue a una temperatura de 0 grados
    # por las condiciones geotérmicas del territorio colombiano
    # 3.2 Los pH totalmente ácidos en aguas de manantiales es casi que imposible, 
    # pues estas llevan años pasando por piedras que favorecen la mineralización
    # así como las piedras calizas
    
      PH_LABORATORIO = na_if(PH_LABORATORIO, 0),
      TEMPERATUR     = na_if(TEMPERATUR, 0),
    
    # 4. Binario: Con olor vs Sin olor (excluye NA y vacíos)
    OLOR_BIN = case_when(
      is.na(OLOR_LIMPIO) | OLOR_LIMPIO == "" ~ NA_character_,
      OLOR_LIMPIO == "Ausente"               ~ "Sin olor",
      TRUE                                   ~ "Con olor"
    ),
    OLOR_BIN = factor(OLOR_BIN, levels = c("Sin olor", "Con olor")),
    
    # 5. Categorizar pH en tres grupos con significado geoquímico:
    # Ácido < 6.5 | Neutro 6.5–7.5 | Básico > 7.5
    # Justificación: el punto de corte 6.5/7.5 es el estándar OMS
    # para calidad del agua
    
    PH_CAT = case_when(
      PH_LABORATORIO < 6.5  ~ "Ácido",
      PH_LABORATORIO <= 7.5 ~ "Neutro",
      PH_LABORATORIO > 7.5  ~ "Básico",
      TRUE ~ NA_character_
    ),
    PH_CAT = factor(PH_CAT, levels = c("Ácido", "Neutro", "Básico")),
    
    # 6. Temperatura categorizada
    # Al igual que PH_CAT, convertir temperatura continua
    # en categorías permite análisis bivariado y comunica mejor
    # el significado geotérmico que un histograma solo.
    # Umbrales: fría <20°C (agua superficial), tibia 20–50°C
    # (geotérmica baja), termal >50°C (geotérmica alta / fumarola).
    
    TEMP_CAT = case_when(
      TEMPERATUR < 20  ~ NA_character_,
      TEMPERATUR <= 50 ~ "Tibia",
      TEMPERATUR > 50  ~ "Termal",
      TRUE ~ NA_character_
    ),
    TEMP_CAT = factor(TEMP_CAT, levels = c("Fría", "Tibia", "Termal"))
  )

# SECCION 2: Filtrado de variables (Exclusión de valores NA e inválidos)


# 2.1 Creamos un único dataset auxiliar libre de NAs para cruces de variables
datos_analisis <- datos %>% 
  filter(!is.na(PH_CAT), !is.na(TEMP_CAT), 
         !is.na(OLOR_LIMPIO), !is.na(CLASIFICACION_LIMPIA),
         # Se filtra este pH en específico porque implica un análisis diferente
         # Se podria decir que se puede clasificar como un grupo diferente
         PH_LABORATORIO >= 4.0)

# 2.2 Filtrado de datos más significativos para la Clasificación del agua

# Creación de vector con las clasificaciones con mayor cantidad de datos
top3 <- c("Bicarbonatada", "Clorurada", "Sulfatada")

datos_top3 <- datos_analisis %>%
  filter(CLASIFICACION_LIMPIA %in% top3)
