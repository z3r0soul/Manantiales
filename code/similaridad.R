# ========================
# MEDIDAS DE SIMILARIDAD
# ========================

top3 <- c("Sulfatada", "Clorurada", "Bicarbonatada")

# 1. Preparación y filtrado de datos
datos_sim <- datos %>%
  filter(
    !is.na(CLASIFICACION_LIMPIA),
    !is.na(OLOR_BIN),
    CLASIFICACION_LIMPIA %in% top3
  ) %>%
  mutate(
    # Definir variables como FACTORES para que Gower las trate correctamente
    CLASIFICACION_LIMPIA = factor(CLASIFICACION_LIMPIA, levels = top3),
    OLOR_BIN             = factor(OLOR_BIN) # Asume p.ej. "Con olor" / "Sin olor"
  )

n_sim <- nrow(datos_sim)
cat("n disponible:", n_sim, "\n")

# 2. Tabla de contingencia y medidas de asociación (entre variables)
tabla <- table(datos_sim$CLASIFICACION_LIMPIA, datos_sim$OLOR_BIN)
cat("\n── Tabla de contingencia ──\n")
print(addmargins(tabla))

cat("\n── Medidas de asociación (Cramér's V, Pearson, etc.) ──\n")
print(assocstats(tabla))

# =========================================================
# SIMILARIDAD DE GOWER (Entre Muestras / Manantiales)
# =========================================================

# Seleccionamos únicamente las columnas de interés
df_gower <- datos_sim %>% 
  select(CLASIFICACION_LIMPIA, OLOR_BIN)

# daisy() calcula la MATRIZ DE DISIMILARIDAD (Distancia de Gower)
# Nota: Si OLOR_BIN es un factor de 2 niveles, se trata como binario nominal.
dist_gower <- daisy(df_gower, metric = "gower")

# Convertimos la matriz de disimilaridad a MATRIZ DE SIMILARIDAD (1 - d)
matriz_similitud <- 1 - as.matrix(dist_gower)

# Extractores de pares para clasificación
pares <- combn(1:n_sim, 2)

# Vector de valores de similaridad de Gower para cada par
gower_vals <- matriz_similitud[lower.tri(matriz_similitud)]

# Clasificar los pares
mismo_tipo <- datos_sim$CLASIFICACION_LIMPIA[pares[1,]] == 
  datos_sim$CLASIFICACION_LIMPIA[pares[2,]]
mismo_olor <- datos_sim$OLOR_BIN[pares[1,]] == 
  datos_sim$OLOR_BIN[pares[2,]]

# Resultados resumidos
cat("\n── Similaridad de Gower promedio por tipo de par ──\n")
cat("  Mismo tipo Y mismo olor:  ", 
    round(mean(gower_vals[ mismo_tipo &  mismo_olor]), 3), "\n")
cat("  Mismo tipo, olor distinto:", 
    round(mean(gower_vals[ mismo_tipo & !mismo_olor]), 3), "\n")
cat("  Distinto tipo, mismo olor:", 
    round(mean(gower_vals[!mismo_tipo &  mismo_olor]), 3), "\n")
cat("  Todo distinto:            ", 
    round(mean(gower_vals[!mismo_tipo & !mismo_olor]), 3), "\n")

# Tabulación ordenada y porcentajes
resumen_gower <- data.frame(
  gower_sim = gower_vals,
  tipo_par = case_when(
    mismo_tipo &  mismo_olor ~ "Mismo tipo y olor",
    mismo_tipo & !mismo_olor ~ "Mismo tipo, olor distinto",
    !mismo_tipo &  mismo_olor ~ "Distinto tipo, mismo olor",
    TRUE                     ~ "Todo distinto"
  )
) %>%
  count(tipo_par, gower_sim) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(gower_sim))

cat("\n── Distribución de frecuencias de Similaridad de Gower ──\n")
print(resumen_gower)