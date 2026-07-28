# ========================
# MEDIDAS DE SIMILARIDAD
# ========================
top3 <- c("Bicarbonatada", "Clorurada", "Sulfatada")

df_sim <- datos %>%
  filter(
    CLASIFICACION_LIMPIA %in% top3,
    !is.na(CLASIFICACION_LIMPIA),
    !is.na(OLOR_BIN),
    !is.na(PH_CAT),
    !is.na(TEMP_CAT)
  ) %>%
  mutate(
    # NOMINAL: sin orden entre categorías
    CLASIFICACION_LIMPIA = factor(CLASIFICACION_LIMPIA),
    OLOR_BIN             = factor(OLOR_BIN),
    
    # ORDINAL: ordered = TRUE le dice a daisy() que hay orden real
    # y usa distancia de rango en vez de 0/1
    PH_CAT   = factor(PH_CAT,
                      levels  = c("Ácido", "Neutro", "Básico"),
                      ordered = TRUE),
    TEMP_CAT = factor(TEMP_CAT,
                      levels  = c("Fría", "Tibia", "Termal"),
                      ordered = TRUE)
  ) %>%
  select(CLASIFICACION_LIMPIA, OLOR_BIN, PH_CAT, TEMP_CAT)

cat("Manantiales en el análisis:", nrow(df_sim), "\n")

# ============================================================
# DISTANCIA DE GOWER -> MATRIZ DE SIMILARIDAD
# ============================================================
dist_gower       <- daisy(df_sim, metric = "gower")
matriz_similitud <- 1 - as.matrix(dist_gower)

# ============================================================
# RESUMEN POR TIPO DE CLASIFICACIÓN
# ============================================================
# Reconstruimos las etiquetas para poder clasificar los pares
etiquetas <- datos %>%
  filter(
    CLASIFICACION_LIMPIA %in% top3,
    !is.na(CLASIFICACION_LIMPIA),
    !is.na(OLOR_BIN),
    !is.na(PH_CAT),
    !is.na(TEMP_CAT)
  ) %>%
  pull(CLASIFICACION_LIMPIA)

n     <- nrow(df_sim)
pares <- combn(1:n, 2)

# Similaridad para cada par
sim_vals   <- matriz_similitud[lower.tri(matriz_similitud)]
tipo_i     <- etiquetas[pares[1,]]
tipo_j     <- etiquetas[pares[2,]]
mismo_tipo <- tipo_i == tipo_j

# Promedio de similaridad dentro y entre tipos
cat("\n── Similaridad promedio DENTRO de cada tipo ──\n")
for (tipo in top3) {
  mask <- tipo_i == tipo & tipo_j == tipo
  cat(sprintf("  %-15s : %.3f  (n pares = %d)\n",
              tipo, mean(sim_vals[mask]), sum(mask)))
}

cat("\n── Similaridad promedio ENTRE tipos distintos ──\n")
pares_entre <- data.frame(
  tipo_i   = as.character(tipo_i),
  tipo_j   = as.character(tipo_j),
  sim      = sim_vals
) %>%
  filter(tipo_i != tipo_j) %>%
  mutate(par = paste(pmin(tipo_i, tipo_j),
                     pmax(tipo_i, tipo_j), sep = " vs ")) %>%
  group_by(par) %>%
  summarise(sim_media = round(mean(sim), 3),
            n_pares   = n(), .groups = "drop") %>%
  arrange(desc(sim_media))

print(pares_entre)

# ============================================================
# DISTRIBUCIÓN GENERAL DE SIMILARIDADES
# ============================================================
cat("\n── Resumen de la distribución de similaridades ──\n")
cat(sprintf("  Mínimo  : %.3f\n", min(sim_vals)))
cat(sprintf("  Mediana : %.3f\n", median(sim_vals)))
cat(sprintf("  Media   : %.3f\n", mean(sim_vals)))
cat(sprintf("  Máximo  : %.3f\n", max(sim_vals)))
