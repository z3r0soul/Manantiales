# =============================================================
# INTEGRIDAD DE DATOS
# =============================================================
# Problema: cada variable tiene un n diferente porque:
#   1. Algunos valores originalmente eran -999 → convertidos a NA
#      en la carga con na = c("", "NA", "-999")
#   2. Los 0 de pH y temperatura se convirtieron a NA en limpieza.R
#      con na_if(), ya que 0°C y pH=0 son errores de registro
#
# Esto significa que al calcular estadísticas con na.rm = TRUE,
# cada variable usa un n distinto sin que quede explícito.
# Este script lo hace visible y lo reporta de forma ordenada.
# =============================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("REPORTE DE INTEGRIDAD DE DATOS\n")
cat("Total de filas en el dataset: ", nrow(datos), "\n")
cat("══════════════════════════════════════════════════════════════\n")

# =============================================================
# SECCIÓN 1: N válido por cada variable del análisis
# =============================================================
# Se construye una tabla con: n válido, n NA, % de completitud
# para todas las variables que se usan en el análisis

variables_analisis <- list(
  # Variables numéricas principales
  "PH_LABORATORIO" = datos$PH_LABORATORIO,
  "TEMPERATUR"     = datos$TEMPERATUR,
  "CONDUCTIVIDAD"  = datos$CONDUCTIVIDAD,
  "SODIO"          = datos$SODIO,
  "CALCIO"         = datos$CALCIO,
  # Variables categóricas
  "CLASIFICACION_LIMPIA" = datos$CLASIFICACION_LIMPIA,
  "OLOR_LIMPIO"          = datos$OLOR_LIMPIO,
  # Variables derivadas
  "PH_CAT"   = datos$PH_CAT,
  "TEMP_CAT" = datos$TEMP_CAT
)

reporte_integridad <- data.frame(
  Variable      = names(variables_analisis),
  N_total       = nrow(datos),
  N_valido      = sapply(variables_analisis, function(x) sum(!is.na(x))),
  N_NA          = sapply(variables_analisis, function(x) sum(is.na(x))),
  Pct_completo  = sapply(variables_analisis,
                         function(x) round(sum(!is.na(x)) / nrow(datos) * 100, 1)),
  row.names     = NULL
)

print(reporte_integridad)

# =============================================================
# SECCIÓN 2: De dónde vienen los NAs de pH y temperatura
# =============================================================
# Es importante distinguir entre:
#   - NAs originales: vinieron como -999 en el CSV
#   - NAs inducidos: eran 0 y los convertimos en limpieza.R

# Para pH
n_na_ph_total    <- sum(is.na(datos$PH_LABORATORIO))

# Recargar temporalmente sin na_if para contar los 0 originales
# Nota: no se modifica datos, solo se cuenta
ph_original <- read_csv2(
  here("data", "raw", "Manantial_limpio.csv"),
  locale = locale(encoding = "UTF-8"),
  na = c("", "NA", "-999")
)$PH_LABORATORIO

n_na_ph_originales <- sum(is.na(ph_original))
n_ceros_ph         <- sum(ph_original == 0, na.rm = TRUE)

cat("\n── Origen de los NAs en PH_LABORATORIO ──\n")
cat("  NAs por -999 en el CSV original:    ", n_na_ph_originales, "\n")
cat("  Ceros convertidos a NA en limpieza: ", n_ceros_ph, "\n")
cat("  Total NAs en el análisis:           ", n_na_ph_total, "\n")
cat("  N válido para análisis de pH:       ",
    nrow(datos) - n_na_ph_total, "\n")

# Para temperatura
temp_original      <- read_csv2(
  here("data", "raw", "Manantial_limpio.csv"),
  locale = locale(encoding = "UTF-8"),
  na = c("", "NA", "-999")
)$TEMPERATUR

n_na_temp_originales <- sum(is.na(temp_original))
n_ceros_temp         <- sum(temp_original == 0, na.rm = TRUE)
n_na_temp_total      <- sum(is.na(datos$TEMPERATUR))

cat("\n── Origen de los NAs en TEMPERATUR ──\n")
cat("  NAs por -999 en el CSV original:    ", n_na_temp_originales, "\n")
cat("  Ceros convertidos a NA en limpieza: ", n_ceros_temp, "\n")
cat("  Total NAs en el análisis:           ", n_na_temp_total, "\n")
cat("  N válido para análisis de temp.:    ",
    nrow(datos) - n_na_temp_total, "\n")

# =============================================================
# SECCIÓN 3: N efectivo en cada análisis bivariado
# =============================================================
# Las tablas cruzadas y los boxplots usan subconjuntos filtrados.
# Aquí se reporta el n real de cada combinación.

cat("\n── N efectivo en análisis bivariados ──\n")

cat("\n  PH_CAT × TEMP_CAT:\n")
cat("  n =", sum(!is.na(datos$PH_CAT) & !is.na(datos$TEMP_CAT)), "\n")

cat("\n  PH_CAT × CLASIFICACION_LIMPIA (top 3):\n")
cat("  n =", nrow(datos_top3), "\n")

cat("\n  PH_CAT × OLOR_LIMPIO:\n")
n_ph_olor <- datos %>%
  filter(!is.na(PH_CAT), !is.na(OLOR_LIMPIO)) %>%
  nrow()
cat("  n =", n_ph_olor, "\n")

cat("\n  Boxplot pH (datos_top3 sin NA):\n")
n_box_ph <- datos_top3 %>%
  filter(!is.na(PH_LABORATORIO)) %>%
  nrow()
cat("  n =", n_box_ph, "\n")

cat("\n  Boxplot Temperatura (datos_top3 sin NA):\n")
n_box_temp <- datos_top3 %>%
  filter(!is.na(TEMPERATUR)) %>%
  nrow()
cat("  n =", n_box_temp, "\n")

cat("\n  Dispersión Sodio vs Conductividad:\n")
n_disp <- datos %>%
  filter(!is.na(SODIO), !is.na(CONDUCTIVIDAD)) %>%
  nrow()
cat("  n =", n_disp, "\n")

# =============================================================
# SECCIÓN 4: N por grupo en los boxplots
# =============================================================
# Esto es especialmente importante porque si un grupo tiene
# muy pocos datos, el boxplot no es confiable para ese grupo.

cat("\n── N por tipo de agua en los boxplots ──\n")

n_por_tipo <- datos_top3 %>%
  group_by(CLASIFICACION_LIMPIA) %>%
  summarise(
    N_pH   = sum(!is.na(PH_LABORATORIO)),
    N_Temp = sum(!is.na(TEMPERATUR)),
    .groups = "drop"
  )

print(n_por_tipo)