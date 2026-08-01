# =============================================================
# 10 ANÁLISIS DE DOS VARIABLES NUMÉRICAS
# =============================================================

cat("\n=============================================================\n
                  ANALISIS BIVARIADO
    \n=============================================================\n" )

# =============================================================
# MATRICES DE CORRELACION
# =============================================================
cat("\n=============================================================\n
      Generando matrices de covarianza y correlación:
      1. Variables hidroquímicas y su conductividad (Vista General)
    \n=============================================================\n" )

cat(" === Vista matriz de correlación general ===\n")

vars_sel <- datos %>%
  filter(TEMPERATUR > 0) %>%
  select(SODIO, CALCIO, CONDUCTIVIDAD) %>%
  na.omit()

# === Matriz de covarianza

cov_mat <- cov(vars_sel)
cat("\nMatriz de covarianza:\n")
print(round(cov_mat, 2))

# === Matriz de correlacion

cor_mat <- cor(vars_sel)
cat("\nMatriz de correlación:\n")
print(round(cor_mat, 3))

# Grafico de covarianza

print(" \n===Generando gráfico de matriz de covarianza ===")
matriz_cov <- ggcorrplot(
  cov_mat,
  method   = "square",
  type     = "full",        # <- full para ver toda la matriz
  lab      = TRUE,
  lab_size = 4,
  colors   = c("#d73027", "white", "#1a9850"),
  title    = "Matriz de covarianza — Sodio, Calcio y Conductividad",
  ggtheme  = theme_minimal()
)

print(matriz_cov)
print(" \n===Generando gráfico de la correlación de Pearson ===")
# Grafico de correlacion de Pearson
graf_corr <- ggcorrplot(
  cor_mat,
  method   = "square",      # <- square en lugar de circle
  type     = "full",
  lab      = TRUE,
  lab_size = 5,
  colors   = c("#d73027", "white", "#1a9850"),
  title    = "Matriz de correlación — Sodio, Calcio y Conductividad",
  ggtheme  = theme_minimal()
)

print(graf_corr)

# =============================================================
# DIAGRAMAS DE DISPERSIÓN
# =============================================================
# 10.1 Diagrama de dispersión para ver la correlación entre el sodio
# y la conductividad

print(" \n===Generando gráfico de la correlación de Pearson
      entre el sodio y la conductividad ===")

disp1 <- datos %>% 
  filter ( !is.na(SODIO),
           !is.na(CONDUCTIVIDAD)
  ) %>%
  ggplot ( aes (x = SODIO, y = CONDUCTIVIDAD)
  ) +
  geom_point( size = 1
  ) +
  geom_smooth(method = "lm", 
              se = FALSE,
              color = "red") +
  labs( title = "Diagrama de dispersión del Sodio y la Conductividad",
        subtitle = "Correlación entre variables con el coeficiente lineal de Pearson",
        x = "Sodio",
        y = "Conductividad"
  )

print(disp1)

# 10.2 Diagrama de dispersión para ver la correlación entre el calcio
# y la conductividad

print(" \n===Generando diagrama de dispersión entre el Calcio y la conducvidad ===")
disp2 <- datos %>% 
  filter ( !is.na(CALCIO),
           !is.na(CONDUCTIVIDAD)
  ) %>%
  ggplot ( aes (x = CALCIO, y = CONDUCTIVIDAD)
  ) +
  geom_point( size = 1
  ) +
  geom_smooth(method = "lm", 
              se = FALSE,
              color = "blue") +
  labs( title = "Diagrama de dispersion del Calcio y la Conductividad",
        subtitle = "Correlacion entre variables con el coeficiente lineal de Pearson",
        x = "Calcio",
        y = "Conductividad"
  )

print(disp2)

# MATRIZ DIAGRAMAS DE DISPERSIÓN
cat("\n=============================================================\n
      Generando matriz de diagramas de dispersión (escala log)
    \n=============================================================\n" )

matriz_diag <- datos %>%
  filter(TEMPERATUR > 0) %>%
  select(SODIO, CALCIO, CONDUCTIVIDAD) %>%
  na.omit() %>%
  # Transformación log para manejar los outliers extremos
  # log1p usa log(x+1) para evitar log(0) = -Inf
  mutate(
    SODIO         = log1p(SODIO),
    CALCIO        = log1p(CALCIO),
    CONDUCTIVIDAD = log1p(CONDUCTIVIDAD)
  ) %>%
  ggpairs(
    upper = list(
      continuous = wrap("cor", method = "spearman", size = 4)
    ),
    lower = list(
      continuous = wrap("smooth", method = "lm",
                        color = "#2c3e8c", alpha = 0.3,
                        se = FALSE)
    ),
    diag = list(
      continuous = wrap("barDiag", fill = "#2c3e8c",
                        color = "white", bins = 9)
    ),
    columnLabels = c("Sodio log(mg/L)",
                     "Calcio log(mg/L)",
                     "Conductividad log(µS/cm)")
  ) +
  labs(title = "Matriz de dispersión — Sodio, Calcio y Conductividad (escala log)") +
  theme_minimal()

print(matriz_diag)


# =============================================================
# DIAGRAMAS DE DISPERSIÓN PARA 3 VARIABLES
# =============================================================
# 11. Gráfico de dispersión para Sodio, Calcio y Conductividad
# Se identifican tres grupos geoquímicos:
# 1. Sistema geotérmico Paipa-Iza (Boyacá) - aguas sulfatadas sódicas de origen magmático
# 2. Salmuera extrema (ID 1283) — conductividad de 278,000 µS/cm
# 3. Resto de manantiales
datos_3d <- datos %>%
  filter(
    !is.na(SODIO), !is.na(CALCIO), !is.na(CONDUCTIVIDAD),
    SODIO > 0, CALCIO > 0, CONDUCTIVIDAD > 0
  ) %>%
  mutate(sistema = case_when(
    LATITUD >= 5.7 & LATITUD <= 5.8 &
      LONGITUD >= -73.2 & LONGITUD <= -73.0 ~ "Sistema Paipa-Iza",
    ID_MANANTIAL == 1283                  ~ "Salmuera extrema",
    TRUE                                  ~ "Resto de manantiales"
  ))

print(datos_3d)

#Def. paleta de colores
colores <- c(
  "Sistema Paipa-Iza"    = "#e74c3c", # Rojo
  "Salmuera extrema"     = "#e67e22", # Naranja
  "Resto de manantiales" = "#402816"  # Marrón oscuro
)

# Calcular n_3d para el título dinámico
n_3d <- nrow(datos_3d)

g3d_plot <- scatterplot3d(
  x = log10(datos_3d$SODIO),
  y = log10(datos_3d$CALCIO),
  z = log10(datos_3d$CONDUCTIVIDAD),
  color = colores[datos_3d$sistema],
  pch = ifelse(datos_3d$sistema == "Resto de manantiales", 16, 17),
  xlab = "log10(Sodio)",
  ylab = "log10(Calcio)",
  zlab = "log10(Conductividad)",
  main = paste0("Diagrama 3D: Sodio vs Calcio vs Conductividad | n = ", n_3d)
)

legend("topleft",
       legend = names(colores),
       col    = colores,
       pch    = c(17, 17, 16),
       bty    = "n")

#============================
# Correlación de Spearman y Kendall
#============================

cat("\n=============================================================\n
      Generando diagramas sobre la correlación de Spearman y Kendall
    \n=============================================================\n" )


datos_cor <- datos %>%
  select(PH_LABORATORIO, TEMPERATUR, CONDUCTIVIDAD, SODIO, CALCIO)

datos_cor_completos <- na.omit(datos_cor)
n_cor <- nrow(datos_cor_completos)

cor_spearman <- cor(datos_cor_completos, method = "spearman")
cor_kendall  <- cor(datos_cor_completos, method = "kendall")

# Spearman
g_spearman <- ggcorrplot(cor_spearman,
                         method    = "square",
                         type      = "full",
                         lab       = TRUE,
                         lab_size  = 3.5,
                         colors    = c("#e74c3c", "white", "#2c3e8c"),
                         title     = paste0("Correlación de Spearman  |  n = ", n_cor),
                         p.mat     = cor_pmat(datos_cor_completos, method = "spearman"),  # p-valores
                         sig.level = 0.05,       # umbral de significancia
                         insig     = "pch",    # deja en blanco las no significativas
                         ggtheme   = theme_minimal())
print(g_spearman)

# Kendall 
g_kendall <- ggcorrplot(cor_kendall,
                        method    = "square",
                        type      = "full",
                        lab       = TRUE,
                        lab_size  = 3.5,
                        colors    = c("#e74c3c", "white", "#2c3e8c"),
                        title     = paste0("Correlación de Kendall  |  n = ", n_cor),
                        p.mat     = cor_pmat(datos_cor_completos, method = "kendall"),
                        sig.level = 0.05,
                        insig     = "pch",
                        ggtheme   = theme_minimal())
print(g_kendall)