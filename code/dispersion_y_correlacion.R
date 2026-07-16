# =============================================================
# 10 ANÁLISIS DE DOS VARIABLES NUMÉRICAS
# =============================================================

cat("\n=============================================================\n
                  ANALISIS BIVARIADO
    \n=============================================================\n" )

# =============================================================
# MATRICES DE COVARIANZA
# =============================================================
cat("\n=============================================================\n
      Generando matriz de covarianza:
      1. Variables hidroquímicas y su conductividad
    \n=============================================================\n" )
matriz_cov <- datos %>%
  select(
    SODIO,
    CALCIO,
    CONDUCTIVIDAD
  ) %>%
  cov(
    use = "complete.obs"
    )

print (matriz_cov)
# =============================================================
# MATRICES DE CORRELACION
# =============================================================
cat("\n=============================================================\n
      Generando matrices de correlación:
      1. Variables hidroquímicas y su conductividad (Vista General)
      2. Variables hidroquimicas segun tipo principal de agua
    \n=============================================================\n" )

cat(" === Vista matriz de correlación general ===\n")

variables <- datos %>%
  select(SODIO,
         CALCIO,
         CONDUCTIVIDAD)

cor_general <- cor(variables,
    use = "complete.obs",
    method = "pearson"
)
print(cor_general)

# Correlación de la Conductividad con el Sodio y el Calcio
# A partir de la clasficación principal del agua

datos_top3 %>%
  
  group_by(CLASIFICACION_LIMPIA) %>%
  
  summarise(
    
    correlacion_s = cor(
      CONDUCTIVIDAD,
      SODIO,
      use = "complete.obs",
      method = "pearson"
    ),
    correlacion_c = cor(
        CONDUCTIVIDAD,
        CALCIO,
        use = "complete.obs",
        method = "pearson"
      ),
    n = n()
  )


# =============================================================
# DIAGRAMAS DE DISPERSIÓN
# =============================================================
# 10.1 Diagrama de dispersión para ver la correlación entre el sodio
# y la conductividad

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

guardar_plot(
  disp1,
  "dispersion_sodio_vs_conductividad",
  "graficos_dispersion",
  tipo = "ggplot"
)
# 10.2 Diagrama de dispersión para ver la correlación entre el calcio
# y la conductividad

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

guardar_plot(
  disp2,
  "dispersion_calcio_vs_conductividad",
  "graficos_dispersion",
  tipo = "ggplot"
)

# MATRIZ DIAGRAMAS DE DISPERSIÓN
cat("\n=============================================================\n
      Generando matriz de diagramas de dispersión
    \n=============================================================\n" )

matriz_diag <- datos %>%
  select(
    SODIO,
    CALCIO,
    CONDUCTIVIDAD
  ) %>%
  ggpairs(
    upper = list(
      continuous = "points"
    ),
    lower = list(
      continuous = "points"
    ),
    diag = list(
      continuous = "densityDiag"
    )
  )

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

1

legend("topleft",
       legend = names(colores),
       col    = colores,
       pch    = c(17, 17, 16),
       bty    = "n")
guardar_plot(g3d_plot, "scatterplot3d_sodio_calcio_conductividad", tipo = "base")

#============================
# Correlación de Spearman y Kendall
#============================
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
guardar_plot(g_spearman, "correlacion_spearman", tipo = "ggplot")

# Kendall 
g_kendall <- ggcorrplot(cor_kendall,
                        method    = "square",
                        type      = "full",
                        lab       = TRUE,
                        lab_size  = 3.5,
                        colors    = c("#e74c3c", "white", "#2c3e8c"),
                        title     = paste0("Correlación de Kendall  |  n = ", n_cor),
                        p.mat     = cor_pmat(datos_cor_completos, method = "spearman"),
                        sig.level = 0.05,
                        insig     = "pch",
                        ggtheme   = theme_minimal())
print(g_kendall)
guardar_plot(g_kendall, "correlacion_kendall", tipo = "ggplot")