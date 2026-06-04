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
  "graficos_dispersion"
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
  "graficos_dispersion"
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