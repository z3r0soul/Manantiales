# =============================================================
# 10 ANÁLISIS DE DOS VARIABLES NUMÉRICAS
# =============================================================

cat("\n=============================================================\n
    Generando diagramas de dispersión y matrices de 
    correlación, covarianza y diagramas de dispersión
    \n=============================================================\n" )
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
  labs( title = "Diagrama de dispersion del Sodio y la Conductividad",
        subtitle = "Correlacion entre variables con el coeficiente lineal de Pearson",
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

# Se utiliza la funcion cor() con el metodo de pearson
# Con esto conseguimos el valor de 0.32, lo que da una correlacion debil
datos_filtrados <- datos %>% 
  filter(
    !is.na(SODIO),
    !is.na(CONDUCTIVIDAD)
  )
cor(
  datos_filtrados$SODIO,
  datos_filtrados$CONDUCTIVIDAD,
  method = "pearson"
)

# =============================================================
# MATRICES DE CORRELACION
# =============================================================
cat("\n=============================================================\n
      Generado matrices de correlación:
      1. Vista general a todas las variables
      2. Variables hidroquímicas

    \n=============================================================\n" )
cat(" === Vista matriz de correlación general ===\n")
variables <- datos %>%
  select(PH_LABORATORIO,
         SODIO,
         CALCIO,
         CONDUCTIVIDAD)

cor_general <- cor(variables,
    use = "complete.obs",
    method = "pearson"
)
print(cor_general)

cat("\n=== Vista matriz de correlación propiedades hidroquímicas ===\n")
variables2 <- datos %>%
  select(
    SODIO,
    CALCIO,
    CONDUCTIVIDAD
  )

cor(variables2, 
    method = "pearson"
    )

