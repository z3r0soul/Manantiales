# =============================================================
# 10 ANÁLISIS DE DOS VARIABLES NUMÉRICAS
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
variables <- datos %>%
  select(PH_LABORATORIO,
         TEMPERATUR,
         SODIO,
         CALCIO,
         CONDUCTIVIDAD)

cor(variables,
    use = "complete.obs",
    method = "pearson"
)