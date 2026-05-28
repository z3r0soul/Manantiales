# =============================================================
# 9. DIAGRAMAS DE CAJA Y BIGOTES
# =============================================================

cat("\n=============================================================\n
    Generando percentiles y boxplots...
    \n=============================================================\n" )

# 9.1
# Variable: pH del agua   
# Hallar percentiles

cat("\n === Resumen del diagrama de caja de Clasificación vs pH de laboratorio ===\n")
boxplot1_summarise <- datos_top3 %>%
  
  filter(
    !is.na(CLASIFICACION_LIMPIA),
    !is.na(PH_LABORATORIO)
  ) %>%
  
  group_by(CLASIFICACION_LIMPIA) %>%
  
  summarise(
    
    Q1 = quantile(PH_LABORATORIO, 0.25),
    
    Mediana = quantile(PH_LABORATORIO, 0.50),
    
    Q3 = quantile(PH_LABORATORIO, 0.75),
    
    RIQ = IQR(PH_LABORATORIO)
    
    )

# Impresión de los percentiles del boxplot 1 
print(boxplot1_summarise)

# Creación del boxplot1  
box1 <- datos_top3 %>%
  filter(!is.na(CLASIFICACION_LIMPIA),
         !is.na(PH_LABORATORIO)
  ) %>%
  ggplot(aes(x    = CLASIFICACION_LIMPIA,
             y    = PH_LABORATORIO,
             fill = CLASIFICACION_LIMPIA)) +
  geom_boxplot() +
  guides(fill = "none") +
  labs(
    title = "Distribución del pH por tipo de agua principal",
    x     = NULL,
    y     = "pH (laboratorio)"
  ) +
  theme_minimal()

print(box1)

guardar_plot(
  box1,
  "boxplot_clasif_vs_ph",
  "graficos_boxplot"
)

# 9.2 Box Plot TEMPERATUR vs CLASIFICACION_LIMPIA
# Vemos de las temperatura mayores a 0 y que no son mixtas ni compuestas: 
# (Bicarbonatada / Clorurada / Sulfatada)
#Hallar percentiles

cat("\n === Resumen del diagrama de caja de Clasificación vs Temperatura ===\n")
boxplot2_summarise <- datos_top3 %>% 
  filter (!is.na(CLASIFICACION_LIMPIA), 
          !is.na(TEMPERATUR),
          ) %>%
  group_by(CLASIFICACION_LIMPIA) %>%
  summarise(
    
    Q1 = quantile(TEMPERATUR, 0.25),
    
    MEDIAN = quantile(TEMPERATUR, 0.50),
    
    Q3 = quantile(TEMPERATUR, 0.75),
    
    RIQ = IQR(TEMPERATUR)
    
  )

# Impresión de los percentiles del boxplot 2

print(boxplot2_summarise)

# Creación del boxplot2

box2 <- datos_top3 %>%
  filter(!is.na(CLASIFICACION_LIMPIA),
         !is.na(TEMPERATUR)
         ) %>%
  ggplot(aes(x    = CLASIFICACION_LIMPIA,
             y    = TEMPERATUR,
             fill = CLASIFICACION_LIMPIA)) +
  geom_boxplot() +
  guides(fill = "none") +
  labs(
    title = "Distribución de temperatura por tipo de agua",
    x     = NULL,
    y     = "Temperatura (°C)"
  ) +
  theme_minimal()

print(box2)

guardar_plot(
  box2,
  "boxplot_clasif_vs_temperatura",
  "graficos_boxplot"
)