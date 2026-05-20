# =============================================================
# 9. DIAGRAMAS DE CAJA Y BIGOTES
# =============================================================

# 9.1 Box plot provisional para practicar
# Variable: pH del agua   
box1 <- datos %>%
  filter(!is.na(CLASIFICACION_LIMPIA),
         !is.na(PH_LABORATORIO)
  ) %>%
  ggplot(aes(x    = CLASIFICACION_LIMPIA,
             y    = PH_LABORATORIO,
             fill = CLASIFICACION_LIMPIA)) +
  geom_boxplot() +
  guides(fill = "none") +
  labs(
    title = "Distribución del pH por tipo de agua",
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

box2 <- datos %>%
  filter(!is.na(CLASIFICACION_LIMPIA),
         !is.na(TEMPERATUR),
         TEMPERATUR > 0,
         CLASIFICACION_LIMPIA %in% top3
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