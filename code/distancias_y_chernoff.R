# ========================
# ANÁLISIS DE AGRUPAMIENTO
# ========================
# Preparar dato
datos_clust <- datos %>%
  filter(
    !is.na(PH_LABORATORIO),
    !is.na(TEMPERATUR),
    !is.na(CONDUCTIVIDAD),
    !is.na(SODIO),
    !is.na(CALCIO),
    TEMPERATUR > 0,
    PH_LABORATORIO > 0
  ) %>%
  select(ID_MANANTIAL, PH_LABORATORIO, TEMPERATUR,
         CONDUCTIVIDAD, SODIO, CALCIO)

n_clust <- nrow(datos_clust)
cat("Manantiales disponibles:", n_clust, "\n")

# Estandarizar (Para poder comparar escalas z-value) 
ids <- datos_clust$ID_MANANTIAL
mat <- datos_clust %>%
  select(-ID_MANANTIAL) %>%
  scale()
rownames(mat) <- ids

# Distancias y agrupamiento
dist_manhattan <- dist(mat, method = "manhattan")
dist_canberra  <- dist(mat, method = "canberra")

hc_manhattan <- hclust(dist_manhattan, method = "complete")
hc_canberra  <- hclust(dist_canberra,  method = "complete")

# Dendrogramas
dendrogramas <- par(mfrow = c(1, 2))
plot(hc_manhattan,
     main = paste0("Manhattan  |  n = ", n_clust),
     xlab = "", ylab = "Distancia",
     labels = FALSE, hang = -1, cex.main = 0.9)
plot(hc_canberra,
     main = paste0("Canberra  |  n = ", n_clust),
     xlab = "", ylab = "Distancia",
     labels = FALSE, hang = -1, cex.main = 0.9)
par(mfrow = c(1, 1))

print(dendrogramas)

# Asignar grupos
k <- 3

datos_clust <- datos_clust %>%
  mutate(
    grupo_manhattan = factor(cutree(hc_manhattan, k = k)),
    grupo_canberra  = factor(cutree(hc_canberra,  k = k))
  )

# Perfil por grupo
cat("\nPerfil por grupo (Manhattan)\n")
datos_clust %>%
  group_by(grupo_manhattan) %>%
  summarise(
    n             = n(),
    pH_mediano    = median(PH_LABORATORIO, na.rm = TRUE),
    temp_mediana  = median(TEMPERATUR,     na.rm = TRUE),
    cond_mediana  = median(CONDUCTIVIDAD,  na.rm = TRUE),
    sodio_mediano = median(SODIO,          na.rm = TRUE),
    calcio_mediano = median(CALCIO,        na.rm = TRUE)
  ) %>%
  print()

# Scatterplot -> Sodio vs Conductividad coloreado por grupo
# Se elige este par porque tiene la correlación más alta (r=0.95)
# y separa mejor los grupos visualmente
g_grupos <- ggplot(datos_clust,
                   aes(x     = SODIO,
                       y     = CONDUCTIVIDAD,
                       color = grupo_manhattan)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(
    values = c("#402816", "#e74c3c", "#f39c12"),
    name   = "Grupo"
  ) +
  labs(
    title    = "Grupos de manantiales - Sodio vs Conductividad",
    subtitle = paste0("5 variables, distancia Manhattan, enlace completo  |  n = ", n_clust),
    x        = "Sodio — log(mg/L)",
    y        = "Conductividad — log(µS/cm)"
  ) +
  theme_minimal()

print(g_grupos)

# Rostros de Chernoff
perfiles <- datos_clust %>%
  group_by(grupo_manhattan) %>%
  summarise(
    pH            = median(PH_LABORATORIO, na.rm = TRUE),
    Temperatura   = median(TEMPERATUR,     na.rm = TRUE),
    Conductividad = median(CONDUCTIVIDAD,  na.rm = TRUE),
    Sodio         = median(SODIO,          na.rm = TRUE),
    Calcio        = median(CALCIO,         na.rm = TRUE)
  ) %>%
  as.data.frame()

rownames(perfiles)       <- paste("Grupo", perfiles$grupo_manhattan)
perfiles$grupo_manhattan <- NULL

chernoff <- faces(perfiles,
      face.type = 1,
      main      = paste0("Rostros de Chernoff  |  n = ", n_clust),
      labels    = rownames(perfiles))

print(chernoff)