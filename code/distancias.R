# ========================
# ANÁLISIS DE AGRUPAMIENTO
# ========================
  # Preparar datos
  datos_clust <- datos %>%
    filter(
      !is.na(PH_LABORATORIO),
      !is.na(TEMPERATUR),
      !is.na(CONDUCTIVIDAD)
    ) %>%
    select(ID_MANANTIAL, PH_LABORATORIO, TEMPERATUR, CONDUCTIVIDAD)
  
  n_clust <- nrow(datos_clust)
  cat("Manantiales disponibles para agrupamiento:", n_clust, "\n")
  
  # Estandarizar (media 0, desviación 1) para que las escalas no dominen
  ids <- datos_clust$ID_MANANTIAL
  mat <- datos_clust %>% 
    select(-ID_MANANTIAL) %>%
    scale()
  rownames(mat) <- ids
  
  # Paso 2: Matrices de distancia 
  dist_manhattan <- dist(mat, method = "manhattan")
  dist_canberra  <- dist(mat, method = "canberra")
  
  # Paso 3: Agrupamiento jerárquico (enlace completo)
  hc_manhattan <- hclust(dist_manhattan, method = "complete")
  hc_canberra  <- hclust(dist_canberra,  method = "complete")
  
  # Paso 4: Dendrogramas para decidir número de grupos
  par(mfrow = c(1, 2))
  
  plot(hc_manhattan,
       main   = paste0("Manhattan  |  n = ", n_clust),
       xlab   = "",
       ylab   = "Distancia",
       labels = FALSE,
       hang   = -1,
       cex.main = 0.9)
  
  plot(hc_canberra,
       main   = paste0("Canberra  |  n = ", n_clust),
       xlab   = "",
       ylab   = "Distancia",
       labels = FALSE,
       hang   = -1,
       cex.main = 0.9)
  
  par(mfrow = c(1, 1))
  
#======================
#
#======================
  # Asignar grupos (k)
  k <- 3  # <-- ajusta este valor según el dendrograma
  
  datos_clust <- datos_clust %>%
    mutate(
      grupo_manhattan = factor(cutree(hc_manhattan, k = k)),
      grupo_canberra  = factor(cutree(hc_canberra,  k = k))
    )
  
  #  Perfil de cada grupo 
  cat("\n── Perfil por grupo (Manhattan) ──\n")
  datos_clust %>%
    group_by(grupo_manhattan) %>%
    summarise(
      n             = n(),
      pH_mediano    = median(PH_LABORATORIO, na.rm = TRUE),
      temp_mediana  = median(TEMPERATUR,     na.rm = TRUE),
      cond_mediana  = median(CONDUCTIVIDAD,  na.rm = TRUE)
    ) %>%
    print()
  
  #  Diagrama ilustrativo de grupos (scatterplot)
  # Usamos temperatura vs conductividad (log) coloreado por grupo
  g_grupos <- ggplot(datos_clust,
                     aes(x = TEMPERATUR,
                         y = CONDUCTIVIDAD,
                         color = grupo_manhattan)) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_y_log10() +
    scale_color_manual(
      values = c("#402816", "#e74c3c", "#f39c12"),
      name   = "Grupo"
    ) +
    labs(
      title    = "Grupos de manantiales por temperatura y conductividad",
      subtitle = paste0("Distancia Manhattan, enlace completo  |  n = ", nrow = n_clust),
      x        = "Temperatura (°C)",
      y        = "Conductividad — log10(µS/cm)"
    )
  
  print(g_grupos)
  guardar_plot(g_grupos, "grupos_scatterplot")
  
  #Rostros de Chernoff por grupo
  # Un rostro por grupo usando el perfil mediano
  
  # 1. Calculamos los perfiles como de costumbre
  perfiles <- datos_clust %>%
    group_by(grupo_manhattan) %>%
    summarise(
      pH            = median(PH_LABORATORIO, na.rm = TRUE),
      Temperatura   = median(TEMPERATUR,     na.rm = TRUE),
      Conductividad = median(CONDUCTIVIDAD,  na.rm = TRUE)
    ) %>%
    as.data.frame() # Aseguramos que sea un data.frame puro de R base
  
  # 2. Asignamos los nombres de las filas usando R Base
  rownames(perfiles) <- perfiles$grupo_manhattan
  
  # 3. Removemos la columna para que no interfiera en el gráfico de rostros
  perfiles$grupo_manhattan <- NULL
  
  # 4. Graficamos
  faces(perfiles,
        face.type = 1,
        main      = paste0("Rostros de Chernoff por grupo  |  n = ", n_clust),
        labels    = paste("Grupo", rownames(perfiles)))
