# ========================
# 5. HISTOGRAMAS
# ========================

# GRÁFICO 5.1: HISTOGRAMA DE pH
# bins = 9 porque tenemos 320 filas → regla de Sturges: 1 + log2(320) ≈ 9
n_ph <- sum(!is.na(datos$PH_LABORATORIO))

h1 <- ggplot(datos %>% filter(!is.na(PH_LABORATORIO)), aes(x = PH_LABORATORIO)) +
  geom_histogram(bins = 9, fill = "#402816", color = "white") +
  geom_vline(
    xintercept = mean(datos$PH_LABORATORIO, na.rm = TRUE),
    color = "#25a2d3", linetype = "dashed"
  ) +
  geom_vline(
    xintercept = median(datos$PH_LABORATORIO, na.rm = TRUE),
    color = "#f2fd4ede", linetype = "dashed"
  ) +
  labs(
    title = "Distribución del pH en manantiales colombianos",
    subtitle = paste0("Azul = Media (6.33)  |  Amarillo = Mediana (6.51)  |  n = ", nrow = n_ph),
    x = "pH (laboratorio)",
    y = "Número de manantiales"
  )
print(h1)
guardar_plot(h1, "histograma_ph")

# CONCLUSIÓN: La mayoría de manantiales son ligeramente ácidos (pH entre 5 y 7).
# La media menor que la mediana indica que algunos pozos muy ácidos
# arrastran la media hacia abajo.

# GRÁFICO 5.2: HISTOGRAMA DE TEMPERATURA
# POR QUÉ ESTE GRÁFICO: Temperatura es numérica continua → histograma.
# Nos permite clasificar si el agua es fría (<20°C) o termal (>20°C).
n_temp <- sum(!is.na(datos$TEMPERATUR))

h2 <- ggplot(datos %>% filter(!is.na(TEMPERATUR)), aes(x = TEMPERATUR)) +
  geom_histogram(bins = 9, fill = "#8bbee0", color = "white") +
  geom_vline(
    xintercept = mean(datos$TEMPERATUR, na.rm = TRUE),
    color = "firebrick", linetype = "dashed"
  ) +
  geom_vline(
    xintercept = median(datos$TEMPERATUR, na.rm = TRUE),
    color = "darkorange", linetype = "dotted"
  ) +
  labs(
    title = "Distribución de temperatura del agua",
    subtitle = paste0("Rojo = Media (41.75°C)  |  Naranja = Mediana (39°C)  |  n = ", nrow = n_temp),
    x = "Temperatura (°C)",
    y = "Número de manantiales"
  )
print(h2)
guardar_plot(h2, "histograma_temperatura")

# CONCLUSIÓN: La mayoría del agua sale entre 30°C y 60°C → aguas termales.
# La cola a la derecha (valores >80°C) explica que la media supere a la mediana.

# GRÁFICO 5.3: HISTOGRAMA DE CONDUCTIVIDAD
# Usamos escala logarítmica porque el rango es enorme (0 a 278,000 µS/cm).
# Sin log, el 95% de los datos se aplasta en la izquierda y no se ve nada.

n_cond <- sum(!is.na(datos$CONDUCTIVIDAD))

h3 <- ggplot(datos %>% filter(!is.na(CONDUCTIVIDAD)), aes(x = CONDUCTIVIDAD)) +
  geom_histogram(bins = 9, fill = "darkgreen", color = "white") +
  scale_x_log10() +
  labs(
    title = "Distribución de conductividad eléctrica (escala log)",
    subtitle = paste0("Escala logarítmica usada por el rango extremo de valores  |  n = ", nrow = n_cond),
    x = "Conductividad (µS/cm) — escala log",
    y = "Número de manantiales"
  )
print(h3)
guardar_plot(h3, "histograma_conductividad")

# CONCLUSIÓN: La mayoría de manantiales tiene conductividad entre 100 y 10,000 µS/cm.
# Unos pocos superan los 100,000, indicando agua muy salina 

# GRÁFICO 5.4: HISTOGRAMA DE SODIO
n_sodio <- sum(!is.na(datos$SODIO))

h4 <- ggplot(datos %>% filter(!is.na(SODIO)), aes(x = SODIO)) +
  geom_histogram(bins = 9, fill = "#a82339", color = "white") +
  scale_x_log10() +
  labs(
    title = "Distribución de las concentraciones de sodio",
    subtitle = paste0("Rojo = Media (706.78)  |  Naranja = Mediana (170.8)  |  n =", nrow = n_sodio),
    x = "Concentración de sodio (mg/L)",
    y = "Número de manantiales"
  )
print(h4)
guardar_plot(h4, "histograma_sodio")

# GRÁFICO 5.5: HISTOGRAMA DE CALCIO
n_calcio <- sum(!is.na(datos$CALCIO))

h5 <- ggplot(datos %>% dplyr::filter(!is.na(CALCIO)), aes(x = CALCIO)) +
  geom_histogram(bins = 9, fill = "#a82339", color = "white") +
  labs(
    title = "Distribución de las concentraciones de calcio",
    subtitle = paste0("n = ", nrow = n_calcio),
    x = "Concentración de calcio (mg/L)",
    y = "Número de manantiales"
  )
print(h5)
guardar_plot(h5, "histograma_calcio")