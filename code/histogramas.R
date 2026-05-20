# ========================
# 5. HISTOGRAMAS
# ========================

# GRÁFICO 5.1: HISTOGRAMA DE pH
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: El pH es numérica continua → histograma.
# Queremos ver si los manantiales son mayormente ácidos, neutros o básicos.
# bins = 9 porque tenemos 320 filas → regla de Sturges: 1 + log2(320) ≈ 9

h1 <- ggplot(datos, aes(x = PH_LABORATORIO)) +
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
    subtitle = "Azul = Media (6.33)  |  Amarillo = Mediana (6.51)",
    x = "pH (laboratorio)",
    y = "Número de manantiales"
  )

print(h1)

guardar_plot(
  h1, "histograma_ph"
)
# CONCLUSIÓN: La mayoría de manantiales son ligeramente ácidos (pH entre 5 y 7).
# La media menor que la mediana indica que algunos pozos muy ácidos
# arrastran la media hacia abajo.


# GRÁFICO 5.2: HISTOGRAMA DE TEMPERATURA
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: Temperatura es numérica continua → histograma.
# Nos permite clasificar si el agua es fría (<20°C) o termal (>20°C).

h2 <- ggplot(datos, aes(x = TEMPERATUR)) +
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
    subtitle = "Rojo = Media (41.75°C)  |  Naranja = Mediana (39°C)",
    x = "Temperatura (°C)",
    y = "Número de manantiales"
  )

print(h2)

guardar_plot(
  h2, "histograma_temperatura"
)
# CONCLUSIÓN: La mayoría del agua sale entre 30°C y 60°C → aguas termales.
# La cola a la derecha (valores >80°C) explica que la media supere a la mediana.


# GRÁFICO 5.3: HISTOGRAMA DE CONDUCTIVIDAD
# (Objetivo 1)
# POR QUÉ ESTE GRÁFICO: Conductividad es numérica continua → histograma.
# Usamos escala logarítmica porque el rango es enorme (0 a 278,000 µS/cm).
# Sin log, el 95% de los datos se aplasta en la izquierda y no se ve nada.

h3 <- ggplot(datos, aes(x = CONDUCTIVIDAD)) +
  geom_histogram(bins = 9, fill = "darkgreen", color = "white") +
  scale_x_log10() + # escala logarítmica en el eje X
  labs(
    title = "Distribución de conductividad eléctrica (escala log)",
    subtitle = "Escala logarítmica usada por el rango extremo de valores",
    x = "Conductividad (µS/cm) — escala log",
    y = "Número de manantiales"
  )
# CONCLUSIÓN: La mayoría de manantiales tiene conductividad entre 100 y 10,000 µS/cm.
# Unos pocos superan los 100,000, indicando agua muy salina (probablemente fumarolas).

print(h3)

guardar_plot(
  h3, "histograma_conductividad"
)

# GRAFICO 5.4: HISTOGRAMA DE LAS CONCENTRACIONES DE SODIO
h4 <- ggplot(datos, aes(x = SODIO)) +
  geom_histogram(bins = 9, fill = "#a82339", color = "white") +
  scale_x_log10() + # escala logarítmica en el eje X
  labs(
    title = "Distribución de las concentraciones de sodio",
    subtitle = "Rojo = Media (706.78)  |  Naranja = Mediana (170.8)",
    x = "Concentración de sodio (mg/L)",
    y = "Número de manantiales"
  )

print(h4)

guardar_plot(
  h4, "histograma_sodio"
)

# GRAFICO 5.5: HISTOGRAMA DE LAS CONCENTRACIONES DE CALCIO
h5 <- ggplot(datos, aes(x = CALCIO)) +
  geom_histogram(bins = 9, fill = "#a82339", color = "white") +
  labs(
    x = "Concentración de calcio (mg/L)",
    y = "Número de manantiales"
  )

print(h5)

guardar_plot(
  h5, "histograma_calcio"
)
