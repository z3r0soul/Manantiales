# =========================================================
# DENSIDAD KERNEL SOBRE ESCALA LOGARÍTMICA (log10)
# =========================================================

# Se crea la variable transformada en log10 (descartando ceros - NA si existen)
datos_cond_log <- datos %>%
  filter(CONDUCTIVIDAD > 0) %>%
  mutate(log_cond = log10(CONDUCTIVIDAD))

x_log <- datos_cond_log$log_cond

# Se Calculan los anchos de banda sobre la variable transformada
h_optimo  <- bw.nrd0(x_log)
h_pequeno <- h_optimo * 0.25
h_grande  <- h_optimo * 3.5

# Se asignan las densidades
d_pequeno <- density(x_log, bw = h_pequeno, kernel = "gaussian")
d_optimo  <- density(x_log, bw = h_optimo,  kernel = "gaussian")
d_grande  <- density(x_log, bw = h_grande,  kernel = "gaussian")

df_dens_log <- bind_rows(
  data.frame(x = d_pequeno$x, y = d_pequeno$y, Tipo = paste0("h pequeño (", round(h_pequeno, 2), ")")),
  data.frame(x = d_optimo$x,  y = d_optimo$y,  Tipo = paste0("h óptimo (", round(h_optimo, 2), ")")),
  data.frame(x = d_grande$x,  y = d_grande$y,  Tipo = paste0("h grande (", round(h_grande, 2), ")"))
)

df_dens_log$Tipo <- factor(df_dens_log$Tipo, levels = unique(df_dens_log$Tipo))

# 4. Gráfico
ggplot(datos_cond_log, aes(x = log_cond)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 40, 
    fill = "gray90", 
    color = "gray60", 
    alpha = 0.8
  ) +
  geom_line(
    data = df_dens_log, 
    aes(x = x, y = y, color = Tipo, linetype = Tipo), 
    linewidth = 1.05
  ) +
  scale_color_manual(values = c("firebrick", "darkblue", "forestgreen")) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash")) +
  labs(
    title = "Estimación Kernel de la Conductividad (Escala Log10)",
    subtitle = "Comparación de anchos de banda (h) usando Kernel Gaussiano",
    x = expression(log[10]*"(Conductividad en µS/cm)"),
    y = "Densidad",
    color = "Ancho de banda (h):",
    linetype = "Ancho de banda (h):"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
  