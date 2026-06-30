# =============================================================================
# 1. TABLAS DE FRECUENCIAS — UNA SOLA VARIABLE CATEGÓRICA
# Variables: CLASIFICACION_LIMPIA, OLOR_LIMPIO, PH_CAT, TEMP_CAT
# =============================================================================


# CLASIFICACIÓN QUÍMICA DEL AGUA (CLASIFICACION_LIMPIA)

datos_clasif <- datos %>% filter(!is.na(CLASIFICACION_LIMPIA))

tabla_clasif <- table(Clasificacion = datos_clasif$CLASIFICACION_LIMPIA)

cat("\n══ CLASIFICACION_LIMPIA — frecuencias ABSOLUTAS ══\n")
print(tabla_clasif)

cat("\n  Frecuencias RELATIVAS (% del total de manantiales):\n")
print(round(prop.table(tabla_clasif) * 100, 1))


# ── OLOR DEL AGUA (OLOR_LIMPIO) ───────────────────────────────────────────
# Justificación: describir qué tan frecuente es cada tipo de olor.
# Se espera que "Ausente" domine, pero los olores intensos (Fuerte, H2S)
# son geoquímicamente relevantes porque indican presencia de gases
# volcánicos disueltos como H2S y CO2.

datos_olor <- datos %>% filter(!is.na(OLOR_LIMPIO))

tabla_olor <- table(Olor = datos_olor$OLOR_LIMPIO)

cat("\n══ OLOR_LIMPIO — frecuencias ABSOLUTAS ══\n")
print(tabla_olor)

cat("\n  Frecuencias RELATIVAS (% del total de manantiales):\n")
print(round(prop.table(tabla_olor) * 100, 1))


# ── 3. CATEGORÍA DE pH (PH_CAT) ──────────────────────────────────────────────
# Justificación: resumir la distribución del pH continuo en tres grupos
# con significado geoquímico claro.
# Umbrales OMS: Ácido < 6.5 | Neutro 6.5–7.5 | Básico > 7.5
# Se espera que la mayoría sea ácida, consistente con la mediana
# de pH = 6.51 reportada en las estadísticas descriptivas.

datos_ph <- datos %>% filter(!is.na(PH_CAT))

tabla_ph <- table(pH = datos_ph$PH_CAT)

cat("\n══ PH_CAT — frecuencias ABSOLUTAS ══\n")
print(tabla_ph)

cat("\n  Frecuencias RELATIVAS (% del total de manantiales):\n")
print(round(prop.table(tabla_ph) * 100, 1))


# ── 4. CATEGORÍA DE TEMPERATURA (TEMP_CAT) ───────────────────────────────────
# Justificación: resumir la distribución de la temperatura continua
# en tres grupos con significado hidrogeológico.
# Umbrales: Fría < 20°C | Tibia 20–50°C | Termal > 50°C
# Se espera que la mayoría sea tibia o termal, confirmando que el
# dataset está dominado por sistemas geotérmicos activos, coherente
# con la actividad volcánica y tectónica del territorio colombiano.

datos_temp <- datos %>% filter(!is.na(TEMP_CAT))

tabla_temp <- table(Temperatura = datos_temp$TEMP_CAT)

cat("\n══ TEMP_CAT — frecuencias ABSOLUTAS ══\n")
print(tabla_temp)

cat("\n  Frecuencias RELATIVAS (% del total de manantiales):\n")
print(round(prop.table(tabla_temp) * 100, 1))

# =============================================================
# 2. TABLAS CRUZADAS BIVARIADAS
# =============================================================

# PH_CAT x TEMP_CAT (pH vs temperatura categóricas)
# Justificación: si el agua termal tiende a ser más ácida,
# se confirma la relación entre temperatura y acidez que
# también muestra el gráfico de dispersión.

datos_pt <- datos %>% filter(!is.na(PH_CAT), 
                             !is.na(TEMP_CAT)
)
tabla_pt <- table(pH = datos$PH_CAT, 
                  Temperatura = datos$TEMP_CAT
)

cat("\n══ PH_CAT × TEMP_CAT — frecuencias ABSOLUTAS ══\n")
print(tabla_pt)

cat("\n  Proporciones por FILA (% dentro de cada categoría de pH):\n")
print(round(prop.table(tabla_pt, margin = 1) * 100, 1))

cat("\n  Proporciones por COLUMNA (% dentro de cada categoría de temperatura):\n")
print(round(prop.table(tabla_pt, margin = 2) * 100, 1))


# Se reportan frecuencias absolutas y proporciones por fila y columna.
# Por fila: qué % de cada tipo de agua tiene cada pH.
# Por columna: qué % de cada categoría de pH corresponde a cada tipo.

# Tabla cruzada: PH_CAT × CLASIFICACIÓN 
# Justificación: permite ver si el tipo de agua está asociado
# con la acidez del manantial. La Sulfatada debería tener más ácidos
# por la oxidación de sulfuros; la Bicarbonatada, más neutros.

tabla_cruzada <- table(
  pH          = datos_top3$PH_CAT,
  Clasificacion = datos_top3$CLASIFICACION_LIMPIA
)

cat("\n══ Tabla cruzada: pH × Clasificación — frecuencias absolutas ══\n")
print(tabla_cruzada)

cat("\n  Proporciones por fila (% dentro de cada categoría de pH):\n")
print(round(prop.table(tabla_cruzada, margin = 1) * 100, 1))

cat("\n  Proporciones por columna (% dentro de cada tipo de agua):\n")
print(round(prop.table(tabla_cruzada, margin = 2) * 100, 1))

# ── Tabla cruzada: PH_CAT × OLOR ─────────────────────────────
# Justificación: el H2S baja el pH — esperamos que "Ácido" tenga
# mayor proporción de olor a H2S y "Fuerte" que los otros grupos.
datos_olor <- datos %>% filter(!is.na(PH_CAT),
                               !is.na(OLOR_LIMPIO))

tabla_ph_olor <- table(
  pH   = datos_olor$PH_CAT,
  Olor = datos_olor$OLOR_LIMPIO
)

cat("\n══ Tabla cruzada: pH × Olor — frecuencias absolutas ══\n")
print(tabla_ph_olor)

cat("\n  Proporciones por fila (% dentro de cada categoría de pH):\n")
print(round(prop.table(tabla_ph_olor, margin = 1) * 100, 1))

cat("\n  Proporciones por columna (% dentro de cada olor):\n")
print(round(prop.table(tabla_ph_olor, margin = 2) * 100, 1))

# ==============================================
# 6. DIAGRAMAS DE BARRAS (1 variable)
# ==============================================
# Usamos las columnas LIMPIAS para que las variantes normalizadas se agrupen.

# 6.1 Clasificacion del agua
b1 <- ggplot(datos_top3 %>% filter(!is.na(CLASIFICACION_LIMPIA)), aes(x = CLASIFICACION_LIMPIA, fill =  CLASIFICACION_LIMPIA)) +
  geom_bar() +
  guides(fill = "none") +
  labs(
    title = "Clasificación del agua segun la \ncantidad de manantiales",
    x = "Clasificaciones principales del agua",
    y = "Número de manantiales"
  ) + 
  theme_minimal()

print(b1)

guardar_plot(
  b1,
  "barras_clasificacion",
  "graficos_barras"
)


# 6.2 Grafico de olor por cantidad de manantiales
# Ordenamos de forma creciente
# Reordenamos los niveles de Olor de menor a mayor frecuencia antes de graficar


datos_analisis$OLOR_LIMPIO <- reorder(datos_analisis$OLOR_LIMPIO, datos_analisis$OLOR_LIMPIO, length)
b2 <- ggplot(datos_analisis, aes(x = OLOR_LIMPIO, fill = OLOR_LIMPIO)) +
  geom_bar() +
  labs(
    title = "Olor del agua según la cantidad de manantiales",
    x = "Tipo de Olor",
    y = "Cantidad de manantiales"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(b2)

guardar_plot(
  b2,
  "barras_olor",
  "graficos_barras"
)


# 6.3 pH categorizado — nueva variable derivada
# Justificación: PH_CAT convierte el pH continuo en tres grupos
# geoquímicamente significativos y permite comparaciones directas
# con variables categóricas como Clasificación.
b3 <- ggplot(datos %>% filter(!is.na(PH_CAT), PH_LABORATORIO >= 4.0 ),
             aes(x = PH_CAT, fill = PH_CAT)) +
  geom_bar() +
  scale_fill_manual(values = c("Ácido" = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  guides(fill = "none") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 4) +
  labs(title = "Categorías de pH en manantiales colombianos",
       subtitle = "Ácido < 6.5  |  Neutro 6.5–7.5  |  Básico > 7.5",
       x = "Categoría de pH", y = "Número de manantiales")

print(b3)

guardar_plot(
  b3,
  "barras_ph",
  "graficos_barras"
)


# 6.4  Grafico de temperatura categorizada

  b4 <- ggplot(datos %>% filter(!is.na(TEMP_CAT)), aes(x = TEMP_CAT, fill = TEMP_CAT)) +
    geom_bar() +
    scale_fill_manual(values = c("Tibia"  = "#f4a582",
                                 "Termal" = "#d6604d")) +
    guides(fill = "none") +
    geom_text(stat = "count", aes(label = after_stat(count)),
              vjust = -0.5, size = 4) +
    labs(title = "Categorías de temperatura en manantiales colombianos",
         subtitle =   " Tibia 20–50°C  |  Termal >50°C",
         x = "Categoría de temperatura", y = "Número de manantiales")
  
  print(b4)

guardar_plot(
  b4,
  "barras_temperatura",
  "graficos_barras"
)


# 6.5 Grafico de mediciones por anio

b5 <- ggplot(datos, aes(x = ANIO)) +
  geom_bar(fill = "#00a81e") +
  coord_flip() +
  labs(title = "Mediciones por año", x = NULL, y = "Cantidad de Manantiales")

print(b5)

guardar_plot(
  b5,
  "barras_mediciones_anio",
  "graficos_barras"
)




# =============================================================
# 7. DIAGRAMAS DE BARRAS — 2 VARIABLES
# =============================================================

# 7.1 Diagrama de pH vs Temperatura
bb1 <- datos %>%
  filter(!is.na(PH_CAT), !is.na(TEMP_CAT), TEMP_CAT != "Fría") %>%
  ggplot(aes(x = TEMP_CAT, fill = PH_CAT)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Ácido"  = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  labs(title = "Categoría de pH según temperatura del agua",
       subtitle = "Cada barra = 100% de los manantiales de esa temperatura",
       x = "Temperatura", y = "Proporción", fill = "pH")

print(bb1)

guardar_plot(
  bb1,
  "barras_ph_vs_temp",
  "graficos_barras"
)


# 7.2 PH_CAT x CLASIFICACIÓN 
# Este gráfico responde: ¿el tipo de agua está asociado con la acidez?
# Solo top 3 clasificaciones para legibilidad.

bb2 <- datos %>%
  filter(CLASIFICACION_LIMPIA %in% top3, !is.na(PH_CAT), PH_LABORATORIO >= 4.0) %>%
  ggplot(aes(x = CLASIFICACION_LIMPIA, fill = PH_CAT)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Ácido"  = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  labs(title = "Categoría de pH según tipo de agua",
       subtitle = "Cada barra = 100% de los manantiales de ese tipo",
       x = NULL, y = "Proporción", fill = "Categoría pH")

print(bb2)

guardar_plot(
  bb2,
  "barras_clas_vs_ph",
  "graficos_barras"
)

# RESULTADO ESPERADO: Sulfatada debería tener mayor proporción de
# Ácido porque sus aguas provienen de oxidación de sulfuros.
# Bicarbonatada debería mostrar más Neutro/Básico.

# 7.3 PH_CAT x OLOR 
# Justificación: el H2S disuelto reduce el pH.
# Si los manantiales con olor a H2S tienen más categoría Ácido,
# se confirma la relación entre origen volcánico y acidez.
  

bb3 <- datos %>%
  filter(!is.na(PH_CAT), 
         !is.na(OLOR_BIN),
         PH_LABORATORIO >= 4.0) %>%
  ggplot(aes(x = OLOR_BIN, fill = PH_CAT)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Ácido"  = "#d73027",
                               "Neutro" = "#fee08b",
                               "Básico" = "#1a9850")) +
  labs(title = "Categoría de pH según presencia de olor",
       subtitle = "Cada barra = 100% de los manantiales con ese olor",
       x = NULL, y = "Proporción", fill = "Categoría pH")

print(bb3)

guardar_plot(
  bb3,
  "barras_ph_vs_olor",
  "graficos_barras"
)



# 7.4 Grafica OLOR VS CLASIFICACION
# Esta grafica permite afirmar que l
colores_olor <- c(
  "Con Olor" = "Black",
  "Sin Olor" = "White"
)

bb4 <- datos %>%
  filter(
    CLASIFICACION_LIMPIA %in% top3,
    !is.na(OLOR_BIN)
  ) %>%
  ggplot(aes(x = CLASIFICACION_LIMPIA, fill = OLOR_BIN)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Sin olor" = "#74add1",
                               "Con olor" = "#d73027")) +
  labs(
    title   = "Presencia de olor según tipo de agua",
    subtitle = "Sulfatada tiene mayor proporción de olor detectable | Top 3",
    x       = "Clasificación química",
    y       = "Proporción",
    fill    = "Olor"
  ) +
  theme_minimal()

print(bb4)


guardar_plot(
  bb4,
  "barras_clas_vs_olor",
  "graficos_barras"
)

