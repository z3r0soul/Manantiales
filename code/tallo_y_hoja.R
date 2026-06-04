# ==============================================================
# DIAGRAMAS DE TALLO Y HOJA
# ==============================================================

# pH — escala = 2 para mostrar un decimal de resolución
cat("\n══ Tallo y hoja: pH (valores > 0) ══\n")

# 1. Extraemos los valores de pH válidos como un vector
ph_manantiales <- datos %>% 
  filter(!is.na(PH_LABORATORIO)) %>% 
  pull(PH_LABORATORIO)

# 2. Dibujamos el diagrama de tallo y hojas en la consola
stem(ph_manantiales, scale = 2)


# Temperatura — scale = 1 agrupa en decenas
cat("\n══ Tallo y hoja: Temperatura (valores > 0) ══\n")

temp_manantiales <- datos %>%
  filter(!is.na(TEMPERATUR)) %>%
  pull (TEMPERATUR)

stem(temp_manantiales, scale = 1)