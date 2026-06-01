# ==============================================================
# DIAGRAMAS DE TALLO Y HOJA
# ==============================================================

# pH — escala = 2 para mostrar un decimal de resolución
cat("\n══ Tallo y hoja: pH (valores > 0) ══\n")

stem(datos$PH_LABORATORIO, scale = 2)

# Temperatura — scale = 1 agrupa en decenas
cat("\n══ Tallo y hoja: Temperatura (valores > 0) ══\n")

stem(datos$TEMPERATUR, scale = 1)
