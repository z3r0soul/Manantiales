# =============================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS
# =============================================================
# Se calculan: media, media recortada (5%), mediana, moda,
# desviación estándar muestral y poblacional, varianza muestral,
# MEDA, MAD, coeficiente de variación (CV), asimetría, curtosis y rango.

# pH — va de 0 a 14. Mínimo de 0.0 es sospechoso, el rango lo revela  .
describir_variable(datos$PH_LABORATORIO, "pH", bowley = FALSE, moors = FALSE)

# Temperatura — rango esperado 0-94°C, valores en 0 son sospechosos
describir_variable(datos$TEMPERATUR, "Temperatura", bowley = FALSE, moors = FALSE)

# Conductividad — rango enorme (0 a 278,000): media recortada muy útil aquí
describir_variable(datos$CONDUCTIVIDAD, "Conductividad", bowley = TRUE, moors = TRUE)

# Calcio
describir_variable(datos$CALCIO, "Calcio", bowley = FALSE, moors = FALSE)

# Sodio — media (706) muy lejana a mediana (171): caso ideal para media recortada
describir_variable(datos$SODIO, "Sodio", bowley = FALSE, moors = FALSE)