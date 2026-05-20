# =============================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS
# =============================================================
# Se calculan: media, media recortada (5%), mediana, moda,
# desviación estándar muestral y poblacional, varianza muestral,
# MEDA, MAD, coeficiente de variación (CV), asimetría, curtosis y rango.

# pH — va de 0 a 14. Mínimo de 0.0 es sospechoso, el rango lo revela  .
cat("── pH ──\n")
mean(datos$PH_LABORATORIO, na.rm = TRUE) # media
mean(datos$PH_LABORATORIO, na.rm = TRUE, trim = 0.05) # media recortada 5%
median(datos$PH_LABORATORIO, na.rm = TRUE) # mediana
moda(datos$PH_LABORATORIO) # moda
sd(datos$PH_LABORATORIO, na.rm = TRUE) # desv. estándar muestral
meda(datos$PH_LABORATORIO) # MEDA
mad(datos$PH_LABORATORIO, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$PH_LABORATORIO)
skewness(na.omit(datos$PH_LABORATORIO)) # asimetría
kurtosis(na.omit(datos$PH_LABORATORIO)) # curtosis
max(datos$PH_LABORATORIO, na.rm = TRUE) - min(datos$PH_LABORATORIO, na.rm = TRUE) # rango

# Temperatura — rango esperado 0-94°C, valores en 0 son sospechosos
cat("── Temperatura ──\n")
mean(datos$TEMPERATUR, na.rm = TRUE)
mean(datos$TEMPERATUR, na.rm = TRUE, trim = 0.05)
median(datos$TEMPERATUR, na.rm = TRUE)
moda(datos$TEMPERATUR)
sd(datos$TEMPERATUR, na.rm = TRUE)
meda(datos$TEMPERATUR)
mad(datos$TEMPERATUR, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$TEMPERATUR)
skewness(na.omit(datos$TEMPERATUR))
kurtosis(na.omit(datos$TEMPERATUR))
max(datos$TEMPERATUR, na.rm = TRUE) - min(datos$TEMPERATUR, na.rm = TRUE)

# Conductividad — rango enorme (0 a 278,000): media recortada muy útil aquí
cat("── Conductividad ──\n")
mean(datos$CONDUCTIVIDAD, na.rm = TRUE)
mean(datos$CONDUCTIVIDAD, na.rm = TRUE, trim = 0.05)
median(datos$CONDUCTIVIDAD, na.rm = TRUE)
moda(datos$CONDUCTIVIDAD)
sd(datos$CONDUCTIVIDAD, na.rm = TRUE)
meda(datos$CONDUCTIVIDAD)
mad(datos$CONDUCTIVIDAD, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$CONDUCTIVIDAD)
skewness(na.omit(datos$CONDUCTIVIDAD))
asimetria_bowley(na.omit(datos$CONDUCTIVIDAD))
kurtosis(na.omit(datos$CONDUCTIVIDAD))
curtosis_moors(na.omit(datos$CONDUCTIVIDAD))
max(datos$CONDUCTIVIDAD, na.rm = TRUE) - min(datos$CONDUCTIVIDAD, na.rm = TRUE)

# Calcio
cat("── Calcio ──\n")
mean(datos$CALCIO, na.rm = TRUE)
mean(datos$CALCIO, na.rm = TRUE, trim = 0.05)
median(datos$CALCIO, na.rm = TRUE)
moda(datos$CALCIO)
sd(datos$CALCIO, na.rm = TRUE)
meda(datos$CALCIO)
mad(datos$CALCIO, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$CALCIO)
skewness(na.omit(datos$CALCIO))
kurtosis(na.omit(datos$CALCIO))
max(datos$CALCIO, na.rm = TRUE) - min(datos$CALCIO, na.rm = TRUE)

# Sodio — media (706) muy lejana a mediana (171): caso ideal para media recortada
cat("── Sodio ──\n")
mean(datos$SODIO, na.rm = TRUE)
mean(datos$SODIO, na.rm = TRUE, trim = 0.05)
median(datos$SODIO, na.rm = TRUE)
moda(datos$SODIO)
sd(datos$SODIO, na.rm = TRUE)
meda(datos$SODIO)
mad(datos$SODIO, na.rm = TRUE) # desviación absoluta mediana
calc_cv(datos$CONDUCTIVIDAD)
skewness(na.omit(datos$SODIO))
kurtosis(na.omit(datos$SODIO))
max(datos$SODIO, na.rm = TRUE) - min(datos$SODIO, na.rm = TRUE)