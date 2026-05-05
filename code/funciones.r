# En este archivo se encuentran las funciones que se utilizan en el script principal
# Estas funciones se definen para hacer el código más legible y evitar la repetición de código

# ==============================================================
# 1. Función para hallar la MODA
# ==============================================================
# Se define una función para hallar la moda de una variable
# Esto es necesario porque R no tiene una función para hallar la moda
# y se utiliza la función table() para crear una tabla de frecuencias
# y luego se busca la frecuencia máxima
# na.rm = TRUE para que ignore los valores nulos
moda <- function(x) {
    x <- na.omit(x)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# ==============================================================
# 2. Normalización de las variables categóricas CLASIFICACIÓN y OLOR
# ==============================================================
# CLASIFICACIÓN tiene errores de escritura (mayúsculas/minúsculas mezcladas
# y espacios inconsistentes alrededor de guiones, ej: "Clorurada-Sulfatada"
# y "Clorurada - Sulfatada").
# Pasos de limpieza:
#   1. str_trim()   -> elimina espacios al inicio y al final
#   2. str_squish() -> colapsa espacios internos múltiples en uno
#   3. gsub()       -> estandariza los separadores: siempre " - " (con espacios)
#   4. str_to_title() -> unifica mayúsculas/minúsculas

normalizar <- function(x) {
    x <- str_trim(x) # quita espacios inicio/fin
    x <- str_squish(x) # colapsa espacios internos
    # Normaliza cualquier variante de guion (con o sin espacios) a " - "
    x <- gsub("\\s*-\\s*", " - ", x)
    # Normaliza barras "/" igual: " / "
    x <- gsub("\\s*/\\s*", " / ", x)
    x <- str_to_title(x) # Title Case
    return(x)
}

# ==============================================================
# 3. MEDA: mediana de las desviaciones absolutas respecto a la mediana
# ==============================================================
# Más robusta que la desv. estándar cuando hay valores atípicos
# Indica cuánto se desvían los datos respecto a la mediana, es útil para valores muy extremos
meda <- function(x) {
    x <- na.omit(x)
    median(abs(x - median(x)))
}
# ==============================================================
# 4. COEFICIENTE DE VARIACION
# ==============================================================
calc_cv <- function(x) {
  (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
} 

# ==============================================================
# ASIMETRIA DE BOWLEY
# ==============================================================
# Fórmula: B = (Q3 + Q1 - 2*Q2) / (Q3 - Q1)
# Rango: siempre entre -1 y +1.
# Interpretación:
#   B > 0  cola derecha (igual que Fisher positivo)
#   B < 0  cola izquierda
#   B = 0  simétrico respecto a los cuartiles
# Ventaja: solo usa Q1, Q2, Q3  inmune a valores extremos..

asimetria_bowley <- function(x) {
  x <- na.omit(x)
  q <- quantile(x, probs = c(0.25, 0.50, 0.75))
  Q1 <- q[1]; Q2 <- q[2]; Q3 <- q[3]
  # Si el rango intercuartílico es 0, la fórmula no está definida
  if ((Q3 - Q1) == 0) return(NA_real_)
  (Q3 + Q1 - 2 * Q2) / (Q3 - Q1)
}
# ==============================================================
# CURTOSIS DE MOORS
# ==============================================================

# Fórmula: M = (E7 - E5 + E3 - E1) / (E6 - E2)
# donde E1..E8 son los octiles (cuantiles de orden 1/8, 2/8, ..., 7/8).
# Rango: valores > 0 indican colas más pesadas que una distribución
#   con referencia uniforme en los cuartiles.
# Interpretación:
#   M alto  distribución con colas pesadas (leptocúrtica en sentido cuantílico)
#   Referencia: para una distribución normal, M ≈ 1.23
# Ventaja: solo usa cuantiles → inmune a valores extremos.

curtosis_moors <- function(x) {
  x <- na.omit(x)
  e <- quantile(x, probs = c(1/8, 2/8, 3/8, 5/8, 6/8, 7/8))
  # e[1]=E1, e[2]=E2, e[3]=E3, e[4]=E5, e[5]=E6, e[6]=E7
  denominador <- e[5] - e[2]   # E6 - E2
  if (denominador == 0) return(NA_real_)
  (e[6] - e[4] + e[3] - e[1]) / denominador
}