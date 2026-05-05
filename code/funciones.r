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
