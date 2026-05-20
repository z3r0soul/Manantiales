# =============================================================
# ARCHIVO ENCARGADO DE LA CARGA Y PREPARACIÓN DE DATOS
# =============================================================

# Carga de datos desde el archivo CSV, identificando valores erróneos y faltantes como NA
# El CSV tiene valores -999 que significan "dato faltante".
# read_csv2 ya los convierte a NA gracias a na = c(..., "-999").
datos <- read_csv2(
  here("data", "raw", "Manantial_limpio.csv"),
  locale = locale(encoding = "UTF-8"),
  na = c("", "NA", "-999")
)