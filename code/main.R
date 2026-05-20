#========================================
# PROYECTO ESTADÍSTICO AGUAS
# Archivo principal
#========================================

# Limpiar entorno
rm(list = ls())

#========================================
# CARGAR FUNCIONES
#========================================
if(!require("here")) install.packages("here")

library(here)

source(here("code", "funciones", "funciones.r"))

#========================================
# EJECUTAR SCRIPTS
#========================================

source(here("code", "setup.R"))
source(here("code", "carga_datos.R"))
source(here("code", "limpieza.R"))
source(here("code", "estadistica_descriptiva.R"))
source(here("code", "histogramas.R"))
source(here("code", "barras_y_frecuencias.R"))
source(here("code", "boxplots.R"))
source(here("code", "dispersion_y_correlacion.R"))
# Añadir diagramas de torta (del script inicial)
# Añadir diagramas de tallo y hoja (del script inicial)

#========================================
# FIN DEL PROYECTO
#========================================

cat("Análisis completado correctamente.\n")