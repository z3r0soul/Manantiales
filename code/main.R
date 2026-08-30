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
# Cierra cualquier dispositivo gráfico colgado de una corrida anterior
while (!is.null(dev.list())) dev.off()

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
source (here("code", "tallo_y_hoja.R"))
source(here("code", "distancias_y_chernoff.R"))
source(here("code", "similaridad.R"))
source(here("code", "kernel.R"))

#========================================
# FIN DEL PROYECTO
#========================================

cat("Análisis completado correctamente.\n")