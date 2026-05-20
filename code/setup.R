# ==============================================================
# SRIPT ENCARGADO DE LA CARGA DE LIBRERIAS 
# ==============================================================

# Instalar paquetes si no están instalados

if (!require("here")) install.packages("here")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")

# Cargar librerías necesarias
library(patchwork)
library(here)
library(scales)
library(ggplot2)
library(readr)
library(moments)
library(dplyr) # %>%, mutate, filter, count
library(lubridate) # year()
library(stringr) # str_trim, str_squish, str_to_title