# 1.- Project1

# 2.- Jorge De Vivar

# 3-. Fecha de finalizaci?n: 11/3/2022, 
#     Fecha de inicio: 9/3/2022

# 4.- R 4.1.2.

# 5.- Imputaci?n de datos

###############################Cargar librer?as#################################

library (readxl)
library(naniar)
library(visdat)
library(tidyverse)
library(simputation)
library(rio)
library(mice)
library(palmerpenguins)
library(missForest)

################################################################################

############################Load_functions######################################
source("R/SEPARA_muestras.R")
source("R/METODO_media.R")
source("R/METODO_cohen.R")
source("R/METODO_NNI.R")
source("R/METODO_razon.R")
source("R/METODO_regresion.R")
source("R/METODO_razon_aleatorio.R")
source("R/METODO_regresion_aleatorio.R")
source("R/METODO_RHD.R")
source("R/REALIZA_imputacion.R")

################################################################################

############################Introducci?n########################################

# Buscar la ruta del archivo del excel
#file.choose()

# Ruta de la consola y guardar en variable
ruta_excel <- "raw_data/DatosTFG.xlsx"

# Hojas del Excel
excel_sheets(ruta_excel)

# Importar los datos de la Hoja Exterior
dato_exterior <- read_excel(ruta_excel, sheet = "Exterior")

# Pasar a numerico la Humedad exterior
yna <- suppressWarnings(as.numeric(dato_exterior$Humedad))

# Pasar a numerico la Temperatura exterior
zna <- suppressWarnings(as.numeric(dato_exterior$Temperatura))

#Obtener los datos de tiempo
x <- dato_exterior$FechaNum

# Saber los datos faltantes de la humedad y temperatura exterior
sum(is.na(yna))
sum(is.na(zna))

################################################################################

#######################MISSFOREST###############################################
# Bosques de decisi?n

# Utilizamos el DataFrame
datos <- data.frame (x,yna,zna)

# Hacemos la imputaci?n
imp <- missForest(datos, verbose = TRUE, variablewise = FALSE)
imp$OOBerror
imp <- missForest(datos, verbose = TRUE, variablewise = TRUE)
imp$OOBerror
sapply(datos,class)

datoslimpio <- as.data.frame(imp$ximp) %>% rename(Hum_missForest = yna,
                                                  Tem_missForest = zna,
                                                  fecha_num = x)
# View(datoslimpio)

################################################################################

################################################################################

# Save data
export(datoslimpio,"processed_data/ImputacionDatosMF.xlsx")
write_csv(datoslimpio,"processed_data/ImputacionDatosMF.csv")
####FIN CODIGO##################################################################
