#------------------------------------------------------------------------------------#
#### Scripts para generar inventarios de SIMANFOR a partir de parcelas del IFN    ####
# Elaborado y reestructurado por Aitor Vázquez Veloso, 2021
# Última revisión: 19/05/2023
#------------------------------------------------------------------------------------#


### Establecemos el directorio de trabajo
setwd("./github/")


### Cargamos las librerías previamente instaladas

#install.packages('plyr')
library(plyr)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readr")
library(readr)

#install.packages("openxlsx")
library(openxlsx)


### Cargamos los archivos iniciales

# datos de parcelas
IFN2_plots <- read.csv("./data/IFN/1_processed/checkpoint_2/IFN2_plots-cp2.csv")
IFN3_plots <- read.csv("./data/IFN/1_processed/checkpoint_2/IFN3_plots-cp2.csv")

# datos de árboles
IFN2_trees <- read.csv("./data/IFN/1_processed/checkpoint_1/IFN2_trees-cp1.csv")
IFN3_trees <- read.csv("./data/IFN/1_processed/checkpoint_1/IFN3_trees-cp1.csv")


### Seleccionar las parcelas que vamos a necesitar, filtrando tanto la especie como su localización (se pueden crear nuevas)

# Filtro por especie, empleando el código de especie del IFN - https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# Seleccionamos la especie deseada y ejecutamos las dos órdenes de filtrado

especies <- 21  # Pinus sylvestris
especies <- 22  # Pinus uncinata
especies <- 23  # Pinus pinea
especies <- 24  # Pinus halepensis
especies <- 25  # Pinus nigra
especies <- 26  # Pinus pinaster
especies <- 27  # Pinus canariensis
especies <- 28  # Pinus radiata

especies <- 31  # Abies alba
especies <- 32  # Abies pinsapo
especies <- 33  # Picea abies
especies <- 34  # Pseudotsuga menziesii

especies <- 41  # Quercus robur
especies <- 42  # Quercus petraea
especies <- 43  # Quercus pyrenaica
especies <- 44  # Quercus faginea
especies <- 45  # Quercus ilex
especies <- 46  # Quercus suber
especies <- 47  # Quercus canariensis
especies <- 48  # Quercus rubra

especies <- 51  # Populus alba
especies <- 61  # Eucalyptus globulus
especies <- 62  # Eucalyptus camaldulensis
especies <- 64  # Eucalyptus nittens
especies <- 71  # Fagus sylvatica
especies <- 72  # Castanea sativa
especies <- 73  # Betula spp.
especies <- 258  # Populus x euramericana

plots_IFN2_esp <- IFN2_plots[
  (IFN2_plots$SPECIE_IFN_ID == especies),
]

plots_IFN3_esp <- IFN3_plots[
  (IFN3_plots$SPECIE_IFN_ID == especies),
]

# Filtro por provincia, empleando el código INE - https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm
# Seleccionamos la lista de provincias a las que es aplicable el modelo que vamos a utilizar

provincias <- c(5, 9, 40, 42) # IBEROPS
provincias <- c(16, 19, 42, 44) # IBEROPT

provincias <- c(1,9) # Alto Valle del Ebro
provincias <- c(4, 11, 14, 18, 21, 23, 29, 41) # Andaluc?a
provincias <- c(22, 44, 50) # Arag?n
provincias <- c(8, 17, 22, 25, 43, 44, 50) # Arag?n y Catalu?a (mezcla)
provincias <- c(5, 9, 24, 34, 37, 40, 42, 47, 49) # Castilla y Le?n
provincias <- c(8, 17, 25, 43) # Catalu?a
provincias <- c(15, 27, 32, 36) # Galicia
provincias <- c(15, 27, 32, 36, 24) # Galicia y Le?n
provincias <- c(28) # Madrid
provincias <- c(34) # Palencia
provincias <- c(5, 9, 28, 34, 37, 40, 47, 49) # Sistema Central
provincias <- c(9, 28, 40, 42) # Sistema Ib?rico y Sistema Central
provincias <- c(16, 19, 42, 44) # Sistema Ib?rico Meridional
provincias <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52)  # TODA la península ibérica

plots_IFN2_esp_prov <- plots_IFN2_esp[
  (plots_IFN2_esp$Provincia %in% provincias),
]

plots_IFN3_esp_prov <- plots_IFN3_esp[
  (plots_IFN3_esp$Provincia %in% provincias),
]


### Eliminar las parcelas con edad y Ho = 0, dado que son parcelas NO válidas para los modelos habituales con los que trabaja SIMANFOR
# La edad suele ser una variable obligatoria, aunque no para todos los modelos
# La Ho = 0 significa que, por alguna razón, la parcela con la que trabajamos no tiene árboles, por tanto no sirve para nuestro propósito

plots_IFN2_esp_prov <- plots_IFN2_esp_prov[
  (plots_IFN2_esp_prov$DOMINANT_H != 0),
]

plots_IFN3_esp_prov <- plots_IFN3_esp_prov[
  (plots_IFN3_esp_prov$DOMINANT_H != 0),
]

#
# Aplicar el filtro de edad de a continuación SOLO cuando sea neceario
#

plots_IFN2_esp_prov <- plots_IFN2_esp_prov[
  (plots_IFN2_esp_prov$AGE != 0),
]

plots_IFN3_esp_prov <- plots_IFN3_esp_prov[
  (plots_IFN3_esp_prov$AGE != 0),
]


### Remodelación de los códigos de parcela del IFN3
# Las parcelas del IFN3, en ocasiones, son movidas a nuevas localizaciones (ver documentador IFN3, anexo I)
# Para el IFN3, clasifico también por la subclase de parcela

plots_IFN3_esp_prov$PLOT_ID <- paste(plots_IFN3_esp_prov$PLOT_ID, plots_IFN3_esp_prov$Clase, plots_IFN3_esp_prov$Subclase, sep = '_')
IFN3_trees$PLOT_ID <- paste(IFN3_trees$PLOT_ID, IFN3_trees$Clase, IFN3_trees$Subclase, sep = '_')


# Extraigo los ID de parcelas para filtrar los árboles

IDs2_esp_prov <- plots_IFN2_esp_prov$PLOT_ID
IDs3_esp_prov <- plots_IFN3_esp_prov$PLOT_ID


### Seleccionar los árboles de las parcelas con las que vamos a trabajar, conociendo los códigos de las parcelas

# IFN2
trees_IFN2_esp_prov <- IFN2_trees[
  (IFN2_trees$PLOT_ID %in% IDs2_esp_prov),
]

# IFN3
trees_IFN3_esp_prov <- IFN3_trees[
  (IFN3_trees$PLOT_ID %in% IDs3_esp_prov),
]


# En el IFN3, eliminar los árboles que ya no están (muertos, cortados...), dado que tiene sentido simular su crecimiento

trees_IFN3_esp_prov_filtered <- trees_IFN3_esp_prov[(trees_IFN3_esp_prov$dbh != 0), ]
trees_IFN3_esp_prov_filtered <- trees_IFN3_esp_prov_filtered[!is.na(trees_IFN3_esp_prov_filtered$PLOT_ID), ]

# Adaptar los nombres de TREE_ID en el inventario de árboles del IFN3 (por defecto usa el nº del IFN2)

trees_IFN3_esp_prov_filtered$TREE_ID <- paste(trees_IFN3_esp_prov_filtered$INVENTORY_ID, trees_IFN3_esp_prov_filtered$Provincia, trees_IFN3_esp_prov_filtered$Estadillo, trees_IFN3_esp_prov_filtered$Clase, trees_IFN3_esp_prov_filtered$Subclase, trees_IFN3_esp_prov_filtered$TREE_ID_IFN3_2, sep = '_')


### exportar a .csv --> SIMANFOR
# En este caso, exportamos por separado los datos del IFN2 y del IFN3

# trees
write.csv(trees_IFN2_esp_prov, file = "./data/IFN/1_processed/inventory_1/trees_IFN2_IBEROPT.csv", fileEncoding = "UTF-8")
write.csv(trees_IFN3_esp_prov_filtered, file = "./data/IFN/1_processed/inventory_1/trees_IFN3_IBEROPT.CSV", fileEncoding = "UTF-8")

# plots
write.csv(plots_IFN2_esp_prov, file = "./data/IFN/1_processed/inventory_1/plots_IFN2_IBEROPT.csv", fileEncoding = "UTF-8")
write.csv(plots_IFN3_esp_prov, file = "./data/IFN/1_processed/inventory_1/plots_IFN3_IBEROPT.csv", fileEncoding = "UTF-8")
