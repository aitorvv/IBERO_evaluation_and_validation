#------------------------------------------------------------------------------------#
#### Scripts elaborados para adaptar el IFN2 e IFN3 a las necesidades de SIMANFOR ####
# Elaborado y reestructurado por Aitor Vázquez Veloso, 2021
# Última revisión: 19/05/2023
#------------------------------------------------------------------------------------#


#### Pasos básicos ####

# directorio
setwd("./github/")


# librerías
#install.packages('plyr')
library(plyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("taRifx")
library(taRifx)


# cargar los archivos iniciales (test data)

#IFN2 - Parcelas
IFN2_plots <- read.csv('./data/IFN/0_raw/test_IFN2_plots.csv', sep = ',')

#IFN3 - Parcelas, necesarias 2 bases de datos
#IFN3_plots_1 <- read.csv('pcespparc.csv', sep = ',')
#IFN3_plots_2 <- read.csv('pcparcelas.csv', sep = ',')
IFN3_plots <- read.csv('./data/IFN/0_raw/test_IFN3_plots.csv')

#IFN2 - árboles
IFN2_trees <- read.csv('./data/IFN/0_raw/test_IFN2_trees.csv', sep = ',')

#IFN3 - árboles
IFN3_trees <- read.csv('./data/IFN/0_raw/test_IFN3_trees.csv', sep = ',')


#### Renombrado de variables (done on test data) ####

# Renombrar variables árbol
IFN2_trees <- rename(IFN2_trees, c("Provincia" = "Origen",
                                   "TREE_ID_IFN2" = "NumOrden",
                                   "rumbo" = "Rumbo",
                                   "distancia" = "Distanci",
                                   "specie" = "Especie",
                                   "dbh_1" = "Diametro1",
                                   "dbh_2" = "Diametro2",
                                   "height" = "Altura",
                                   "quality" = "Calidad",
                                   "shape" = "Forma",
                                   "special_param" = "ParamEsp"
))

IFN3_trees <- rename(IFN3_trees, c("Provincia" = "Origen",
                                   "Clase" = "Cla",
                                   "TREE_ID_IFN3_2" = "nArbol",
                                   "TREE_ID_IFN3" = "OrdenIf3",
                                   "TREE_ID_IFN2" = "OrdenIf2",
                                   "rumbo" = "Rumbo",
                                   "distancia" = "Distanci",
                                   "specie" = "Especie",
                                   "dbh_1" = "Dn1",
                                   "dbh_2" = "Dn2",
                                   "height" = "Ht",
                                   "quality" = "Calidad",
                                   "shape" = "Forma",
                                   "special_param" = "ParEsp"
))

# Renombrar variables parcela
IFN2_plots <- rename(IFN2_plots, c("Provincia" = "PROVINCIA",
                                   "Estadillo" = "ESTADILLO"
))

IFN3_plots_1 <- rename(IFN3_plots_1, c("Provincia" = "Origen",
                                       "Estadillo" = "Estadillo"
))

IFN3_plots_2 <- rename(IFN3_plots_2, c("Estadillo" = "Estadillo"
))         


#### Crear identificadores (done on test data) ####

# crear INVENTORY_ID 

IFN2_plots$INVENTORY_ID <- 'IFN2'
IFN3_plots_1$INVENTORY_ID <- 'IFN3'
IFN3_plots_2$INVENTORY_ID <- 'IFN3'

IFN2_trees$INVENTORY_ID <- 'IFN2'
IFN3_trees$INVENTORY_ID <- 'IFN3'


# Crear una columna para identificar la parcela - PLOT_ID - utilizando provincia y estadillo

IFN2_plots$PLOT_ID <- paste(IFN2_plots$Provincia, IFN2_plots$Estadillo, sep = '_')
IFN3_plots_1$PLOT_ID <- paste(IFN3_plots_1$Provincia, IFN3_plots_1$Estadillo, sep = '_')
IFN3_plots_2$PLOT_ID <- paste(IFN3_plots_2$Provincia, IFN3_plots_2$Estadillo, sep = '_')

IFN2_trees$PLOT_ID <- paste(IFN2_trees$Provincia, IFN2_trees$Estadillo, sep = '_')
IFN3_trees$PLOT_ID <- paste(IFN3_trees$Provincia, IFN3_trees$Estadillo, sep = '_')


# Crear TREE_ID como la suma de provincia, estadillo y nº de árbol correspondiente al IFN2

IFN2_trees$TREE_ID <- paste(IFN2_trees$Provincia, IFN2_trees$Estadillo, IFN2_trees$TREE_ID_IFN2, sep = '_')
IFN3_trees$TREE_ID <- paste(IFN3_trees$Provincia, IFN3_trees$Estadillo, IFN3_trees$TREE_ID_IFN2, sep = '_')


# Crear IFN_TREE_ID para poder localizar si los datos a utilizar son del 2º o 3er IFN

IFN2_trees$IFN_TREE_ID <- paste(IFN2_trees$INVENTORY_ID, IFN2_trees$Provincia, IFN2_trees$Estadillo, IFN2_trees$TREE_ID_IFN2, sep = '_')
IFN3_trees$IFN_TREE_ID <- paste(IFN3_trees$INVENTORY_ID, IFN3_trees$Provincia, IFN3_trees$Estadillo, IFN3_trees$TREE_ID_IFN2, sep = '_')


# ID especial para unir IFN3 data

IFN3_plots_1$IFN3_ID <- paste(IFN3_plots_1$PLOT_ID, IFN3_plots_1$Cla, IFN3_plots_1$Subclase, sep = '_')
IFN3_plots_2$IFN3_ID <- paste(IFN3_plots_2$PLOT_ID, IFN3_plots_2$Cla, IFN3_plots_2$Subclase, sep = '_')

IFN3_trees$IFN3_ID <- paste(IFN3_trees$PLOT_ID, IFN3_trees$Clase, IFN3_trees$Subclase, sep = '_')


# Unir datos parcela IFN3

IFN3_plots <- merge(IFN3_plots_1, IFN3_plots_2, by.x=c('IFN3_ID'), by.y=c('IFN3_ID'))


#### Correcciones de datos IFN2 ####

# Correcciones de alturas del IFN2:

# hemos detectado que los datos de las alturas del IFN2 tienen como separadores de decimales tanto . como ,
# adem?s de unificar el criterio, se transforma la columna a num?rica para poder operar con sus valores

IFN2_trees <- IFN2_trees %>% 
  mutate(height = str_replace(height, ",", "."))

IFN2_trees$height <- destring(IFN2_trees$height)


#### Cálculo variables árbol ####

# Cálculo de diámetro medio (cm)

IFN2_trees$dbh <- (IFN2_trees$dbh_1 + IFN2_trees$dbh_2)/20
IFN3_trees$dbh <- (IFN3_trees$dbh_1 + IFN3_trees$dbh_2)/20


# cálculo de expan usando el dbh de cada árbol

IFN2_trees$expan <- with(IFN2_trees, 
                         ifelse (dbh < 7.5, 0, 
                                 ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                        ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                               ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                      10000/(pi*(25^2)))))))
IFN3_trees$expan <- with(IFN3_trees, 
                         ifelse (dbh < 7.5, 0, 
                                 ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                        ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                               ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                      10000/(pi*(25^2)))))))


#- calcular el área basimétrica

IFN2_trees$basal_area <- ((pi)/4)*(IFN2_trees$dbh^2)
IFN3_trees$basal_area <- ((pi)/4)*(IFN3_trees$dbh^2)


# eliminar árboles con dbh y h = NA, y eliminar tambi?n los ?rboles que tienen identificadores especiales en el IFN3
# que indican alg?n tipo de incidencia (ver documentador), usando una BD temporal para hacer los c?lculos de parcelas con ellos

IFN3_trees_temp <- IFN3_trees[!is.na(IFN3_trees$dbh), ]

delete_trees <- c(0, 444, 666, 777, 888, 999)
IFN3_trees_temp <- IFN3_trees_temp[!IFN3_trees_temp$TREE_ID_IFN3 %in% delete_trees, ]


#### Cálculo variables parcela ####

# Calcular dbh y h medios, G, N, N por CD

plot2_calc <- ddply(IFN2_trees, c('PLOT_ID'), summarise,
                    SUM_DBH = sum(dbh*expan, na.rm = TRUE), ###
                    SUM_H = sum(height*expan, na.rm = TRUE), ###
                    BASAL_AREA  = sum(basal_area*expan/10000, na.rm = TRUE), ###
                    DENSITY = sum(expan, na.rm = TRUE),                
                    CD_0_75 = sum(ifelse(dbh <= 7.5, expan, 0), na.rm = TRUE),
                    CD_75_125 = sum(ifelse(dbh > 7.5 & dbh <= 12.5, expan, 0), na.rm = TRUE),
                    CD_125_175 = sum(ifelse(dbh > 12.5 & dbh <= 17.5, expan, 0), na.rm = TRUE),
                    CD_175_225 = sum(ifelse(dbh > 17.5 & dbh <= 22.5, expan, 0), na.rm = TRUE),
                    CD_225_275 = sum(ifelse(dbh > 22.5 & dbh <= 27.5, expan, 0), na.rm = TRUE),
                    CD_275_325 = sum(ifelse(dbh > 27.5 & dbh <= 32.5, expan, 0), na.rm = TRUE),
                    CD_325_375 = sum(ifelse(dbh > 32.5 & dbh <= 37.5, expan, 0), na.rm = TRUE),
                    CD_375_425 = sum(ifelse(dbh > 37.5 & dbh <= 42.5, expan, 0), na.rm = TRUE),
                    CD_425_ = sum(ifelse(dbh > 42.5, expan, 0), na.rm = TRUE)
)

plot2_calc$MEAN_DBH <- plot2_calc$SUM_DBH/plot2_calc$DENSITY ###
plot2_calc$MEAN_H <- plot2_calc$SUM_H/plot2_calc$DENSITY ###


plot3_calc <- ddply(IFN3_trees_temp, c('IFN3_ID'), summarise, # aqu? paso a calcularlo con el IFN3_ID (clase + subclase)
                    SUM_DBH = sum(dbh*expan, na.rm = TRUE),
                    SUM_H = sum(height*expan, na.rm = TRUE),
                    BASAL_AREA  = sum(basal_area*expan/10000, na.rm = TRUE),
                    DENSITY = sum(expan, na.rm = TRUE),
                    CD_0_75 = sum(ifelse(dbh <= 7.5, expan, 0), na.rm = TRUE),
                    CD_75_125 = sum(ifelse(dbh > 7.5 & dbh <= 12.5, expan, 0), na.rm = TRUE),
                    CD_125_175 = sum(ifelse(dbh > 12.5 & dbh <= 17.5, expan, 0), na.rm = TRUE),
                    CD_175_225 = sum(ifelse(dbh > 17.5 & dbh <= 22.5, expan, 0), na.rm = TRUE),
                    CD_225_275 = sum(ifelse(dbh > 22.5 & dbh <= 27.5, expan, 0), na.rm = TRUE),
                    CD_275_325 = sum(ifelse(dbh > 27.5 & dbh <= 32.5, expan, 0), na.rm = TRUE),
                    CD_325_375 = sum(ifelse(dbh > 32.5 & dbh <= 37.5, expan, 0), na.rm = TRUE),
                    CD_375_425 = sum(ifelse(dbh > 37.5 & dbh <= 42.5, expan, 0), na.rm = TRUE),
                    CD_425_ = sum(ifelse(dbh > 42.5, expan, 0), na.rm = TRUE)               
)

plot3_calc$MEAN_DBH <- plot3_calc$SUM_DBH/plot3_calc$DENSITY
plot3_calc$MEAN_H <- plot3_calc$SUM_H/plot3_calc$DENSITY


# calcular diametro medio cuadratico (dg)

plot2_calc$QM_DBH <- with(plot2_calc, 200*(BASAL_AREA/DENSITY/pi)^0.5, na.rm=TRUE)
plot3_calc$QM_DBH <- with(plot3_calc, 200*(BASAL_AREA/DENSITY/pi)^0.5, na.rm=TRUE)


# En el IFN3 hay que eliminar para este c?lculo las parcelas con DENSITY, MEAN_DBH y MEAN_H = 0 (IFN2 no tiene)

plot3_calc <- plot3_calc[plot3_calc$MEAN_DBH != 0, ] 
plot3_calc <- plot3_calc[!is.na(plot3_calc$MEAN_DBH),] 


# Calculo de la altura dominante (Ho)
# Se compone de dos funciones válidas para IFN2 e IFN3

# Función 1
dominantHeight<-function(x, plotID="PLOT_ID") {
  if(plotID %in% names(x)) {
    IDs = unique(x[[plotID]])
    Ho = rep(NA, length(IDs))
    names(Ho) = IDs
    for(i in 1:length(IDs)) {
      Ho[i] = .domheight(x$height[x[[plotID]] ==IDs[i]],
                         x$dbh[x[[plotID]]  ==IDs[i]],
                         x$expan[x[[plotID]]  ==IDs[i]])
    }
    Hd <- data.frame(IDs, Ho)
    return(Hd)
  }
  return(.domheight(x$h, x$d, x$n))
}

# Función 2
.domheight<-function(h, d, n) {
  o <-order(d, decreasing=TRUE)
  h = h[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(h)) {
    ncum = ncum + n[i]
    if(ncum>100) return(sum(h[1:i]*n[1:i], na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], na.rm=TRUE))
  }
  return(sum(h*n)/sum(n))
}

#IFN2
Ho_2 <- dominantHeight(IFN2_trees, 'PLOT_ID')

#IFN3
Ho_3 <- dominantHeight(IFN3_trees_temp, 'IFN3_ID') # IFN3_ID

# Añadir el valor de Ho a la base de datos anterior

plot2_calc <- merge(plot2_calc, Ho_2, by.x=c('PLOT_ID'), by.y=c('IDs'))
#IFN3 necesita, además, las parcelas para las que no se calculó el Ho
plot3_calc <- merge(plot3_calc, Ho_3, all = TRUE, by.x=c('IFN3_ID'), by.y=c('IDs')) #IFN3_ID


# Renombrar Ho como DOMINANT_H --> SIMANFOR

plot2_calc <- rename(plot2_calc, c("DOMINANT_H" = "Ho"))
plot3_calc <- rename(plot3_calc, c("DOMINANT_H" = "Ho"))


# unir los datos del cálculo de variables de parcela a la base de datos original

IFN2_plots<- merge(plot2_calc, IFN2_plots, all = TRUE, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))
IFN3_plots<- merge(plot3_calc, IFN3_plots, all = TRUE, by.x=c('IFN3_ID'), by.y=c('IFN3_ID')) #IFN3_ID


#### Cálculos por especie ####

# Calculo G y N por especie

plot2_calc_sp <- ddply(IFN2_trees, c('PLOT_ID', 'specie'), summarise, ###
                       G_sp = sum(basal_area*expan, na.rm = TRUE), ###
                       N_sp = sum(expan, na.rm = TRUE) ###
)

plot3_calc_sp <- ddply(IFN3_trees_temp, c('PLOT_ID', 'specie'), summarise, ###
                       G_sp = sum(basal_area*expan, na.rm = TRUE), ###
                       N_sp = sum(expan, na.rm = TRUE) ###
)


# ordeno los datos por parcela y G

plot2_calc_sp <- plot2_calc_sp %>%
  arrange(PLOT_ID, -G_sp)
#plot2_calc_sp$sp_1 <- plot2_calc_sp$G_sp[1:,]

plot3_calc_sp <- plot3_calc_sp %>%
  arrange(PLOT_ID, -G_sp)
#plot3_calc_sp$sp_1 <- plot3_calc_sp$G_sp[1:,]


# ahora, lo que tengo que hacer son dos columnas, asignando G y N por cada especie (sp1, sp2...)

plots_useful_IFN2 <- data.frame()
for (k in plot2_calc_sp$PLOT_ID) {
  
  plots_k <- plot2_calc_sp[plot2_calc_sp$PLOT_ID %in% k,]
  
  plots_k$sp_1 <- plots_k$specie[1]
  plots_k$sp_2 <- plots_k$specie[2] 
  plots_k$sp_3 <- plots_k$specie[3] 
  plots_k$G_sp_1 <- plots_k$G_sp[1]
  plots_k$G_sp_2 <- plots_k$G_sp[2]
  plots_k$G_sp_3 <- plots_k$G_sp[3]
  plots_k$N_sp_1 <- plots_k$N_sp[1]
  plots_k$N_sp_2 <- plots_k$N_sp[2]
  plots_k$N_sp_3 <- plots_k$N_sp[3]
  
  plots_useful_IFN2 <- rbind(plots_useful_IFN2, plots_k)
}

plots_useful_IFN3 <- data.frame()
for (k in plot3_calc_sp$PLOT_ID) {
  
  plots_k <- plot3_calc_sp[plot3_calc_sp$PLOT_ID %in% k,]
  
  plots_k$sp_1 <- plots_k$specie[1]
  plots_k$sp_2 <- plots_k$specie[2] 
  plots_k$sp_3 <- plots_k$specie[3] 
  plots_k$G_sp_1 <- plots_k$G_sp[1]
  plots_k$G_sp_2 <- plots_k$G_sp[2]
  plots_k$G_sp_3 <- plots_k$G_sp[3]
  plots_k$N_sp_1 <- plots_k$N_sp[1]
  plots_k$N_sp_2 <- plots_k$N_sp[2]
  plots_k$N_sp_3 <- plots_k$N_sp[3]
  
  plots_useful_IFN3 <- rbind(plots_useful_IFN3, plots_k)
}

# elimino los duplicados y la información sobrante

plots_useful_IFN2$PLOT_ID_SPECIE <- paste(plots_useful_IFN2$PLOT_ID,plots_useful_IFN2$specie, sep = '_')
plots_useful_IFN2 <- plots_useful_IFN2[!duplicated(plots_useful_IFN2$PLOT_ID_SPECIE),]
plots_useful_IFN2 <-  subset(plots_useful_IFN2, select = -c(PLOT_ID_SPECIE))

plots_useful_IFN3$PLOT_ID_SPECIE <- paste(plots_useful_IFN3$PLOT_ID,plots_useful_IFN3$specie, sep = '_')
plots_useful_IFN3 <- plots_useful_IFN3[!duplicated(plots_useful_IFN3$PLOT_ID_SPECIE),]
plots_useful_IFN3 <-  subset(plots_useful_IFN3, select = -c(PLOT_ID_SPECIE))


# y añado la información de G y N de las especies principales a la base de datos

IFN2_plots<- merge(plots_useful_IFN2, IFN2_plots, all = TRUE, by.x=c('PLOT_ID', 'specie'), by.y=c('PLOT_ID', 'ESPECIA11'))

IFN3_plots<- merge(plots_useful_IFN3, IFN3_plots, all = TRUE, by.x=c('PLOT_ID', 'specie'), by.y=c('PLOT_ID.y', 'Especie'))


#### Exportar los datos procesados - checkpoint 1 ####

### exportar a .csv

#IFN2
write.csv(IFN2_plots, file = "./data/IFN/1_processed/checkpoint_1/IFN2_plots-cp1.csv", fileEncoding = "UTF-8")
write.csv(IFN2_trees, file = "./data/IFN/1_processed/checkpoint_1/IFN2_trees-cp1.csv", fileEncoding = "UTF-8")

#IFN3
write.csv(IFN3_plots, file = "./data/IFN/1_processed/checkpoint_1/IFN3_plots-cp1.csv", fileEncoding = "UTF-8")
write.csv(IFN3_trees, file = "./data/IFN/1_processed/checkpoint_1/IFN3_trees-cp1.csv", fileEncoding = "UTF-8")


#### Adaptar datos ####

# eliminar archivos temporales
rm(Ho_2, Ho_3, IFN3_trees_temp, plot2_calc, plot2_calc_sp, plot3_calc, plot3_calc_sp,
   plots_k, plots_useful_IFN2, plots_useful_IFN3, delete_trees, k, dominantHeight)

# Eliminar las parcelas que no tienen datos en el IFN2

IFN2_plots_compare <- IFN2_plots[!is.na(IFN2_plots$MEAN_DBH),]  

# eliminar columnas duplicadas y renombrar

IFN3_plots <- subset(IFN3_plots, select = -c(Provincia.x, Provincia.y, Estadillo.y, Cla.y, Subclase.y, INVENTORY_ID.y))

IFN3_plots <- rename(IFN3_plots, c("Provincia" = "Origen",
                                   "Estadillo" = "Estadillo.x",
                                   "Clase" = "Cla.x",
                                   "Subclase" = "Subclase.x",
                                   "INVENTORY_ID" = "INVENTORY_ID.x",
                                   "Estado_pcespparc" = "Estado.x", # perteneciente al archivo pcespparc
                                   "Estado_pcparcelas" = "Estado.y", # perteneciente al archivo pcparcelas                                   
))


# crear variable ?til para filtrar las parcelas del IFN3 que se pueden comparar con el IFN2

IFN3_plots$MASTER_CLASE <- paste(IFN3_plots$Clase, IFN3_plots$Subclase, sep = '')


# En el IFN3 hay que eliminar para este c?lculo las parcelas con DENSITY, MEAN_DBH y MEAN_H = 0

IFN3_plots <- IFN3_plots[IFN3_plots$MEAN_DBH != 0, ] 
IFN3_plots <- IFN3_plots[!is.na(IFN3_plots$MEAN_DBH),] 

# seleccionar parcelas ?tiles para comparar con IFN2
# Clase + Subclase = A1, A3C, A4C, A6C

IFN3_plots_compare <- IFN3_plots[(
  (IFN3_plots$MASTER_CLASE == 'A1') | 
    (IFN3_plots$MASTER_CLASE == 'A3C') | 
    (IFN3_plots$MASTER_CLASE == 'A4C') | 
    (IFN3_plots$MASTER_CLASE == 'A6C')),
]


#### Unir parcelas comparables del IFN2 e IFN3 en un mismo archivo ####

# las variables __.x son del IFN2, y las variables __.y del IFN3

plots_IFN <- merge(IFN2_plots_compare, IFN3_plots_compare, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))


# Adecuar las variables: eliminar duplicadas y renombrar las que utilizaremos

plots_IFN <- subset(plots_IFN, select = -c(Provincia.y, Estadillo.y))

plots_IFN <- rename(plots_IFN, c("Provincia" = "Provincia.x",
                                 "Estadillo" = "Estadillo.x",
                                 "MEAN_DBH" = "MEAN_DBH.x",
                                 "MEAN_DBH_IFN3" = "MEAN_DBH.y",
                                 "MEAN_H" = "MEAN_H.x",
                                 "MEAN_H_IFN3" = "MEAN_H.y",
                                 "BASAL_AREA" = "BASAL_AREA.x",
                                 "BASAL_AREA_IFN3" = "BASAL_AREA.y",                                 
                                 "DENSITY" = "DENSITY.x",
                                 "DENSITY_IFN3" = "DENSITY.y",
                                 "QM_DBH" = "QM_DBH.x",                                 
                                 "QM_DBH_IFN3" = "QM_DBH.y",                                 
                                 "DOMINANT_H" = "DOMINANT_H.x",                                 
                                 "DOMINANT_H_IFN3" = "DOMINANT_H.y",                                 
                                 "MUNICIPALITY" = "MUNICI",                                  
                                 #"SPECIE_IFN_ID" = "ESPECIE1",  # ahora uso las variables provenientes de ?rboles
                                 "AGE" = "EDADB1", 
))


# Establezco la especie principal como la que tiene una mayor ?rea basim?trica (calculado en el apartado anterior)

plots_IFN$SPECIE_IFN_ID <- plots_IFN$sp_1.x


# Sin unir los inventarios IFN2 e IFN3, adecuar las variables de cada uno

IFN2_plots_compare <- rename(IFN2_plots_compare, c("MUNICIPALITY" = "MUNICI",                                  
                                                   "AGE" = "EDADB1"
)
)


#### Completar variable de edad ####

# Asignar edad de parcela a aquellas que no la tienen en el IFN2 pero s? en el IFN3
# Utilizamos la edad del IFN3 y la diferencia de a?os de muestreo...
# La idea era utilizar los meses para tener en cuenta el periodo de crecimiento, pero los datos no son correctos
#   ni siguen un orden claro en los campos de mes y d?a, lo que dificulta mucho precisar tanto en el c?lculo


### Extraer el a?o del IFN2
plots_IFN$Fecha_IFN2 <- paste('19', plots_IFN$FECHGR, sep = '') 
plots_IFN$Anho_IFN2 <- substring(plots_IFN$Fecha_IFN2, 0, 4)
plots_IFN$Anho_IFN2 <- as.numeric(plots_IFN$Anho_IFN2)

# Limpiar las parcelas con dato de Fecha vac?o
plots_IFN_clean <- plots_IFN[plots_IFN$Fecha_IFN2 != '19NA', ] 


### Extraer el a?o del IFN3
# Pasar FechaIFN3 a una variable diferente
plots_IFN_clean$Fecha_IFN3 <- as.Date(plots_IFN_clean$FechaIni, format = '%m/%d/%Y') # as? se ordenan: a?o/mes/d?a

# Extraigo el a?o en que se mide cada IFN
plots_IFN_clean$Anho_IFN3 <- substring(plots_IFN_clean$Fecha_IFN3, 0, 4)
plots_IFN_clean$Anho_IFN3 <- as.numeric(plots_IFN_clean$Anho_IFN3)

# Transformo IFN3 al a?o que corresponde
under_2000 <- c(95, 96, 97, 98, 99)
plots_IFN_clean$Anho_IFN3 <- ifelse(plots_IFN_clean$Anho_IFN3 %in% under_2000, 
                                    1900 + plots_IFN_clean$Anho_IFN3,
                                    2000 + plots_IFN_clean$Anho_IFN3)


### Relleno las columnas de edad de parcela del IFN2 vac?as con la edad del IFN3 menos el tiempo transcurrido entre mediciones

# Elimino las columnas que no tienen edad de la masa en el IFN3
plots_IFN_clean_2 <- filter(plots_IFN_clean, (!is.na(plots_IFN_clean$AGE) | !is.na(plots_IFN_clean$Edad)))

# crear columna con tiempo de ejecuci?n (tiempo transcurrido entre mediciones del IFN de una misma parcela)
plots_IFN_clean_2$EXEC_TIME <- plots_IFN_clean_2$Anho_IFN3 - plots_IFN_clean_2$Anho_IFN2

# Calculo la edad de la masa del IFN2 para aquellos casos en los que:
#   - su edad est? vac?a
#   - la edad en el IFN3 es mayor o igual al tiempo transcurrido entre inventarios (elimino inventarios nuevos)
plots_IFN_clean_2$AGE <- with(plots_IFN_clean_2, ifelse((is.na(AGE) & (Edad >= EXEC_TIME)),
                                                        Edad - EXEC_TIME,
                                                        AGE
) # ifelse
) # with

# extraigo las parcelas v?lidas para el simulador
plots_IFN_useful <- filter(plots_IFN_clean_2, !is.na(plots_IFN_clean_2$AGE))

# Hago una lista con los IDs de las parcelas que van a ser ?tiles para SIMANFOR (tienen valor de edad de la masa)

useful_IDs <- plots_IFN_useful$PLOT_ID


# Recojo la info calculada en el script 1_05, separando ambos inventarios

info_IFN2 <- select(plots_IFN_useful, PLOT_ID, 
                    AGE, 
                    MUNICIPALITY,
                    SPECIE_IFN_ID,
                    Fecha_IFN2,
                    Anho_IFN2,
                    EXEC_TIME
)

info_IFN3 <- select(plots_IFN_useful, PLOT_ID, 
                    AGE,
                    MUNICIPALITY,
                    SPECIE_IFN_ID,
                    Fecha_IFN3,
                    Anho_IFN3,
                    EXEC_TIME
)


# Recalculo la edad de la parcela del IFN3

info_IFN3$AGE <- (info_IFN3$AGE + info_IFN3$EXEC_TIME)

# Filtro cada IFN con los PLOT_ID que voy a utilizar

IFN2_plots_filtered <- filter(IFN2_plots, PLOT_ID %in% useful_IDs)
IFN3_plots_filtered <- filter(IFN3_plots, PLOT_ID %in% useful_IDs)


# Copio los datos extraidos a cada uno de ellos por separado

IFN2_plots_useful <- merge(IFN2_plots_filtered, info_IFN2, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))
IFN3_plots_useful <- merge(IFN3_plots_filtered, info_IFN3, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))


### Elimino los datos duplicados

# IFN2
IFN2_plots_useful <- subset(IFN2_plots_useful, !duplicated(PLOT_ID))

# El IFN3 presenta una fila para cada especie de la parcela, entonces hay que eliminar duplicados de forma diferente
# Creo una variable nueva que une IFN3_ID con Especie
IFN3_plots_useful$IFN3_specie <- paste(IFN3_plots_useful$IFN3_ID, IFN3_plots_useful$Especie, sep = '_')
# Elimino duplicados utilizando la variable nueva
IFN3_plots_useful <- subset(IFN3_plots_useful, !duplicated(IFN3_specie))
# Elimino esa variable temporal
IFN3_plots_useful <- subset(IFN3_plots_useful, select = -c(IFN3_specie))


#### Exportamos datos - checkpoint 2 ####

write.csv(IFN2_plots_useful, './data/IFN/1_processed/checkpoint_2/IFN2_plots-cp2.csv')
write.csv(IFN3_plots_useful, './data/IFN/1_processed/checkpoint_2/IFN3_plots-cp2.csv')
