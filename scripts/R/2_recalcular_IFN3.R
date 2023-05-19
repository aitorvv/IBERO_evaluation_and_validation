#------------------------------------------------------------------------------------#
#### Scripts para recalcular las parcelas del IFN3 a 10 años después del IFN2     ####
# Elaborado y reestructurado por Aitor Vázquez Veloso, 2021
# Última revisión: 19/05/2023
#------------------------------------------------------------------------------------#


# directorio
setwd("./github/")


#librer?as

#install.packages('plyr')
library(plyr)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readr")
library(readr)

#install.packages("openxlsx")
library(openxlsx)


# cargar los archivos iniciales (csv)

IFN2_plots <- read.csv("./data/IFN/1_processed/inventory_1/plots_IFN2_IBEROPT.csv", sep = ',')
IFN3_plots <- read.csv("./data/IFN/1_processed/inventory_1/plots_IFN3_IBEROPT.csv", sep = ',')

IFN2_trees <- read.csv("./data/IFN/1_processed/inventory_1/trees_IFN2_IBEROPT.csv", sep = ',')
IFN3_trees <- read.csv("./data/IFN/1_processed/inventory_1/trees_IFN3_IBEROPT.CSV", sep = ',')


#### Preparar datos ####

# copiar los datos de EXEC_TIME en el data.frame IFN3_trees

IFN3_EXEC <- IFN3_plots[ , c('PLOT_ID', 'EXEC_TIME')]
IFN3_EXEC <- subset(IFN3_EXEC, !duplicated(PLOT_ID))
IFN3_trees <- merge(IFN3_trees, IFN3_EXEC, by=c('PLOT_ID'))
#IFN3_trees_2 <- IFN3_trees %>% inner_join(IFN3_EXEC, by="PLOT_ID")


# seleccionar las parcelas que no hay que modificar y las que si

IFN3_final_plots <- IFN3_plots[
  (IFN3_plots$EXEC_TIME == 10),
]

IFN3_plots <- IFN3_plots[
  (IFN3_plots$EXEC_TIME != 10),
]


# obtener los IDs de las parcelas que vamos a modificar y las que no

IDs_IFN3_good <- IFN3_final_plots$PLOT_ID
IDs_IFN3_modify <- IFN3_plots$PLOT_ID


# seleccionar los ?rboles del IFN3 que hay que modificar y que no

IFN3_final_trees <- IFN3_trees[
  (IFN3_trees$PLOT_ID %in% IDs_IFN3_good),
]

IFN3_dead_trees <- IFN3_trees[(IFN3_trees$dbh == 0),]
IFN3_na_trees <- IFN3_trees[is.na(IFN3_trees$dbh),]

IFN3_final_trees <- rbind(IFN3_final_trees, IFN3_dead_trees, IFN3_na_trees)
IDs_IFN3_final_trees <- IFN3_final_trees$TREE_ID

IFN3_trees <- subset(IFN3_trees, !TREE_ID %in% IDs_IFN3_final_trees)
#IFN3_trees <- IFN3_trees[
#  (IFN3_trees$PLOT_ID %in% IDs_IFN3_modify),
#]


# uno los datos de un mismo ?rbol en el mismo archivo

IFN_joined_trees <- merge(IFN3_trees, IFN2_trees, by.x=c('PLOT_ID', 'TREE_ID_IFN2'), by.y=c('PLOT_ID', 'TREE_ID_IFN2'))


#### Reajuste variables dbh y h ####

# reajusto los valores de dbh y h de los ?rboles, simulando que para todos los casos han pasado 10 a?os entre inventarios

# h

IFN_joined_trees$diff_h <- IFN_joined_trees$height.x - IFN_joined_trees$height.y
IFN_joined_trees$ih_1 <- IFN_joined_trees$diff_h / IFN_joined_trees$EXEC_TIME 
IFN_joined_trees$height <- IFN_joined_trees$height.y + IFN_joined_trees$ih_1*10

# dbh

IFN_joined_trees$sub_dbh2 <- IFN_joined_trees$dbh.x^2 - IFN_joined_trees$dbh.y^2
IFN_joined_trees$idbh2_1 <- IFN_joined_trees$sub_dbh2 / (IFN_joined_trees$EXEC_TIME*10)
IFN_joined_trees$idbh_10 <- IFN_joined_trees$idbh2_1 * 10
IFN_joined_trees$dbh <- IFN_joined_trees$dbh.y + sqrt(IFN_joined_trees$idbh_10)


# extraer datos nuevos de ?rboles IFN3 y renombrar datos necesarios

IFN3_trees_10 <- subset(IFN_joined_trees, select = c(INVENTORY_ID.x, PLOT_ID, Estadillo.x, Clase, Subclase, TREE_ID_IFN2, TREE_ID_IFN3, TREE_ID_IFN3_2, TREE_ID.x, IFN3_ID, specie.x, dbh, height))
IFN3_trees_10 <- rename(IFN3_trees_10, c("INVENTORY_ID" = "INVENTORY_ID.x",
                                         "Estadillo" = "Estadillo.x",
                                         "TREE_ID" = "TREE_ID.x",
                                         "specie" = "specie.x"
))


# separar los ?rboles para los que no hay valor de dbh

#IFN3_trees_10_no_dbh <- IFN3_trees_10[is.na(IFN3_trees_10$dbh),] 
#IFN3_trees_10 <- IFN3_trees_10[!is.na(IFN3_trees_10$dbh),] 


# registramos las parcelas que tienen ?rboles ?tiles

IDs_IFN3_trees <- IFN3_trees_10$PLOT_ID


# c?lculo de expan usando el dbh de cada ?rbol

IFN3_trees_10$expan <- with(IFN3_trees_10, 
                            ifelse (dbh < 7.5, 0, 
                                    ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                           ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                                  ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                         10000/(pi*(25^2)))))))

#- calcular el ?rea basim?trica

IFN3_trees_10$basal_area <- ((pi)/4)*(IFN3_trees_10$dbh^2)


#### Recalcular parcelas ####

# Calcular dbh y h medios, G, N

plot3_calc <- ddply(IFN3_trees_10, c('IFN3_ID'), summarise, # aqu? paso a calcularlo con el IFN3_ID (clase + subclase)
                    SUM_DBH = sum(dbh*expan, na.rm = TRUE), ###
                    SUM_H = sum(height*expan, na.rm = TRUE), ###
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
#head(plot3_calc)

plot3_calc$MEAN_DBH <- plot3_calc$SUM_DBH/plot3_calc$DENSITY ###
plot3_calc$MEAN_H <- plot3_calc$SUM_H/plot3_calc$DENSITY ###


# calcular diametro medio cuadratico (dg)

plot3_calc$QM_DBH <- with(plot3_calc, 200*(BASAL_AREA/DENSITY/pi)^0.5, na.rm=TRUE)


# Calculo de la altura dominante (Ho)
# Se compone de dos funciones v?lidas para IFN2 e IFN3

# Funci?n 1

#IFN3
dominantHeight<-function(x, plotID="IFN3_ID") {
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


# Funci?n 2
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


# Ejecutar el c?lculo de Ho (m)

Ho_3 <- dominantHeight(IFN3_trees_10, 'IFN3_ID')


# A?adir el valor de Ho a la base de datos anterior

plot3_calc <- merge(plot3_calc, Ho_3, by.x=c('IFN3_ID'), by.y=c('IDs'))


# Renombrar Ho como DOMINANT_H --> SIMANFOR

plot3_calc <- rename(plot3_calc, c("DOMINANT_H" = "Ho"))


#### Unir datos ####

# elimino los datos anteriores de las variables que calculo

IFN3_plots <-  subset(IFN3_plots, select = -c(SUM_DBH, SUM_H, MEAN_DBH, MEAN_H, BASAL_AREA, DENSITY, QM_DBH, DOMINANT_H, G_sp, N_sp, sp_1, 
                                              sp_2, sp_3, G_sp_1, G_sp_2, G_sp_3, N_sp_1, N_sp_2, N_sp_3, CD_0_75, CD_75_125,
                                              CD_125_175, CD_175_225, CD_225_275, CD_275_325, CD_325_375, CD_375_425, CD_425_))


# a?ado los datos nuevos, extrapolados para ejecuciones de 10 a?os

IFN3_plots <- merge(plot3_calc, IFN3_plots, by.x=c('IFN3_ID'), by.y=c('IFN3_ID'))
# El IFN3 presenta una fila para cada especie de la parcela, entonces hay que eliminar duplicados de forma diferente
# Creo una variable nueva que une IFN3_ID con Especie
IFN3_plots$IFN3_specie <- paste(IFN3_plots$IFN3_ID, IFN3_plots$Especie, sep = '_')
# Elimino duplicados utilizando la variable nueva
IFN3_plots <- subset(IFN3_plots, !duplicated(IFN3_specie))


# juntamos las parcelas y ?rboles recalculadas/os con las que no lo necesitaban

IFN3_final_plots <- merge(IFN3_final_plots, IFN3_plots, all = TRUE)
IFN3_final_trees <- merge(IFN3_final_trees, IFN3_trees_10, all = TRUE)
#IFN3_final_trees <- merge(IFN3_final_trees, IFN3_trees_10_no_dbh, all = TRUE)  # a?ado tambi?n ?rboles con dbh = 0


IFN3_final_plots <- subset(IFN3_final_plots, !duplicated(IFN3_ID))
#IFN3_final_trees <- subset(IFN3_final_trees, !duplicated(TREE_ID))


#### Cálculos por especie ####

# Calculo G y N por especie

plot3_calc_sp <- ddply(IFN3_trees_10, c('PLOT_ID', 'specie'), summarise, ###
                       G_sp = sum(basal_area*expan, na.rm = TRUE), ###
                       N_sp = sum(expan, na.rm = TRUE) ###
)


# ordeno los datos por parcela y G

plot3_calc_sp <- plot3_calc_sp %>%
  arrange(PLOT_ID, -G_sp)
#plot3_calc_sp$sp_1 <- plot3_calc_sp$G_sp[1:,]


# ahora, lo que tengo que hacer son dos columnas, asignando G y N por cada especie (sp1, sp2...)

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

# elimino los duplicados y la informaci?n sobrante

plots_useful_IFN3$PLOT_ID_SPECIE <- paste(plots_useful_IFN3$PLOT_ID,plots_useful_IFN3$specie, sep = '_')
plots_useful_IFN3 <- plots_useful_IFN3[!duplicated(plots_useful_IFN3$PLOT_ID_SPECIE),]
plots_useful_IFN3 <-  subset(plots_useful_IFN3, select = -c(PLOT_ID_SPECIE))


# y a?ado la informaci?n de G y N de las especies principales a la base de datos

IFN3_final_plots <- merge(IFN3_final_plots, plots_useful_IFN3, all = TRUE, by.x=c('PLOT_ID'), by.y=c('PLOT_ID'))


# elimino duplicados de nuevo

IFN3_final_plots <- IFN3_final_plots[!duplicated(IFN3_final_plots$PLOT_ID),]


# elimino ?rboles con PLOT_ID = NA

IFN3_final_trees <- IFN3_final_trees[!is.na(IFN3_final_trees$PLOT_ID),]


# crear um identificador nuevo de ?rboles del IFN3 (SIMANFOR necesita que cada ?rbol tenga un identificador diferente)

IFN3_final_trees$TREE_ID_compare <- IFN3_final_trees$TREE_ID
IFN3_final_trees$TREE_ID <- paste(IFN3_final_trees$PLOT_ID, IFN3_final_trees$TREE_ID_IFN3_2, sep = "_")


# sustituir los valores importantes que lee SIMANFOR y tienen NA por 0

IFN3_final_trees$expan[is.na(IFN3_final_trees$expan)] <- 0
IFN3_final_trees$dbh[is.na(IFN3_final_trees$dbh)] <- 0
IFN3_final_trees$height[is.na(IFN3_final_trees$height)] <- 0
IFN3_final_trees$specie[is.na(IFN3_final_trees$specie)] <- 0

# Psylvestris SIM
#IFN3_final_plots <- IFN3_final_plots[IFN3_final_plots$PLOT_ID != '44_229',]
#IFN3_final_trees <- IFN3_final_trees[IFN3_final_trees$PLOT_ID != '44_229',]


#### Exportar resultados ####

### exportar a .csv

# trees
write.csv(IFN3_final_trees, file = "./data/IFN/1_processed/inventory_2/trees_IFN3_IBEROPS.csv", fileEncoding = "UTF-8")

# plots
write.csv(IFN3_final_plots, file = "./data/IFN/1_processed/inventory_2/plots_IFN3_IBEROPS.csv", fileEncoding = "UTF-8")

