                          
                          ##  UNIVERSIDAD INTERNACIONAL DE VALENCIA ##
                                ##  Trabajo Final de Máster ##

# Elaborado por: Jonathan Aquiles Túquerres Mendoza


# Instalar paquetes

#install.packages("dplyr")
#install.packages("plyr")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("ggplot2")
# Cargar el paquete dplyr
library(dplyr)
library(plyr)
library(arules)
library(arulesViz)
library(ggplot2)
library(readr)

R.version

                                  ##SIN PERSONALIZACIÓN##
# Leer data set limpio
dataNP <- read_csv("TFM/DATOS_NUEVOS/sin_personalizacion/sin_personalizacion_noche.csv", 
                  col_types = cols(Pedido = col_character(), 
                  Cant. = col_number(), Total = col_number(), 
                  Fecha = col_date(format = "%Y-%d-%m"), 
                  Hora = col_time(format = "%H:%M")))

# Filtro MENU/BEBIDAS
dataNP <- dataNP %>% filter(!`Sub Grupo` %in% c('MENU'))

# Preprocesado de la data
datos_Apriori <- dataNP %>% mutate(`Producto` = as.factor(`Producto`))
pedidos <- ddply(datos_Apriori,c("Pedido"), function(df) paste(df$`Producto`, collapse = ","))

write.csv(pedidos,"pedidos.csv", quote = FALSE, row.names = FALSE)
ped <- read.transactions("pedidos.csv", format = 'basket', sep=',', header = TRUE)

# Definición de parámetros REGLAS
param_reglas = list(
  supp = 8/dim(ped)[1],
  conf = 0.3,
  target = "rules")

reglas_asociacion <- apriori(ped, parameter = param_reglas)

# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' es 'PARRILLA'
antecedentes_parrillaMENU <- unique(subset(dataNP, `Sub Grupo` == 'PARRILLA')$`Producto`)
# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' no es 'PARRILLA'
consecuentes_menuMENU <- unique(subset(dataNP, `Sub Grupo` != 'PARRILLA')$`Producto`)

# Filtro reglas requeridas
filtered_rulesMENU <- subset(reglas_asociacion, 
                             subset = lhs %in% antecedentes_parrillaMENU & 
                               !(lhs %in% consecuentes_menuMENU) & 
                               rhs %in% consecuentes_menuMENU)

##reglas_asociacion <- sort(reglas_asociacion, by = "confidence", decreasing = TRUE)
duplicated(filtered_rulesMENU)
reglas_redundantes <- is.redundant(filtered_rulesMENU)
which(is.redundant(filtered_rulesMENU))
reglas_podadas <- filtered_rulesMENU[!reglas_redundantes]

reglas_ordenadas <- reglas_podadas[order(reglas_podadas@quality$lift, decreasing = TRUE),]
top_10_reglas <- head(reglas_ordenadas, 10)
print(top_10_reglas)
inspectDT(top_10_reglas)

plot(top_10_reglas, method = "graph",  engine = "htmlwidget", shading = "confidence")
productos_frecuentes <- data.frame(Productos = labels(reglas_podadas), 
                                   Indicador = reglas_podadas@quality)

write.csv(productos_frecuentes, "TFM/REGLAS/REGLAS_NUEVAS/sin_personalizacion/reglas_BEBIDAS_mediatarde.csv")







                                    ##PERSONALIZACIÓN 1##
# Leer data set limpio
dataNP1 <- read_csv("TFM/DATOS_NUEVOS/personalizacion_1/personalizacion_1_noche.csv", 
                   col_types = cols(Pedido = col_character(), 
                                    Cant. = col_number(), Total = col_number(), 
                                    Fecha = col_date(format = "%Y-%d-%m"), 
                                    Hora = col_time(format = "%H:%M")))

# Filtro MENU/BEBIDAS
dataNP1 <- dataNP1 %>% filter(!`Sub Grupo` %in% c('MENU'))

# Preprocesado de la data
datos_Apriori1 <- dataNP1 %>% mutate(`Producto` = as.factor(`Producto`))
pedidos1 <- ddply(datos_Apriori1,c("Pedido"), function(df1) paste(df1$`Producto`, collapse = ","))

write.csv(pedidos1,"pedidos1.csv", quote = FALSE, row.names = FALSE)
ped1 <- read.transactions("pedidos1.csv", format = 'basket', sep=',', header = TRUE)

# Definición de parámetros REGLAS
param_reglas1 = list(
  supp = 8/dim(ped1)[1],
  conf = 0.3,
  target = "rules")

#Reglas se asociación
reglas_asociacion1 <- apriori(ped1, parameter = param_reglas1)

# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' es 'PARRILLA'
antecedentes_parrillaMENU1 <- unique(subset(dataNP1, `Sub Grupo` == 'PARRILLA')$`Producto`)
# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' no es 'PARRILLA'
consecuentes_menuMENU1 <- unique(subset(dataNP1, `Sub Grupo` != 'PARRILLA')$`Producto`)

# Filtro reglas requeridas
filtered_rulesMENU1 <- subset(reglas_asociacion1, 
                             subset = lhs %in% antecedentes_parrillaMENU1 & 
                               !(lhs %in% consecuentes_menuMENU1) & 
                               rhs %in% consecuentes_menuMENU1)

##reglas_asociacion <- sort(reglas_asociacion, by = "confidence", decreasing = TRUE)
duplicated(filtered_rulesMENU1)
reglas_redundantes1 <- is.redundant(filtered_rulesMENU1)
which(is.redundant(filtered_rulesMENU1))
reglas_podadas1 <- filtered_rulesMENU1[!reglas_redundantes1]
summary(reglas_podadas1)

reglas_ordenadas1 <- reglas_podadas1[order(reglas_podadas1@quality$lift, decreasing = TRUE),]
top_10_reglas1 <- head(reglas_ordenadas1, 10)
print(top_10_reglas1)
inspectDT(top_10_reglas1)
summary(top_10_reglas1)

plot(top_10_reglas1, method = "graph",  engine = "htmlwidget", shading = "confidence")
productos_frecuentes1 <- data.frame(Productos = labels(reglas_podadas1), 
                                   Indicador = reglas_podadas1@quality)

write.csv(productos_frecuentes1, "TFM/REGLAS/REGLAS_NUEVAS/personalizacion_1/reglas_BEBIDAS_1_tardeMENZALVO.csv")






                                      ## PERSONALIZACIÓN 2 ##

# Leer data set limpio
dataNP2 <- read_csv("TFM/DATOS_NUEVOS/personalizacion_2/personalizacion_2_noche.csv", 
                    col_types = cols(Pedido = col_character(), 
                                     Cant. = col_number(), Total = col_number(), 
                                     Fecha = col_date(format = "%Y-%d-%m"), 
                                     Hora = col_time(format = "%H:%M")))

# Filtro MENU/BEBIDAS
dataNP2 <- dataNP2 %>% filter(!`Sub Grupo` %in% c('MENU'))

# Preprocesado de la data
datos_Apriori2 <- dataNP2 %>% mutate(`Producto` = as.factor(`Producto`))
pedidos2 <- ddply(datos_Apriori2,c("Pedido"), function(df2) paste(df2$`Producto`, collapse = ","))

write.csv(pedidos2,"pedidos2.csv", quote = FALSE, row.names = FALSE)
ped2 <- read.transactions("pedidos2.csv", format = 'basket', sep=',', header = TRUE)

# Definición de parámetros REGLAS
param_reglas2 = list(
  supp = 8/dim(ped2)[1],
  conf = 0.3,
  target = "rules")

#Reglas se asociación
reglas_asociacion2 <- apriori(ped2, parameter = param_reglas2)

# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' es 'PARRILLA'
antecedentes_parrillaMENU2 <- unique(subset(dataNP2, `Sub Grupo` == 'PARRILLA')$`Producto`)
# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' no es 'PARRILLA'
consecuentes_menuMENU2 <- unique(subset(dataNP2, `Sub Grupo` != 'PARRILLA')$`Producto`)

# Filtro reglas requeridas
filtered_rulesMENU2 <- subset(reglas_asociacion2, 
                              subset = lhs %in% antecedentes_parrillaMENU2 & 
                                !(lhs %in% consecuentes_menuMENU2) & 
                                rhs %in% consecuentes_menuMENU2)

##reglas_asociacion <- sort(reglas_asociacion, by = "confidence", decreasing = TRUE)
duplicated(filtered_rulesMENU2)
reglas_redundantes2 <- is.redundant(filtered_rulesMENU2)
which(is.redundant(filtered_rulesMENU2))
reglas_podadas2 <- filtered_rulesMENU2[!reglas_redundantes2]
summary(reglas_podadas2)

reglas_ordenadas2 <- reglas_podadas2[order(reglas_podadas2@quality$lift, decreasing = TRUE),]
top_10_reglas2 <- head(reglas_ordenadas2, 10)
print(top_10_reglas2)
inspectDT(top_10_reglas2)
summary(top_10_reglas2)

plot(top_10_reglas2, method = "graph",  engine = "htmlwidget", shading = "confidence")
productos_frecuentes2 <- data.frame(Productos = labels(reglas_podadas1), 
                                    Indicador = reglas_podadas@quality)

write.csv(productos_frecuentes2, "TFM/REGLAS/REGLAS_NUEVAS/personalizacion_2/reglas_BEBIDAS_2_mediatarde.csv")









                                      ## PERSONALIZACIÓN 3 ##

# Leer data set limpio
dataNP3 <- read_csv("TFM/DATOS_NUEVOS/personalizacion_3/personalizacion_3_noche.csv", 
                    col_types = cols(Pedido = col_character(), 
                                     Cant. = col_number(), Total = col_number(), 
                                     Fecha = col_date(format = "%Y-%d-%m"), 
                                     Hora = col_time(format = "%H:%M")))

# Filtro MENU/BEBIDAS
dataNP3 <- dataNP3 %>% filter(!`Sub Grupo` %in% c('MENU'))

# Preprocesado de la data
datos_Apriori3 <- dataNP3 %>% mutate(`Producto` = as.factor(`Producto`))
pedidos3 <- ddply(datos_Apriori3,c("Pedido"), function(df3) paste(df3$`Producto`, collapse = ","))

write.csv(pedidos3,"pedidos3.csv", quote = FALSE, row.names = FALSE)
ped3 <- read.transactions("pedidos3.csv", format = 'basket', sep=',', header = TRUE)

# Definición de parámetros REGLAS
param_reglas3 = list(
  supp = 8/dim(ped3)[1],
  conf = 0.3,
  target = "rules")

#Reglas se asociación
reglas_asociacion3 <- apriori(ped3, parameter = param_reglas3)

# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' es 'PARRILLA'
antecedentes_parrillaMENU3 <- unique(subset(dataNP3, `Sub Grupo` == 'PARRILLA')$`Producto`)
# Obtener los valores únicos de la columna 'Servicio personalizado' donde 'Sub Grupo' no es 'PARRILLA'
consecuentes_menuMENU3 <- unique(subset(dataNP3, `Sub Grupo` != 'PARRILLA')$`Producto`)

# Filtro reglas requeridas
filtered_rulesMENU3 <- subset(reglas_asociacion3, 
                              subset = lhs %in% antecedentes_parrillaMENU3 & 
                                !(lhs %in% consecuentes_menuMENU3) & 
                                rhs %in% consecuentes_menuMENU3)

##reglas_asociacion <- sort(reglas_asociacion, by = "confidence", decreasing = TRUE)
duplicated(filtered_rulesMENU3)
reglas_redundantes3 <- is.redundant(filtered_rulesMENU3)
which(is.redundant(filtered_rulesMENU3))
reglas_podadas3 <- filtered_rulesMENU3[!reglas_redundantes3]
summary(reglas_podadas3)

reglas_ordenadas3 <- reglas_podadas3[order(reglas_podadas3@quality$lift, decreasing = TRUE),]
top_10_reglas3 <- head(reglas_ordenadas3, 10)
print(top_10_reglas3)
inspectDT(top_10_reglas3)
summary(top_10_reglas3)

plot(top_10_reglas3, method = "graph",  engine = "htmlwidget", shading = "confidence")
productos_frecuentes2 <- data.frame(Productos = labels(reglas_podadas1), 
                                    Indicador = reglas_podadas@quality)

write.csv(productos_frecuentes2, "TFM/REGLAS/REGLAS_NUEVAS/personalizacion_2/reglas_BEBIDAS_2_mediatarde.csv")