# CARGA DE PAQUETES ####
library(openxlsx)     # para cargar datos que están en un archivo xlsx
library(dplyr)        # para usar 'pipes'
#
# CARGA DE DATOS ####
# Ruta que describe la localización del archivo. Alternativamente puede ejecutar
# 'file.choose()' y seleccionar el archivo xlsx con los datos.
setwd("./data")
Datos_0 <- "Base-de-Datos-F1-F2-EX-CIDI-metales.xlsx"
# Cargar los datos en un objeto de tipo 'data.frame'
Datos_0 <- openxlsx::read.xlsx(xlsxFile = Datos_0, sheet = "Datos")
#
# SELECCIÓN DE PARTICIPANTES ####
# Criterios de selección: 'di3' y 'm2p6' con respuestas iguales a '1'.
table(Datos_0$di3, Datos_0$m2p6, exclude = FALSE)
652/nrow(Datos_0)
# 10% de los participantes de la ENS (652) cumplen el criterio de inclusión.
# Crear un nuevo objeto para trabajar con este subconjunto.
Datos_1 <- subset(x = Datos_0, subset = di3 == "1" & m2p6 == "1")
#
# SELECCIÓN DE VARIABLES
# 'Region': se debe cambiar su clase desde 'character' a 'factor'
Datos_1$Region <- as.factor(x = Datos_1$Region)
# 'Sexo': se debe cambiar su clase desde 'character' a 'factor'
Datos_1$Sexo <- factor(x = Datos_1$Sexo,
                       levels = c("1", "2"),
                       labels = c("Hombre", "Mujer"),
                       ordered = FALSE)
# 'sd28_F1': se deben cambiar respuestas '-8888' (no sabe) y '-9999' (no
#            responde) a 'NA' (valor perdido), y cambiar desde clase 'character'
#            a 'factor'.
Datos_1$sd28_F1[Datos_1$sd28_F1 %in% c("-8888", "-9999")] <- NA
Datos_1$sd28_F1 <- factor(x = Datos_1$sd28_F1,
                          levels = c("2", "1"),
                          labels = c("No", "Sí"),
                          ordered = FALSE)
# 'IMC': no necesita modificaciones.
# 'h2': se debe cambiar respuesta '4' (no recuerda o no está seguro/a) a 'NA'
#       (valor perdido), y añadir la clase 'ordered' a su clase 'factor'.
Datos_1$h2[Datos_1$h2 == "4"] <- NA
Datos_1$h2 <- factor(
  x = Datos_1$h2,
  levels = c("3", "1", "2"), # de menor a mayor
  labels = c("No, nunca me lo han dicho",
             "Sí, una sola vez",
             "Sí, más de una vez"),
  ordered = TRUE)
# 'h5': se debe cambiar su calse desde 'character' a 'factor'.
Datos_1$h5 <- factor(x = Datos_1$h5,
                     levels = c("1", "2"),
                     labels = c("Sí", "No"),
                     ordered = FALSE)
# 'di5': se deben cambiar respuestas '-8888' (no sabe) y '-9999' (no responde)
#        a 'NA' (valor perdido), y cambiar desde clase 'character' a 'factor'.
Datos_1$di5[Datos_1$di5 %in% c("-8888", "-9999")] <- NA
Datos_1$di5 <- factor(x = Datos_1$di5,
                      levels = c("1", "2"),
                      labels = c("Sí", "No"),
                      ordered = FALSE)
# 'dis2': se debe cambiar respuesta '4' (no recuerda o no está seguro/a) a 'NA'
#         (valor perdido), cambiar a clase 'factor' y 'ordered'.
Datos_1$dis2[Datos_1$dis2 == "4"] <- NA
Datos_1$dis2 <- factor(x = Datos_1$dis2,
                       levels = c("3", "1", "2"), # de menor a mayor
                       labels = c("No, nunca me lo han dicho",
                                  "Sí, una sola vez",
                                  "Sí, más de una vez"),
                       ordered = TRUE)
# 'af1b': se debe cambiar respuesta '-8888' (No sabe) a 'NA' y cambiar desde su
#         clase 'character' a 'factor'.
Datos_1$af1b[Datos_1$af1b == "-8888"] <- NA
Datos_1$af1b <- factor(x = Datos_1$af1b,
                       levels = c("1", "2"),
                       labels = c("Sí", "No"),
                       ordered = FALSE)
# 'as28': se deben cambiar respuestas '-8888' (No sabe) y '-9999' (No responde)
#         por NA, y cambiar desde su clase 'character' a 'factor' y 'ordered'.
Datos_1$as28[Datos_1$as28 %in% c("-8888", "-9999")] <- NA
Datos_1$as28 <- factor(x = Datos_1$as28,
                       levels = 1:11,
                       ordered = TRUE)
# 'm4p3': cambiar desde clase 'character' a clase 'numeric'.
Datos_1$m4p3 <- as.numeric(x = Datos_1$m4p3)
# 'o6': cambiar desde clase 'character' a clase 'factor'.
Datos_1$o6 <- factor(x = Datos_1$o6,
                     levels = c("1", "2"),
                     labels = c("Poca", "Mucha"),
                     ordered = TRUE)
# 'gpaq': cambiar desde clase 'character' a clase 'factor' y 'ordered'.
Datos_1$GPAQ <- factor(x = Datos_1$GPAQ,
                       levels = c("1", "2", "3"), # de menor a mayor
                       labels = c("Bajo nivel", "Moderado nivel", "Alto nivel"),
                       ordered = TRUE)
# 'di7_1': cambiar desde clase 'character' a clase 'factor'.
Datos_1$di7_1 <- as.factor(ifelse(test = is.na(Datos_1$di7_1), yes = 0, no = 1))
# 'di7_2': cambiar desde clase 'character' a clase 'factor'.
Datos_1$di7_2 <- as.factor(ifelse(test = is.na(Datos_1$di7_2), yes = 0, no = 1))
# 'di7_3': cambiar desde clase 'character' a clase 'factor'.
Datos_1$di7_3 <- as.factor(ifelse(test = is.na(Datos_1$di7_3), yes = 0, no = 1))
# 'ta3': cambiar desde clase 'character' a clase 'factor' y 'ordered'.
Datos_1$ta3 <- factor(x = Datos_1$ta3,
                      levels = c(4,3,2,1), # de menor a mayor
                      labels = c("No, nunca he fumado",
                                 "No, he dejado de fumar",
                                 "Sí, ocasionalmente",
                                 "Sí, uno o más cigarrillos al día"),
                      ordered = TRUE)
# 'ta4': cambiar desde 'character' a 'numeric' y cambiar 'NA' a '0'
Datos_1$ta4[Datos_1$ta3 != "Sí, uno o más cigarrillos al día"] <- 0
Datos_1$ta4 <- as.numeric(Datos_1$ta4)
#
# Construcción de variables ####
# Construir 'Frutas' = número de porciones de frutas en una semana típica
Datos_1$Frutas <- as.numeric(Datos_1$die6) * as.numeric(Datos_1$die7)
Datos_1$Frutas[is.na(Datos_1$Frutas)] <- 0
Datos_1$Frutas <- cut(x = Datos_1$Frutas, breaks = 7*c(0:3,15),
                      right = FALSE, ordered_result = TRUE)
# Construir 'Verduras' = número de porciones de verduras en una semana típica
Datos_1$Verduras <- as.numeric(Datos_1$die8) * as.numeric(Datos_1$die9)
Datos_1$Verduras[is.na(Datos_1$Verduras)] <- 0
Datos_1$Verduras <- cut(x = Datos_1$Verduras, breaks = 7*c(0:3,15),
                        right = FALSE, ordered_result = TRUE)
# Construir 'IndSD' = indicador de tener uno o más sintomas depresivos
Datos_1$IndSD <- cut(x = Datos_1$Cantidad_sintomas_depresivos,
                     breaks = c(-Inf, 1, Inf), right = FALSE,
                     labels = c("0", "1 o más"))
# Construir 'IndTMP' = indicador de test minimental o Pfeffer alterado
C1 <- (Datos_1$Edad >= 60)
C2 <- (Datos_1[C1,]$Puntaje_MMentalMINSAL < 13)
C3 <- (Datos_1[C1,]$ptjePfeffer_MINSAL >= 6)
Datos_1$IndTMP <- 0
Datos_1$IndTMP[C1][C2] <- 1
Datos_1$IndTMP[C1][C3] <- 1
Datos_1$IndTMP <- as.factor(Datos_1$IndTMP)
rm(list = c("C1", "C2", "C3"))
# Construir 'NEDU' = categoría de educación recibida
Datos_1$NEDU <- 0
Datos_1$NEDU[Datos_1$as7_corr_1 == "1"] <- 1
Datos_1$NEDU[Datos_1$as7_corr_1 %in% c("2", "3", "4", "5")] <- 2
Datos_1$NEDU[Datos_1$as7_corr_1 %in% c("6", "7")] <- 3
Datos_1$NEDU[Datos_1$as7_corr_1 %in% c("8", "9")] <- 4
Datos_1$NEDU[Datos_1$as7_corr_1 %in% c("10", "11")] <- 5
Datos_1$NEDU[Datos_1$as7_corr_1 %in% c("12", "13", "14")] <- 6
Datos_1$NEDU[Datos_1$NEDU == 0] <- NA
Datos_1$NEDU <- factor(x = Datos_1$NEDU,
                       levels = 1:6, # de menor a mayor
                       labels = c("Ninguna",
                                  "Pre-escolar o diferencial",
                                  "Básica o equivalente",
                                  "Media o equivalente",
                                  "Técnica o equivalente",
                                  "Superior"),
                       ordered = TRUE)
#
# GUARDADO DE DATOS ####
save(Datos_0, file = "Datos_0.RData")
save(Datos_1, file = "Datos_1.RData")
setwd("..")