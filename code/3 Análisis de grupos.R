# CARGAR DATOS
setwd("./data")
load("Datos_1.RData")
load("clustID.RData")
Datos_3 <- Datos_1
Datos_3$clustID <- clustID
Variables <- c("Region", "Sexo", "sd28_F1", "IMC", "h2",
               "di5", "dis2", "af1b", "as28", "m4p3",
               "o6", "GPAQ", "di7_1", "di7_2", "di7_3",
               "ta3", "ta4", "Frutas", "Verduras", "IndSD",
               "IndTMP", "NEDU")
#
# ANÁLISIS DE DATOS PERDIDOS
library(naniar)
for (grupo in 1:7) print(gg_miss_upset(Datos_3[Datos_3$clustID == grupo, Variables],
                                 nsets = 5))
sapply(X = 1:7, FUN = function(g) {
  mean(complete.cases(Datos_3[Datos_3$clustID == g, Variables]))})
#
# MODIFICACIÓN DE VARIABLES
Datos_3$Region <- factor(Datos_3$Region,
                         levels = c(15, 1:5, 13, 6:9, 14, 10:12),
                         labels = c("XV. Arica y Parinacota",
                                    "I. Tarapacá",
                                    "II. Antofagasta",
                                    "III. Atacama",
                                    "IV. Coquimbo",
                                    "V. Valparaíso",
                                    "XIII. Metropolitana",
                                    "VI. L. Bdo. O'Higgins",
                                    "VII. Maule",
                                    "VIII. Bíobío",
                                    "IX. La Araucanía",
                                    "XIV. Los Ríos",
                                    "X. Los Lagos",
                                    "XI. Aysén",
                                    "XII. Magallanes y Antártica"),
                         ordered = TRUE)
levels(Datos_3$as28) <- c("Menos de $77.999",
                          "$78.000 A $134.999",
                          "$135.000 A $217.999",
                          "$218.000 A $295.999",
                          "$296.000 A $383.999",
                          "$384.000 A $480.999",
                          "$481.000 A $607.999",
                          "$608.000 A $764.999",
                          "$765.000 A $1.029.999",
                          "$1.030.000 A $1.572.999",
                          "Más de $1.573.000")
levels(Datos_3$di7_1) <- c("Sí", "No")
levels(Datos_3$di7_2) <- c("Sí", "No")
levels(Datos_3$di7_3) <- c("Sí", "No")
levels(Datos_3$IndTMP) <- c("OK", "Alterado")
#
# NUEVAS VARIABLES
Datos_3$ta4cat <- NA
Datos_3$ta4cat[Datos_3$ta4 == 0] <- 0
Datos_3$ta4cat[Datos_3$ta4 >= 1  & Datos_3$ta4 <= 10] <- 1
Datos_3$ta4cat[Datos_3$ta4 >= 11 & Datos_3$ta4 <= 20] <- 2
Datos_3$ta4cat[Datos_3$ta4 >= 21 & Datos_3$ta4 <= 40] <- 3
Datos_3$ta4cat[Datos_3$ta4 >= 41 & Datos_3$ta4 <= 80] <- 4
Datos_3$ta4cat <- factor(x = Datos_3$ta4cat,
                         levels = 0:4,
                         labels = c("{0}",       # Ningún cigarrillo
                                    "[1, 10]",   # Media cajetilla o menos
                                    "[11, 20]",  # Entre media o una cajetilla
                                    "[21, 40]",  # Entre una o dos cajetillas
                                    "[41, 80]")) # Más de dos cajetillas
Variables[17] <- "ta4cat"
#
# TABLA RESUMENES ESTADÍSTICOS
library(tableone)
tabla <- CreateTableOne(vars = Variables,
                        strata = "clustID",
                        data = Datos_3,
                        addOverall = TRUE)
tabla <- print(tabla, test = FALSE, missing = TRUE,
               #nonnormal = "ta4",
               formatOptions = list(big.mark = ","))
write.csv(tabla, file = "../tables/Table S3 (in Spanish).csv")
#
# ANÁLISIS VISUAL ####
pdf(file = "../figures/Figuras - análisis de grupos.pdf")
# Region
levels(Datos_3$Region) <- sub(pattern = "\\. .*",
                              replacement = "",
                              x = levels(Datos_3$Region))
mosaicplot(table(Datos_3$Region, Datos_3$clustID),
           col = 2:8,
           main = "Fig.A. Proporción de cada grupo según región",
           ylab = "Grupo")
# Sexo
mosaicplot(table(Datos_3$clustID, Datos_3$Sexo),
           col = c("Blue", "Red"),
           main = "Fig.B. Proporción de cada sexo según grupo",
           xlab = "Grupo",
           las = 1)
# sd28_F1
mosaicplot(table(Datos_3$clustID, Datos_3$sd28_F1, exclude = FALSE),
           col = c("White", "Black", "Gray"),
           main = "Fig.C1. Proporción de cada respuesta a sd28\n('No responde' o 'No sabe' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$sd28_F1),
           col = c("White", "Black"),
           main = "Fig.C2. Proporción de cada respuesta a sd28\n('No responde' o 'No sabe' omitidas)",
           xlab = "Grupo",
           las = 1)
# IMC
boxplot(Datos_3$IMC ~ Datos_3$clustID, xlab = "Grupo", ylab = "IMC", col = 2:8,
        main = "Fig.D. IMC según grupo")
# h2
mosaicplot(table(Datos_3$clustID, Datos_3$h2, exclude = FALSE),
           col = c("Green", "Yellow", "Red", "Gray"),
           main = "Fig.E1. Proporción de cada respuesta a h2 según grupo\n('No recuerdo, no estoy seguro(a)' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$h2),
           col = c("Green", "Yellow", "Red"),
           main = "Fig.E2. Proporción de cada respuesta a h2 según grupo\n('No recuerdo, no estoy seguro(a)' omitida)",
           xlab = "Grupo",
           las = 1)
# di5
mosaicplot(table(Datos_3$clustID, Datos_3$di5, exclude = FALSE),
           col = c("White", "Black", "Gray"),
           main = "Fig.F1. Proporción de cada respuesta a di5 según grupo\n('No responde' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$di5),
           col = c("White", "Black"),
           main = "Fig.F2. Proporción de cada respuesta a di5 según grupo\n('No responde' omitida)",
           xlab = "Grupo",
           las = 1)
# dis2
mosaicplot(table(Datos_3$clustID, Datos_3$dis2, exclude = FALSE),
           col = c("Green", "Yellow", "Red", "Gray"),
           main = "Fig.G1. Proporción de cada respuesta a dis2 según grupo\n('No recuerdo, no estoy seguro(a)' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$dis2),
           col = c("Green", "Yellow", "Red"),
           main = "Fig.G2. Proporción de cada respuesta a dis2 según grupo\n('No recuerdo, no estoy seguro(a)' omitida)",
           xlab = "Grupo",
           las = 1)
# af1b
mosaicplot(table(Datos_3$clustID, Datos_3$af1b, exclude = FALSE),
           col = c("White", "Black", "Gray"),
           main = "Fig.H1. Proporción de cada respuesta a af1b según grupo\n('No sabe' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$af1b),
           col = c("White", "Black"),
           main = "Fig.H2. Proporción de cada respuesta a af1b según grupo\n('No sabe' omitida)",
           xlab = "Grupo",
           las = 1)
# as28
mosaicplot(table(Datos_3$clustID, Datos_3$as28, exclude = FALSE),
           col = c(colorRampPalette(c("red", "yellow", "green", "blue"))( 11 ), "Gray"),
           main = "Fig.I1. Proporción de cada respuesta a as28 según grupo\n('No responde' o 'No sabe' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$as28),
           col = colorRampPalette(c("red", "yellow", "green", "blue"))( 11 ),
           main = "Fig.I2. Proporción de cada respuesta a as28 según grupo\n('No responde' o 'No sabe' omitidas)",
           xlab = "Grupo",
           las = 1)
# m4p3
boxplot(Datos_3$m4p3 ~ Datos_3$clustID, xlab = "Grupo",
        ylab = "Perímetro de cintura", col = 2:8,
        main = "Fig.J. Perímetro de cintura según grupo")
# o6
mosaicplot(table(Datos_3$clustID, Datos_3$o6, exclude = FALSE),
           col = c("White", "Black", "Gray"),
           main = "Fig.K1. Proporción de cada respuesta a o6 según grupo\n(Registros sin información en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$o6),
           col = c("White", "Black"),
           main = "Fig.K2. Proporción de cada respuesta a o6 según grupo\n(Registros sin información omitidos)",
           xlab = "Grupo",
           las = 1)
# GPAQ
mosaicplot(table(Datos_3$clustID, Datos_3$GPAQ, exclude = FALSE),
           col = c("Red", "Yellow", "Green", "Gray"),
           main = "Fig.L1. Proporción de niveles de GPAQ según grupo\n(Registros sin información en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$GPAQ),
           col = c("Red", "Yellow", "Green"),
           main = "Fig.L2. Proporción de niveles de GPAQ según grupo\n(Registros sin información en gris)",
           xlab = "Grupo",
           las = 1)
# di7_1, di7_2, di7_3
mosaicplot(table(Datos_3$clustID, Datos_3$di7_1),
           col = c("White", "Black"),
           main = "Fig.M. Proporción de respuestas a di7_1 según grupo",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$di7_2),
           col = c("White", "Black"),
           main = "Fig.N. Proporción de respuestas a di7_2 según grupo",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$di7_3),
           col = c("White", "Black"),
           main = "Fig.Ñ. Proporción de respuestas a di7_3 según grupo",
           xlab = "Grupo",
           las = 1)
# ta3
mosaicplot(table(Datos_3$clustID, Datos_3$ta3),
           col = c("Green", "Yellow", "Orange", "Red", "Gray"),
           main = "Fig.O. Proporción de respuestas a ta3 según grupo",
           xlab = "Grupo",
           las = 1)
# ta4cat
mosaicplot(table(Datos_3$clustID, Datos_3$ta4cat),
           col = c("Green", colorRampPalette(c("yellow", "red"))( 4 )),
           main = "Fig.P. Proporción de categorizaciones de ta4 según grupo",
           xlab = "Grupo",
           las = 1)
# Frutas, Verduras
mosaicplot(table(Datos_3$clustID, Datos_3$Frutas),
           col = c("Red", "Yellow", "Green", "Blue"),
           main = "Fig.Q. Proporción de niveles de consumo de frutas según grupo",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$Verduras),
           col = c("Red", "Yellow", "Green", "Blue"),
           main = "Fig.R. Proporción de niveles de consumo de verduras según grupo",
           xlab = "Grupo",
           las = 1)
# IndSD
mosaicplot(table(Datos_3$clustID, Datos_3$IndSD, exclude = FALSE),
           col = c("White", "Black", "Gray"),
           main = "Fig.S1. Proporción de categorías de Nº de síntomas depresivos\nsegún grupo (Registros sin información en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$IndSD),
           col = c("White", "Black"),
           main = "Fig.S2. Proporción de categorías de Nº de síntomas depresivos\nsegún grupo (Registros sin información omitidos)",
           xlab = "Grupo",
           las = 1)
# IndTMP
mosaicplot(table(Datos_3$clustID, Datos_3$IndTMP),
           col = c("White", "Black"),
           main = "Fig.T. Proporción de participantes con resultados\nalterados en tests mini-mental o de Pfeffer (o ambos)",
           xlab = "Grupo",
           las = 1)
# NEDU
mosaicplot(table(Datos_3$clustID, Datos_3$NEDU, exclude = FALSE),
           col = c("White", "Skyblue", "Green", "Red", "Blue", "Yellow", "Gray"),
           main = "Fig.U1. Proporción de niveles de educación\n('No responde' o 'No sabe' en gris)",
           xlab = "Grupo",
           las = 1)
mosaicplot(table(Datos_3$clustID, Datos_3$NEDU),
           col = c("White", "Skyblue", "Green", "Red", "Blue", "Yellow"),
           main = "Fig.U2. Proporción de niveles de educación\n('No responde' o 'No sabe' omitidas)",
           xlab = "Grupo",
           las = 1)
dev.off()
setwd("..")