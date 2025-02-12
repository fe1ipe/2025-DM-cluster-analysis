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
Datos_3$Edad_Codificada <- factor(Datos_3$Edad_Codificada,
                                  levels = 1:4,
                                  labels = c("15 - 24",
                                             "25 - 44",
                                             "45 - 64",
                                             "65+"),
                                  ordered = TRUE)
Datos_3$SINDROME_METABOLICO <- factor(Datos_3$SINDROME_METABOLICO,
                                  levels = 0:1,
                                  labels = c("No",
                                             "Sí"),
                                  ordered = TRUE)
PostVariables <- c("Edad", "Edad_Codificada", "SINDROME_METABOLICO")
library(tableone)
PostTabla <- CreateTableOne(vars = PostVariables,
                            strata = "clustID",
                            data = Datos_3,
                            addOverall = TRUE)
PostTabla <- print(PostTabla, test = FALSE, missing = TRUE,
                   nonnormal = "Edad",
                   formatOptions = list(big.mark = ","))
write.csv(PostTabla, file = "../tables/PostTabla.csv")
#
# FIGURAS
pdf(file = "../figures/Figuras adicionales.pdf")
# o6 según Region
levels(Datos_3$Region) <- sub(pattern = "\\. .*",
                              replacement = "",
                              x = levels(Datos_3$Region))
mosaicplot(table(Datos_3$Region, Datos_3$o6, exclude = FALSE),
           col = c("white", "black", "gray"),
           main = "Fig.V.1. Proporción de la respuesta o6 según región\n(Casos sin información en gris)",
           ylab = "")
mosaicplot(table(Datos_3$Region, Datos_3$o6),
           col = c("white", "black", "gray"),
           main = "Fig.V.2. Proporción de la respuesta o6 según región\n(Casos sin información omitidos)",
           ylab = "")
# Edad_Codificada según grupo
mosaicplot(table(Datos_3$clustID, Datos_3$Edad_Codificada),
           col = rainbow(4),
           main = "Fig.W. Proporción de Edad_Codificada según grupo",
           ylab = "Edad_Codificada",
           las = 1)
# GPAQ según Edad_Codificada
mosaicplot(table(Datos_3$Edad_Codificada, Datos_3$GPAQ, exclude = FALSE),
           col = c("red", "yellow", "green", "gray"),
           main = "Fig.X.1. Proporción de GPAQ según Edad_Codificada\n(Casos sin información en gris)",
           ylab = "GPAQ",
           las = 1)
mosaicplot(table(Datos_3$Edad_Codificada, Datos_3$GPAQ),
           col = c("red", "yellow", "green"),
           main = "Fig.X.2. Proporción de GPAQ según Edad_Codificada\n(Casos sin información omitidos)",
           ylab = "GPAQ",
           las = 1)
dev.off()
setwd("..")