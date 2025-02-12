library(cluster)
#
# LOAD DATA ####
setwd("./data")
load("Datos_1.RData")
#
# DEFINE VARIABLES ####
Variables <- c(
  "Region", "Sexo", "sd28_F1", "IMC", "h2",
  "di5", "dis2", "af1b",  "as28",
  "m4p3", "o6", "GPAQ", "di7_1", "di7_2",
  "di7_3", "ta3", "ta4", "Frutas", "Verduras",
  "IndSD", "IndTMP", "NEDU")
#
# COMPUTE DISTANCE MATRIX ####
# Create a data frame that only keeps the selected variables
Datos_2 <- Datos_1[,Variables]
# Create a weights vector (ordered according to the same variables)
Ponderaciones <- c(
  1,   1,   1/2, 1,   1,
  1/2, 1,   1,   1,   1,
  1/2, 1.5, 1/3, 1/3, 1/3,
  3/4, 1/4, 1,   1,   1/2,
  1,   1.5)
# Compute the distance matrix using Gower's metric
DistMat <- cluster::daisy(
  x = Datos_2,
  metric = "gower",
  type = list(symm = c("Sexo", "di5", "af1b", "o6",
                       "di7_1", "di7_2", "di7_3"),
              asymm = c("sd28_F1", "IndSD", "IndTMP")),
  weights = Ponderaciones,
  warnType = TRUE)
#
# FIGURE S1
png(
  filename = "../figures/Figure S1.png",
  width = 6.43,
  height = 5.38,
  units = "in",
  res = 72*4)
plot(hclust(d = DistMat, method = "ward.D2"), labels = FALSE, hang = -1, main = "")
dev.off()
#
# FIGURE S2
png(
  filename = "../figures/Figure S2.png",
  width = 6.43,
  height = 5.38,
  units = "in",
  res = 72*4)
clust2 <- cluster::pam(x = DistMat, k = 7, diss = TRUE)
plot(silhouette(clust2$clustering, DistMat),
     col = 2:8, border = NA,
     main = "")
dev.off()
setwd("..")