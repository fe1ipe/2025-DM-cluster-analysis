# CARGA DE DATOS ####
setwd("./data")
load("Datos_1.RData")
#
# ANÁLISIS EXPLORATORIO ####
Variables <- c("Region", "Sexo", "sd28_F1", "IMC", "h2",
               "di5", "dis2", "af1b",  "as28",
               "m4p3", "o6", "GPAQ", "di7_1", "di7_2",
               "di7_3", "ta3", "ta4", "Frutas", "Verduras",
               "IndSD", "IndTMP", "NEDU")
par(mfrow = c(2,2))
for (Variable in Variables) {
  clase <- class(Datos_1[,Variable])
  if (any(clase %in% "numeric")) hist(Datos_1[,Variable], main = Variable)
  if (any(clase %in% "factor")) barplot(table(Datos_1[,Variable]), main = Variable)
}
par(mfrow = c(1,1))
#
# ANÁLISIS DE DATOS PERDIDOS ####
# Proporción de entradas con datos completos para las variables seleccionadas
mean(complete.cases(Datos_1[,Variables])) # 66.10%
#
NumVarDatosPerdidos <- apply(X = Datos_1[,Variables],
           MARGIN = 1,
           FUN = function(x) {sum(is.na(x))})
barplot(table(NumVarDatosPerdidos))
#
# Visualización de patrones de pérdida de información
library(VIM)
Perdidos_1 <- aggr(x = Datos_1[,Variables], plot = FALSE)
plot(Perdidos_1,
     prop = TRUE, # usar tasa (TRUE) o frecuencia (FALSE) de pérdida por variable
     numbers = FALSE, # mostrar cuantas veces se repite cada patrón de perdida
     sortVars = TRUE, # ordenar variables de mayor a menor cantidad de datos perdidos
     sortCombs = TRUE, # ordenar combinaciones de mayor a menor frecuencia
     cex.axis = 3/4, las = 2)
#
# Otra visualización de patrones de pérdida de información
library(naniar)
gg_miss_upset(Datos_1[,Variables], nsets = 5)
gg_miss_upset(Datos_1[,Variables], nsets = 30, nintersects = 50)
#
# CÁLCULO DE LA MATRIZ DE DISTANCIAS ####
# Creación de un objeto de datos que sólo contiene las variables seleccionadas
Datos_2 <- Datos_1[,Variables]
# Crear un vector de ponderaciones (en el mismo orden que las variables)
Ponderaciones <- c(1,   1,   1/2, 1,   1,
                   1/2, 1,   1,   1,   1,
                   1/2, 1.5, 1/3, 1/3, 1/3,
                   3/4, 1/4, 1,   1,   1/2,
                   1,   1.5)
# Gráfico de barras mostrando la importancia de las variables
barplot(height = Ponderaciones,
        names.arg = Variables,
        horiz = TRUE, las = 2, cex.names = .75)
# Cálculo de la matriz de distancia usando la métrica de Gower
DistMat <- cluster::daisy(x = Datos_2,
                          metric = "gower",
                          type = list(symm = c("Sexo", "di5", "af1b", "o6",
                                               "di7_1", "di7_2", "di7_3"),
                                      asymm = c("sd28_F1", "IndSD", "IndTMP")),
                          weights = Ponderaciones,
                          warnType = TRUE)
# Gráfico de la densidad de probabilidad estimada
plot(density(as.dist(as.matrix(DistMat))), xlim = c(0,1))
# Permutaciones de la matriz de datos originales
P = 999 # Número de permutaciones requeridas
Datos_2p <- replicate(n = P,
                      expr = as.data.frame(lapply(X = Datos_2, FUN = sample)),
                      simplify = FALSE)
save(Datos_2p, file = "Datos_2p.RData")
# Matrices de distancia de las tablas de datos permutadas
DistMatp <- lapply(X = Datos_2p,
                   FUN = cluster::daisy,
                   metric = "gower",
                   type = list(symm = c("Sexo", "di5", "af1b", "o6",
                                        "di7_1", "di7_2", "di7_3"),
                               asymm = c("sd28_F1", "IndSD", "IndTMP")),
                   weights = Ponderaciones,
                   warnType = TRUE)
save(DistMatp, file = "DistMatp.RData")
#
# AGRUPAMIENTO JERARQUICO ####
pdf(file = "../figures/Figuras - agrupamiento.pdf")
library(cluster)
# Estudiar qué criterio o método de enlace produce dendrogramas más informativos
par(mfrow = c(2,2))
for (method in c("single", "complete", "centroid", "ward.D2")) {
  temp <- hclust(d = DistMat, method = method)
  plot(temp, labels = FALSE, main = method)
}
par(mfrow = c(1,1))
# El método de Ward generó un dendrograma que es más fácil de comunicar.
clust1 <- hclust(d = DistMat, method = "ward.D2")
plot(clust1, labels = FALSE, hang = -1, main = "Método de Ward")
# Ahora necesitamos conocer qué tan válido es este dendrograma.
# Función que cálcula el índice Silhouette para varias soluciones del
# agrupamiento jerárquico.
avg.sil.fun1 <- function(distmat, kseq) {
  ClusSol <- hclust(d = distmat, method = "ward.D2")
  AvgSil <- sapply(X = kseq,
                   FUN = function(k) {
                     summary(cluster::silhouette(
                       x = stats::cutree(tree = ClusSol, k = k),
                       dist = distmat))$avg.width
                     }
                   )
  return(AvgSil)
}
# Cálculo del índice Silhouette para las soluciones de agrupamiento jerárquico
K1 = 651
clust1_avg.sil <- avg.sil.fun1(distmat = DistMat, kseq = 2:K1)
# Cálculo del mismo índice, pero en las soluciones de los datos permutados
KSEQ <- c(2:30, seq(30,650,25)[-1]) # c(2:30, seq(40,650,10))
perm.clust1_avg.sil <- lapply(X = DistMatp,
                              FUN = avg.sil.fun1,
                              kseq = KSEQ)
save(perm.clust1_avg.sil, file = "perm.clust1_avg.sil.RData")
# load("perm.clust1_avg.sil.RData")
#
par(mfrow = c(1,2))
# Determinar límites de la figura A
YLOWLIM <- min(clust1_avg.sil[1:29],
               sapply(perm.clust1_avg.sil, function(x) min(x[1:29]))) * 0.5
YUPPLIM <- max(clust1_avg.sil[1:29],
               sapply(perm.clust1_avg.sil, function(x) max(x[1:29]))) * 1.1
plot(NULL,
     xlim = c(2,30), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Nº de conglomerados", ylab = "Índice Silhouette promedio",
     main = "Agrupamiento jerárquico\n(Zoom in)")
lapply(X = perm.clust1_avg.sil,
       FUN = function(x) {
         lines(x[1:29] ~ c(2:30), type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust1_avg.sil[1:29] ~ c(2:30), type = "l", lwd = 2)
# Determinar límites de la figura B
YLOWLIM <- min(clust1_avg.sil,
               sapply(perm.clust1_avg.sil, min)) * 0.5
YUPPLIM <- max(clust1_avg.sil,
               sapply(perm.clust1_avg.sil, max)) * 1.1
plot(NULL,
     xlim = c(2,K1), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Nº de conglomerados", ylab = "Índice Silhouette promedio",
     main = "Agrupamiento jerárquico\n(Zoom out)")
lapply(X = perm.clust1_avg.sil,
       FUN = function(x) {
         lines(x ~ KSEQ, type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust1_avg.sil ~ c(2:K1), type = "l", lwd = 2)
#
# Uno podría interpretar esta figura como que las soluciones de los
# agrupamientos para pocos no son mejores que los que se observarían
# en conjuntos de datos donde no existe asociación entre las variables.
# 
# AGRUPAMIENTO MEDIANTE OPTIMIZACIÓN ####
# Función que cálcula el índice Silhouette para varias soluciones del
# agrupamiento por K-medoides
avg.sil.fun2 <- function(distmat, kseq) {
  AvgSil <- sapply(X = kseq,
                   FUN = function(k) {
                     cluster::pam(x = distmat,
                                  k = k,
                                  diss = TRUE)$silinfo$avg.width
                   }
  )
  return(AvgSil)
}
# Cálculo del índice Silhouette para las soluciones del algoritmo k-medoides
K2 = 50
clust2_avg.sil <- avg.sil.fun2(distmat = DistMat, kseq = 2:K2)
# Cálculo del mismo índice, pero en las soluciones de los datos permutados
KSEQ <- c(2:10, seq(10,K2,3)[-1])
perm.clust2_avg.sil <- lapply(X = DistMatp,
                              FUN = avg.sil.fun2,
                              kseq = KSEQ)
save(perm.clust2_avg.sil, file = "perm.clust2_avg.sil.RData")
# load("perm.clust2_avg.sil.RData")
#
par(mfrow = c(1,2))
# Determinar límites de la figura A
YLOWLIM <- min(clust2_avg.sil[1:9],
               sapply(perm.clust2_avg.sil, function(x) min(x[1:9]))) * 0.5
YUPPLIM <- max(clust2_avg.sil[1:9],
               sapply(perm.clust2_avg.sil, function(x) max(x[1:9]))) * 1.1
plot(NULL,
     xlim = c(2,10), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Nº de conglomerados", ylab = "Índice Silhouette promedio",
     main = "Agrupamiento por k-medoides\n(Zoom in)")
lapply(X = perm.clust2_avg.sil,
       FUN = function(x) {
         lines(x[1:9] ~ c(2:10), type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust2_avg.sil[1:9] ~ c(2:10), type = "l", lwd = 2)
# Determinar límites de la figura B
YLOWLIM <- min(clust2_avg.sil,
               sapply(perm.clust2_avg.sil, min)) * 0.5
YUPPLIM <- max(clust2_avg.sil,
               sapply(perm.clust2_avg.sil, max)) * 1.1
plot(NULL,
     xlim = c(2,K2), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Nº de conglomerados", ylab = "Índice Silhouette promedio",
     main = "Agrupamiento por k-medoides\n(Zoom out)")
lapply(X = perm.clust2_avg.sil,
       FUN = function(x) {
         lines(x ~ KSEQ, type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust2_avg.sil ~ c(2:K2), type = "l", lwd = 2)
# Solución de 7 conglomerados
par(mfrow = c(1,1))
clust2 <- cluster::pam(x = DistMat,
                       k = 7,
                       diss = TRUE)
plot(silhouette(clust2$clustering, DistMat),
     col = 2:8, border = NA,
     main = "Gráfico de los índices Silhouette")
dev.off()
#
# Guardar las asignaciones a cada conglomerado
clustID <- cluster::pam(x = DistMat, k = 7, diss = TRUE)$clustering
save(clustID, file = "clustID.RData")
setwd("..")