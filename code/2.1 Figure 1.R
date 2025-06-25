library(cluster)
#
# LOAD DATA ####
setwd("./data")
load("Datos_1.RData")
load("perm.clust1_avg.sil.RData")
load("perm.clust2_avg.sil.RData")
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
# COMPUTE AVERAGE SILHOUETTE INDEXES FOR DIFFERENT CLUSTERING SOLUTIONS ####
# Function for such computations under hierarchical clustering.
avg.sil.fun1 <- function(distmat, kseq) {
  ClusSol <- hclust(d = distmat, method = "ward.D2")
  AvgSil <- sapply(
    X = kseq,
    FUN = function(k) {
      summary(cluster::silhouette(
        x = stats::cutree(tree = ClusSol, k = k),
        dist = distmat))$avg.width
      }
    )
  return(AvgSil)
  }
# Function for such computations under k-medoids clustering.
avg.sil.fun2 <- function(distmat, kseq) {
  AvgSil <- sapply(
    X = kseq,
    FUN = function(k) {
      cluster::pam(x = distmat,
                   k = k,
                   diss = TRUE)$silinfo$avg.width
      }
    )
  return(AvgSil)
  }
# Computation of average silhouette indexes for clustering solutions with
# different number of clusters. First for hierarchical clustering:
K1 = 651
clust1_avg.sil <- avg.sil.fun1(distmat = DistMat, kseq = 2:K1)
# Now for k-medoids clustering:
K2 = 50
clust2_avg.sil <- avg.sil.fun2(distmat = DistMat, kseq = 2:K2)
#
# FIGURE 1 ####
svg(
  filename = "../figures/Figure 1.svg",
  width = 6.43,
  height = 5.38)
par(mfrow = c(2,2),
    oma = c(0,0,0,0),
    mar = c(5,5,2,1),
    plt = c(0.21, 0.96, 0.3, 0.9),
    las = 1)
#
KSEQ <- c(2:30, seq(30,650,25)[-1])
# Fix Figure (a) limits
YLOWLIM <- min(clust1_avg.sil[1:29],
               sapply(perm.clust1_avg.sil, function(x) min(x[1:29]))) * 0.5
YUPPLIM <- max(clust1_avg.sil[1:29],
               sapply(perm.clust1_avg.sil, function(x) max(x[1:29]))) * 1.1
plot(NULL,
     xlim = c(2,30), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Number of clusters", ylab = "Average silhouette index",
     main = "(a)")
lapply(X = perm.clust1_avg.sil,
       FUN = function(x) {
         lines(x[1:29] ~ c(2:30), type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust1_avg.sil[1:29] ~ c(2:30), type = "l", lwd = 2)
# Fix Figure (b) limits
YLOWLIM <- min(clust1_avg.sil,
               sapply(perm.clust1_avg.sil, min)) * 0.5
YUPPLIM <- max(clust1_avg.sil,
               sapply(perm.clust1_avg.sil, max)) * 1.1
plot(NULL,
     xlim = c(2,K1), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Number of clusters", ylab = "Average silhouette index",
     main = "(b)")
lapply(X = perm.clust1_avg.sil,
       FUN = function(x) {
         lines(x ~ KSEQ, type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust1_avg.sil ~ c(2:K1), type = "l", lwd = 2)
#
KSEQ <- c(2:10, seq(10,K2,3)[-1])
# Fix Figure (c) limits
YLOWLIM <- min(clust2_avg.sil[1:9],
               sapply(perm.clust2_avg.sil, function(x) min(x[1:9]))) * 0.5
YUPPLIM <- max(clust2_avg.sil[1:9],
               sapply(perm.clust2_avg.sil, function(x) max(x[1:9]))) * 1.1
plot(NULL,
     xlim = c(2,10), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Number of clusters", ylab = "Average silhouette index",
     main = "(c)")
lapply(X = perm.clust2_avg.sil,
       FUN = function(x) {
         lines(x[1:9] ~ c(2:10), type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust2_avg.sil[1:9] ~ c(2:10), type = "l", lwd = 2)
# Fix Figure (d) limits
YLOWLIM <- min(clust2_avg.sil,
               sapply(perm.clust2_avg.sil, min)) * 0.5
YUPPLIM <- max(clust2_avg.sil,
               sapply(perm.clust2_avg.sil, max)) * 1.1
plot(NULL,
     xlim = c(2,K2), ylim = c(YLOWLIM, YUPPLIM),
     xlab = "Number of clusters", ylab = "Average silhouette index",
     main = "(d)")
lapply(X = perm.clust2_avg.sil,
       FUN = function(x) {
         lines(x ~ KSEQ, type = "l", lty = 1, col = rgb(0,0,1,.15))
       })
lines(clust2_avg.sil ~ c(2:K2), type = "l", lwd = 2)
dev.off()
setwd("..")
