rm(list = ls())
graphics.off()
require(matlab)
source("pca_eigen.r")
source("pca_svd.r")
source("pca_eigen_3d.r")
source("auto.r")
source("mncn.r")
source("rangescale.r")
#set directory auto
setwd("~/Thesis/R scripts/Lecture script")
#static file load
fileToLoad = "../../Data/batch2_classes.csv"
DATA <- read.table(fileToLoad, sep = ",", header = TRUE, row.names = 1)
fieldsToIgnore <- c("Batch", "Adulteration")
X <- DATA[,!(names(DATA) %in% fieldsToIgnore)]
Xas <- auto(X)
Xmncn <- mncn(X)
Xrs <- rangescale(X)

CLASS <- DATA$Adulteration
samplenames <- row.names(X)
#wavelengths <- colnames(X)

##########
#PCA eigen
#worse results than in svd
##########
#noscale
pca_eigen(X, samplenames, CLASS, "noscale")
#autoscale
pca_eigen(Xas, samplenames, CLASS, "autoscale")
#meancentered
pca_eigen(Xmncn, samplenames, CLASS, "meancentered")
#rangescaled
pca_eigen(Xrs, samplenames, CLASS, "rangescale")

##########
#PCA SVD
#prcom() function is this version
##########
#noscale
pca_svd(X, samplenames, CLASS, "noscale")
#autoscale
pca_svd(Xas, samplenames, CLASS, "autoscale")
#meancentered
pca_svd(Xmncn, samplenames, CLASS, "meancentered")
#rangescaled
pca_svd(Xrs, samplenames, CLASS, "rangescale")
