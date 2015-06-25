rm(list = ls())
graphics.off()

#set directory auto
setwd("~/Thesis/R scripts/Lecture script")

#load libraries and scripts
require(matlab)
source("pca_eigen.r")
source("pca_svd.r")
source("pca_eigen_3d.r")
source("auto.r")
source("mncn.r")
source("rangescale.r")
######################
#static file load for my format of data: ID/Batch/Class/wavelengths
fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv"
DATA <- read.table(fileToLoad, sep = ",", header = TRUE, row.names = 1)

#ignore some columns and load rest to X
fieldsToIgnore <- c("Batch", "Class")
X <- DATA[,!(names(DATA) %in% fieldsToIgnore)]
#######################
#some scaling
Xas <- auto(X)
Xmncn <- mncn(X)
Xrs <- rangescale(X)

#retrieve classes of samples
CLASS <- DATA$Class
#retrieve names of samples
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
#prcom() function is this version or at least gives the same results
##########
#noscale
pca_svd(X, samplenames, CLASS, "noscale")
#autoscale
pca_svd(Xas, samplenames, CLASS, "autoscale")
#meancentered
pca_svd(Xmncn, samplenames, CLASS, "meancentered")
#rangescaled
pca_svd(Xrs, samplenames, CLASS, "rangescale")
