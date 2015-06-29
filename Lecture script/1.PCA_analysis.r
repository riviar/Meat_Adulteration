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
#static file load for kural format of data: ID/wavelengths/Batch/Class
fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
appendName = "VM"
#fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data
#appendName = "FTIR"

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
pca_eigen(X, samplenames, CLASS, paste(appendName, "noscale", sep="_"))
#autoscale
pca_eigen(Xas, samplenames, CLASS, paste(appendName, "autoscale", sep="_"))
#meancentered
pca_eigen(Xmncn, samplenames, CLASS, paste(appendName, "meancentered", sep="_"))
#rangescaled
pca_eigen(Xrs, samplenames, CLASS, paste(appendName, "rangescale", sep="_"))

##########
#PCA SVD
#prcom() function is this version or at least gives the same results
##########
#noscale
SCORES <- pca_svd(X, samplenames, CLASS, paste(appendName, "noscale", sep="_"))
#autoscale
pca_svd(Xas, samplenames, CLASS, paste(appendName, "autoscale", sep="_"))
#meancentered
pca_svd(Xmncn, samplenames, CLASS, paste(appendName, "meancentered", sep="_"))
#rangescaled
SCORESrs <- pca_svd(Xrs, samplenames, CLASS, paste(appendName, "rangescale", sep="_"))
