##############################################
# Performs PCA viar Eugen Vectors            #
# and Singular Value Decomposition           #
# Results available as pdf files             #
#                                            #
# Rafal Kural                                #
##############################################

# clear workspace and open windows
rm(list = ls())
graphics.off()

#set working directory to script directory
test <- dirname(sys.frame(1)$ofile)
setwd(test)

#load required libraries and scripts
require(matlab)
source("pca_eigen.r")
source("pca_svd.r")
source("../Toolbox/scalings/auto.r")
source("../Toolbox/scalings/mncn.r")
source("../Toolbox/scalings/rangescale.r")
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

#retrieve classes of samples
CLASS <- DATA$Class
#retrieve names of samples
samplenames <- row.names(X)
#wavelengths <- colnames(X)

#some scaling
Xas <- auto(X)
Xmncn <- mncn(X)
Xrs <- rangescale(X)

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
pca_svd(X, samplenames, CLASS, paste(appendName, "noscale", sep="_"))
#autoscale
pca_svd(Xas, samplenames, CLASS, paste(appendName, "autoscale", sep="_"))
#meancentered
pca_svd(Xmncn, samplenames, CLASS, paste(appendName, "meancentered", sep="_"))
#rangescaled
pca_svd(Xrs, samplenames, CLASS, paste(appendName, "rangescale", sep="_"))
