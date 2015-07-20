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
source("pca_eigen.r")
source("run_pca_basic_scalings.r")
source("run_pca_sg_snv_combined.r")
source("../Toolbox/data_manipulation/load_data_from_file.r")
source("run_pca_sg_msc_combined.r")
######################
### LOAD FTIR DATA ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
#fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
#name of the data that will appear in result file name
#dataName = "VM"
fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data
#name of the data that will appear in result file name
dataName = "FTIR"

# load chosen file
DATA <- load_data_from_file(fileToLoad)
X <- DATA[[1]]
CLASS <- DATA[[3]]
#retrieve names of samples
samplenames <- row.names(X)
###### DATA LOAD END #########################

###### BASIC PCA #############################
# runs pca and creates PCA plots for first 3 PCs
# for raw data, autoscaled data, meancentered data
# and rangescaled data
#run_pca_basic_scalings(X, dataName)

####### PCA S-G + SNV ########################
# runs pca and created PCA plots for first 3 PCs
# with Savitzky-Golay and Standard Normal Variate
# combined scalings
#run_pca_sg_snv_combined(X, dataName)

####### PCA S-G + MSC ########################
# runs pca and created PCA plots for first 3 PCs
# with Savitzky-Golay and Multivariate Scatter Correction
# combined scalings
run_pca_sg_msc_combined(X, dataName)
