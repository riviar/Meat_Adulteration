##############################################
# Performs PLS regression                    #
# Results available as csv files             #
# Main use to adjust class prediction model  #
# using numerical values                     #
#                                            #
# Rafal Kural                                #
##############################################
# clear workspace and open windows
rm(list = ls())
graphics.off()

#set working directory to script directory
test <- dirname(sys.frame(1)$ofile)
setwd(test)

#loading libraries and required scripts
#require(plsgenomics)
#require(pls)
require(plsdepot)
require(pls)
source("generate_loading_plot_pdf.r")
source("../Toolbox/data_reading/load_file_for_regression.r")
source("run_depot_plsreg.r")
require(prospectr)

###### LOAD DATA FROM FILE ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
#fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data

# load chosen file
DATA <- load_file_for_regression(fileToLoad)
X <- DATA[[1]]
CLASS <- DATA[[3]]

#VM ONLY
#remove selected batches
#batchToRemoveIndices <- grep("b2", rownames(X))
#X <- X[-batchToRemoveIndices,]
#CLASS <- CLASS[-batchToRemoveIndices]

#optionally scale the data
Xscaled <- savitzkyGolay(X, m=0, p=3, w=27)
#X <- standardNormalVariate(Xscaled)
X <- msc(Xscaled)
###### DATA LOAD END #########################

###### SETTING TRAINING AND TEST DATA ########
#retrieve training data
Xtrain <- X[-seq(1, nrow(X), 4),]
CLASStrain <- CLASS[-seq(1, length(CLASS), 4)]

#retrieve testing data
Xtest <- X[seq(1, nrow(X), 4),]
CLASStest <- CLASS[seq(1, nrow(X), 4)]
###### END ###################################

#### SETTINGS START ##########################
# Settings allow automating analysis process
# number of latent components to use for dimension reduction, vector of values allowed
componentsNumber <- c(1:40)
# additional file append - scaling or other, will be added to result file name in form Result_addedFileAppend_rest
#addedFileAppend <- "FTIR_SG_SNV_0.75"
addedFileAppend <- "FTIR_SG_MSC_0.75"
# create directory for outputs (does nothing if it exists)
outputDir = "../Results/PLSREGR_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

# run algorithm for all components
run_depot_plsreg(Xtrain, CLASStrain, Xtest, CLASStest, componentsNumber, outputDir)

# create pdf with loadings plots
#generate_loading_plot_pdf(LOADINGS = PLSregr$P, filename = paste(outputDir, "/Loadings_Plot_", addedFileAppend, "_.pdf", sep=""), ncompNum = length(componentsNumber))