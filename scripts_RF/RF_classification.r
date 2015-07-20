##############################################
# Performs classification by RF
# Results available as csv files             
#                                            
# Rafal Kural                                
##############################################
# clear workspace and open windows
rm(list = ls())
graphics.off()

#set working directory to script directory
test <- dirname(sys.frame(1)$ofile)
setwd(test)

#loading libraries and required scripts
require(randomForest)
require(pls)
source("../Toolbox/data_manipulation/load_file_for_regression.r")
source("run_rf.r")

###### LOAD DATA FROM FILE ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
#fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data

# load chosen file
DATA <- load_file_for_regression(fileToLoad)

#extract wavelengths and classes
X <- DATA[[1]]
CLASS <- DATA[[3]]

#VM ONLY
#remove selected batches
#batchToRemoveIndices <- grep("b2", rownames(X))
#X <- X[-batchToRemoveIndices,]
#CLASS <- CLASS[-batchToRemoveIndices]

#FTIR ONLY
#reduce spectra number
#X <- X[,-seq(,1, nrow(X), 15)]
#X <- cut_off_by_variance(X, 0.5)

#optionally scale the data
#Xscaled <- savitzkyGolay(X, m=0, p=3, w=27)
#X <- standardNormalVariate(Xscaled)
#X <- msc(Xscaled)
###### DATA LOAD END #########################

###### SETTING TRAINING AND TEST DATA ########
#retrieve training data
Xtrain <- X[-seq(1, nrow(X), 4),]
CLASStrain <- CLASS[-seq(1, length(CLASS), 4)]

#retrieve testing data
Xtest <- X[seq(1, nrow(X), 4),]
CLASStest <- CLASS[seq(1, length(CLASS), 4)]
###### END ###################################

#### SETTINGS START ##########################
#vector with number of trees to use in algorithm
ntrees = c(50, 100, 200, 300, 400)
# additional file append - scaling or other, will be added to result file name in form Result_addedFileAppend_rest
addedFileAppend <- "FTIR_SG_MSC_0.75"
# create directory for outputs (does nothing if it exists)
outputDir = "../Results/RF_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

# run random forest for all elements in ntrees vector
run_rf(Xtrain = Xtrain, Xtest = Xtest, CLASStrain = CLASStrain, CLASStest = CLASStest,
       ntrees = ntrees, outputDir = outputDir, addedFileAppend = addedFileAppend)
