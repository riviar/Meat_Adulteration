##############################################
# Performs classification by KNN
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
require(class)
source("../Toolbox/data_reading/load_data_from_file.r")
source("run_knn.r")

###### LOAD DATA FROM FILE ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
#name of the data that will appear in result file name
dataName = "VM"
#fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data
#name of the data that will appear in result file name
#dataName = "FTIR"

# load chosen file
DATA <- load_data_from_file(fileToLoad)

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
#Xscaled <- savitzkyGolay(X, m=0, p=4, w=5)
#X <- standardNormalVariate(Xscaled)
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
#number of neighbours considered by algorithm
numberOfNeighboursVector <- c(2,3,4,5,6,7)
# additional file append - scaling or other, will be added to result file name in form Result_addedFileAppend_rest
addedFileAppend <- paste(dataName, "_SG_SNV_0.75", sep = "")
# create directory for outputs (does nothing if it exists)
outputDir = "../Results/KNN_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

#run knn for all elements in numberOfNeighboursVector
run_knn(Xtrain = Xtrain, Xtest = Xtest, CLASStrain = CLASStrain, 
        numberOfNeighboursVector = numberOfNeighboursVector, 
        resultDirectory = outputDir, appendText = addedFileAppend)