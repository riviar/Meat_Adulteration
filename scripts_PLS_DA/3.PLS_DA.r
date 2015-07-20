##############################################
# Performs PLS-DA                            #
# Results available as csv files             #
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
require(plsgenomics)
require(prospectr)
source("../Toolbox/data_manipulation/load_data_from_file.r")
source("create_variance_table.r")
source("cut_off_by_variance.r")

###### LOAD DATA FROM FILE ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
#fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
#name of the data that will appear in result file name
#dataName = "VM"
fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data
#name of the data that will appear in result file name
dataName = "FTIR"

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
#X <- X[-seq(1, nrow(X), 15),]
#CLASS <- CLASS[-seq(1, nrow(X), 15)]
#X <- cut_off_by_variance(X, 0.5)

#optionally scale the data
#Xscaled <- savitzkyGolay(X, m=0, p=3, w=5)
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
# Settings allow automating analysis process
# number of latent components to use for dimension reduction, vector of values allowed
componentsNumber <- c(1:40)
# additional file append - scaling or other, will be added to result file name in form Result_addedFileAppend_rest
addedFileAppend <- "SG_SNV_0.75"
# create directory for outputs (does nothing if it exists)
outputDir = "../Results/PLSDA_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

#save accuracy scores across all ncomps
accuracyScores <- rep(0, length(componentsNumber))

for (componentsNumber_temp in componentsNumber) {
  #Display analysis status
  cat(paste("Processing data for componentNumber: ", componentsNumber_temp, "..\n", sep=""))
  
  #perform pls-da
  PLSDA <- pls.lda(Xtrain = Xtrain, Ytrain = CLASStrain, Xtest = Xtest, ncomp = componentsNumber_temp)
  
  #create hit/miss vector in form: 1 - hit, 0 - missclassification
  hitmiss <- c(rep("0", length(PLSDA$predclass)))
  for (i in 1:length(PLSDA$predclass)) {
    if (CLASStest[i] == PLSDA$predclass[i]) {
      hitmiss[i] <- "1"
    }
  }
  
  #calculate accuracy
  accuracy <- sum(as.numeric(hitmiss))/length(hitmiss)
  accuracyScores[componentsNumber_temp] <- accuracy*100
  
  #display accuracy
  cat(paste(accuracy, "\n",sep = ""))
  
  #create frame for results of test
  #table will take form: ExpectedClass, PredictedClass, HitOrMiss, OverallAccuracy
  resultTable <- data.frame(CLASStest, PLSDA$predclass, hitmiss, accuracy)
  
  #save results table in csv file
  write.csv(resultTable, file = paste(outputDir, "/Result_", dataName, "_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep=""))      
}
