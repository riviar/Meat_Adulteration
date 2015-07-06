##############################################
# Performs PLS-DA                            #
# Results available as csv files             #
#                                            #
# Rafal Kural                                #
##############################################
rm(list = ls())
graphics.off()
require(plsgenomics)

#set working directory to script directory
setwd("~/Thesis/R scripts/Lecture script")

###### LOAD DATA FROM FILE ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
#fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data

# load chosen file
DATA <- read.table(fileToLoad, sep = ",", header = TRUE, row.names = 1)

# columns to ignore when extracting wavelength data
fieldsToIgnore <- c("Batch", "Class")
# extract wavelength data
X <- DATA[,!(names(DATA) %in% fieldsToIgnore)]

# extract classes
CLASS <- DATA$Class
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
addedFileAppend <- "noscale_VM_0.75"
# create directory for outputs (does nothing if it exists)
outputDir = "../PLSDA_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

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
  #display accuracy
  cat(paste(accuracy, "\n",sep = ""))
  #create frame for results of test
  #table will take form: ExpectedClass, PredictedClass, HitOrMiss, OverallAccuracy
  resultTable <- data.frame(CLASStest, PLSDA$predclass, hitmiss, accuracy)
  #save results table in csv file
  write.csv(resultTable, file = paste(outputDir, "/Result_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep=""))      
}
