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
source("generate_loading_plot_pdf.r")
source("../Toolbox/data_reading/load_file_for_regression.r")

###### LOAD DATA FROM FILE ###################
# name of csv file in format of data: ID/wavelengths/Batch/Class
fileToLoad = "../../Data/Videometer_allbatches_kural_format.csv" #VideometerLab data
#fileToLoad = "../../Data/FTIR_batch4_kural_format.csv" #FTIR data

# load chosen file
DATA <- load_file_for_regression(fileToLoad)
X <- DATA[[1]]
CLASS <- DATA[[3]]
###### DATA LOAD END #########################

###### SETTING TRAINING AND TEST DATA ########
#retrieve training data
Xtrain <- X[-seq(1, nrow(X), 4),]
CLASStrain <- CLASS[-seq(1, length(CLASS), 4)]

#retrieve testing data
Xtest <- X[-seq(1, nrow(X), 4),]
CLASStest <- CLASS[-seq(1, nrow(X), 4)]
###### END ###################################

#### SETTINGS START ##########################
# Settings allow automating analysis process
# number of latent components to use for dimension reduction, vector of values allowed
componentsNumber <- c(1:40)
# additional file append - scaling or other, will be added to result file name in form Result_addedFileAppend_rest
addedFileAppend <- "noscale_0.75"
# create directory for outputs (does nothing if it exists)
outputDir = "../Results/PLSREGR_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

# run algorithms for all components
for (componentsNumber_temp in componentsNumber) {
  #Display analysis status
  cat(paste("Processing data for componentNumber: ", componentsNumber_temp, "..\n", sep=""))
  
  #perform pls regression
  #PLSregr <- pls.regression(Xtrain = Xtrain, Ytrain = CLASStrain, Xtest = Xtest, ncomp = componentsNumber_temp)
  #predictedValues <- PLSregr$Ypred
  #PLSRmodel <- plsr(class ~ sample, data = Xtrain, ncomp = componentsNumber_temp)
  #predictedValues <- predict(PLSRmodel, newdata = Xtest, ncomp = componentsNumber_temp)
  PLSregr <- plsreg1(Xtrain, CLASStrain, comps = componentsNumber_temp)
  predictedValues <- PLSregr$y.pred
  
  #round predicted to nearest decimal number
  #further improvements would include: separate criteria for each range of values
  #some would give better results when floor'red, some when ceiling'ed, and most would need more complicated means of classification
  predictedValuesRounded <- round(predictedValues, -1)
  #create missclasification value vector (by how many % did prediction miss its class)
  missclassification <- predictedValuesRounded - CLASStrain
  #display how many samples were missclasified overall
  missCount <- length(which(missclassification != 0))
  cat(paste("Missclassification occured ", missCount, " times..\n", sep = ""))
  
  #create frame for results of test
  #table will take form: ExpectedClass, PredictedClass
  resultTable <- data.frame(CLASStest, predictedValues, missclassification, missCount)
  #save results table in csv file
  write.csv(resultTable, file = paste(outputDir, "/Result_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
  
  #retrieve sample ids by classes
  id00 <- which(CLASStest == 0)
  id10 <- which(CLASStest == 10)
  id20 <- which(CLASStest == 20)
  id30 <- which(CLASStest == 30)
  id40 <- which(CLASStest == 40)
  id50 <- which(CLASStest == 50)
  id60 <- which(CLASStest == 60)
  id70 <- which(CLASStest == 70)
  id80 <- which(CLASStest == 80)
  id90 <- which(CLASStest == 90)
  id100 <- which(CLASStest == 100)
  # push ids to list of vectors
  id <- list(id00, id10, id10, id30, id40, id50, id60, id70, id80, id90, id100)
  
  # retrieve min and max predicted values for each class
  classGrade <- as.vector(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  classMin <- rep(0, length(classGrade))
  classMax <- rep(0, length(classGrade))
  for (i in 1:length(classGrade)) {
    classMin[i] <- min(predictedValues[id[[i]]])
    classMax[i] <- max(predictedValues[id[[i]]])
  }
  
  # generate data table
  rangeTable <- data.frame(classGrade, classMin, classMax)
  # save data table as csv file
  write.csv(rangeTable, file = paste(outputDir, "/Class_Ranges_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
  
}

# create pdf with loadings plots
#generate_loading_plot_pdf(LOADINGS = PLSregr$P, filename = paste(outputDir, "/Loadings_Plot_", addedFileAppend, "_.pdf", sep=""), ncompNum = length(componentsNumber))