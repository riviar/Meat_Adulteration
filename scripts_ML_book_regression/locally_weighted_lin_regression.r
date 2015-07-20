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

#load neccessary packages and scripts
source("../Toolbox/data_manipulation/load_file_for_regression.r")
source("lwlr.r")

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
# decay rate for
decay = 1
# additional file append - scaling or other, will be added to result file name in form Result_addedFileAppend_rest
addedFileAppend <- "decay_1"
# create directory for outputs (does nothing if it exists)
outputDir = "../Results/LWLR_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ############################

#predict values for test set
predictedValues <- rep(0, length(CLASStest))
for (i in 1:length(predictedValues)) {
  predictedValues[i] <- lwlr(as.matrix(Xtest[i,]), as.matrix(Xtrain), as.matrix(CLASStrain), decay = 1)
}

#round predicted to nearest decimal number
#further improvements would include: separate criteria for each range of values
#some would give better results when floor'red, some when ceiling'ed, and most would need more complicated means of classification
predictedValuesRounded <- round(predictedValues, -1)
#count missclassification values
missclassification <- CLASStest - predictedValuesRounded

# create result table
resultTable <- data.frame(CLASStest, predictedValues, missclassification)
#save results table in csv file
write.csv(resultTable, file = paste(outputDir, "/Result_", addedFileAppend, ".csv", sep="")) 
