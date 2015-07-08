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

