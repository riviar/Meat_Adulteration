##############################################
# Performs PLSDA with split prediction       #
# Requires running 1.PCA_analysis.r before   #
#                                            #
# Rafal Kural                                #
##############################################
rm(list = ls())
graphics.off()
require(plsgenomics)

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

###### SETTING TRAINING AND TEST DATA ####################
#retrieve training variables
Xtrain <- X[-seq(1, nrow(X), 4),] #original data
#SCORES_SVD <- SCORESrs[,1:9]
#Xtrain <- SCORES_SVD[-seq(1, nrow(SCORES_SVD), 4),] #PCA scores
CLASStrain <- CLASS[-seq(1, length(CLASS), 4)]
#setting test arrays (use either full data or part of it)
Xtest <- X[seq(1, nrow(X), 4),]
CLASStest <- CLASS[seq(1, length(CLASS), 4)]
#Xtest <- X
#CLASStest <- CLASS
###### END ###############################################

#### SETTINGS START ###################################
# Settings allow automating analysis process
# additional file append - scaling or other, will be added to result file in form Result_addFileAppend_rest
addFileAppend <- "noscale_VM_0.75_SPLITPRED"
# create directory for outputs (does nothing if it exists)
outputDir = "../PLSDA_results"
dir.create(path = outputDir, showWarnings = FALSE)
#### SETTINGS END ######################################

#retrieve sample ids by classes for split predictions
id00 <- grep("^0%", CLASStrain)
id10 <- grep("^10%", CLASStrain)
id20 <- grep("20%", CLASStrain)
id30 <- grep("30%", CLASStrain)
id40 <- grep("40%", CLASStrain)
id50 <- grep("50%", CLASStrain)
id60 <- grep("60%", CLASStrain)
id70 <- grep("70%", CLASStrain)
id80 <- grep("80%", CLASStrain)
id90 <- grep("90%", CLASStrain)
id100 <- grep("100%", CLASStrain)

#perform pls-da, ncom=30 optimal
PLSDA <- pls.lda(Xtrain = Xtrain, Ytrain = CLASStrain, Xtest = Xtest, ncomp = 30)
  
#initialize final predicted classes vector
predictedFinal <- rep(0, length(PLSDA$predclass))

# repeat prediction for all results with partial models
for(i in 1:length(PLSDA$predclass)) {
  cat(paste("Processing data for test sample nr: ", i, " ..\n", sep=""))
  predclass <- PLSDA$predclass[i]
  
  #chose training set classes to be adjecent to predicted
  if (predclass == "0%" || predclass == "10%") {
    Xsplit <- Xtrain[c(id00, id10, id20),]
    CLASSsplit <- CLASStrain[c(id00, id10, id20)]
  }
  else if (predclass == "20%") { 
    Xsplit <- Xtrain[c(id10, id20, id30),]
    CLASSsplit <- CLASStrain[c(id10, id20, id30)]
  }
  else if (predclass == "30%") { 
    Xsplit <- Xtrain[c(id20, id30, id40),]
    CLASSsplit <- CLASStrain[c(id20, id30, id40)]
  }
  else if (predclass == "40%") { 
    Xsplit <- Xtrain[c(id30, id40, id50),]
    CLASSsplit <- CLASStrain[c(id30, id40, id50)]
  }
  else if (predclass == "50%") { 
    Xsplit <- Xtrain[c(id40, id50, id60),]
    CLASSsplit <- CLASStrain[c(id40, id50, id60)]
  }
  else if (predclass == "60%") { 
    Xsplit <- Xtrain[c(id50, id60, id70),]
    CLASSsplit <- CLASStrain[c(id50, id60, id70)]
  }
  else if (predclass == "70%") { 
    Xsplit <- Xtrain[c(id60, id70, id80),]
    CLASSsplit <- CLASStrain[c(id60, id70, id80)]
  }
  else if (predclass == "80%") { 
    Xsplit <- Xtrain[c(id70, id80, id90),]
    CLASSsplit <- CLASStrain[c(id70, id80, id90)]
  }
  else if (predclass == "90%" || predclass == "100%") { 
    Xsplit <- Xtrain[c(id80, id90, id100),]
    CLASSsplit <- CLASStrain[c(id80, id90, id100)]
  }
  #perform pls-da with best number of latent components determined by internal cross-validation
  PLSDAsplit <- pls.lda(Xtrain = Xsplit, Ytrain = CLASSsplit, Xtest = Xtest[i,], ncomp = 1:40, nruncv = 20)
  #push predicted value to new vector of predictions, output is in weird format, need to treat it with paste
  predictedFinal[i] <- as.character(PLSDAsplit$predclass[1])
}


#create hit/miss vector
hitmiss <- c(rep("0", length(predictedFinal)))
for (i in 1:length(predictedFinal)) {
  if (CLASStest[i] == predictedFinal[i]) {
    hitmiss[i] <- "1"
  }
}
  
#calculate accuracy
accuracy <- sum(as.numeric(hitmiss))/length(hitmiss)
#display accuracy
cat(paste(accuracy, "\n",sep = ""))
#create frame for results of test
resultTable <- data.frame(CLASStest, predictedFinal, hitmiss, accuracy)
#save results table in csv file
write.csv(resultTable, file = paste(outputDir, "/Result_", addFileAppend, ".csv", sep=""))      