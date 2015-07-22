##############################################
# Tests performance of models created using
# SVD, PLS-DA, PLS-R, KNN, RF algorithms
# Generates pdf file with performance/stability graph
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
source("../Toolbox/data_manipulation/load_file_for_regression.r")
source("PLSDA_performance.r")
source("PLSR_performance.r")
source("SVM_performance.r")
source("RF_performance.r")
source("generate_performance_plot.r")

######## Performance test settings ############
# path to data directory
dataDirectory <- "../../Data/"
# vector of data file names (each one will get separate analysis and plot)
dataFileNames <- c("Exp1_FTIR.csv", "Exp1_GCMS.csv", "Exp1_microarray.csv", "Exp1_VM.csv",
                   "Exp2_FTIR_errors_removed", "Exp2_GCMS", "Exp2_HPLC", "Exp2_VM_errors_removed.csv")

# ratio of training/test set
splitRatio <- 0.75

# vector with numbers of latent variables (PLS-DA)
ncomp <- 4:36
# max number of components to use (PLS-R)
maxncomp <- 36

# number of iterations to repeat test
iterations <- 100

resultsDirectory = "../Results/Performance_Results"
######## END Performance test settings #########

for(i in 1:length(dataFileNames)) {
  ######## Process data files and load data ######
  # load data from file
  DATA <- load_file_for_regression(filepath = paste(dataDirectory, dataFileNames[i], sep = ""))
  
  #extract wavelengths and classes
  X <- DATA[[1]]
  CLASS <- DATA[[3]]
  ######## END Process data files and load data ######
  
  
  ######## PLS-DA performance test ###############
  PLSDA_scores <- PLSDA_performance(X, CLASS, splitRatio, ncomp, iterations)
  write.csv(file = paste(resultsDirectory, "/PLSDA_Scores_", dataFileNames[i], sep = ""), x = PLSDA_scores)
  ######## END PLS-DA performance test ###########
  
  ######## PLS regression performance test #######
  PLSR_scores <- PLSR_performance(X, CLASS, splitRatio, maxncomp, iterations)
  write.csv(file = paste(resultsDirectory, "/PLSR_Scores_", dataFileNames[i], sep = ""), x = PLSR_scores)
  ######## END PLS regression performance test ###
  
  ######## SVM performance test ##################
  SVM_Radial_scores <- SVM_performance(X, CLASS, splitRatio, "radial", iterations)
  write.csv(file = paste(resultsDirectory, "/SVM_Radial_Scores_", dataFileNames[i], sep = ""), x = SVM_Radial_scores)
  
  SVM_Polynomial_scores <- SVM_performance(X, CLASS, splitRatio, "polynomial", iterations)
  write.csv(file = paste(resultsDirectory, "/SVM_Polynomial_Scores_", dataFileNames[i], sep = ""), x = SVM_Polynomial_scores)
  ######## END SVM performance test ##############
  
  ######## RF performance test ###################
  RF_scores <- RF_performance(X, CLASS, splitRatio, iterations)
  write.csv(file = paste(resultsDirectory, "/RF_scores_", dataFileNames[i], sep = ""), x = RF_scores)
  ######## END RF performance test ###############
  
  ######## Plot results of test and save in pdf file ##
  # create results directory if not exists
  dir.create(path = resultsDirectory, showWarnings = FALSE)
  
  # destination file
  plotFilename <- paste(resultsDirectory, "/Performance_Plot_", dataFileNames[i], ".pdf", sep = "")
  # merge data 
  scoresData <- matrix(PLSDA_scores, nrow=1)
  scoresData <- rbind(scoresData, PLSR_scores, SVM_Radial_scores, 
                             SVM_Polynomial_scores, RF_scores)
  # create vector with names for data
  scoresNames <- c("PLS-DA", "PLS-R", "SVM Radial", "SVM Polynomial", "RF")
  # draw plot
  generate_performance_plot(data = scoresData, names = scoresNames, file = plotFilename)
}
