########################################
# Wrapper for parallelization of Performance_Test
# Runs entire workflow for one file
#
# Rafal Kural
########################################

run_Ptest <- function(dataDirectory, dataFileNames, ratio, ncomp, maxncomp, iterations,
                      proximityAllowed, allowedDeviation) {
  ######## Process data files and load data ######
  # load data from file
  DATA <- load_file_for_regression(filepath = paste(dataDirectory, dataFileNames, sep = ""))
  
  #extract wavelengths and classes
  X <- DATA[[1]]
  CLASS <- DATA[[3]]
  ######## END Process data files and load data ######
  
  
  ######## PLS-DA performance test ###############
  PLSDA_scores <- PLSDA_performance(X, CLASS, splitRatio, ncomp, iterations, proximityAllowed)
  write.csv(file = paste(resultsDirectory, "/PLSDA_Scores_", dataFileNames, sep = ""), x = PLSDA_scores)
  ######## END PLS-DA performance test ###########
  
  ######## PLS regression performance test #######
  PLSR_scores <- PLSR_performance(X, CLASS, splitRatio, maxncomp, iterations, allowedDeviation)
  write.csv(file = paste(resultsDirectory, "/PLSR_Scores_", dataFileNames, sep = ""), x = PLSR_scores)
  ######## END PLS regression performance test ###
  
  ######## SVM performance test ##################
  SVM_Radial_scores <- SVM_performance(X, CLASS, splitRatio, "radial", iterations, allowProximity = proximityAllowed)
  write.csv(file = paste(resultsDirectory, "/SVM_Radial_Scores_", dataFileNames, sep = ""), x = SVM_Radial_scores)
  
  SVM_Polynomial_scores <- SVM_performance(X, CLASS, splitRatio, "polynomial", iterations, allowProximity = proximityAllowed)
  write.csv(file = paste(resultsDirectory, "/SVM_Polynomial_Scores_", dataFileNames, sep = ""), x = SVM_Polynomial_scores)
  ######## END SVM performance test ##############
  
  ######## RF performance test ###################
  RF_scores <- RF_performance(X, CLASS, splitRatio, iterations, allowedDeviation)
  write.csv(file = paste(resultsDirectory, "/RF_scores_", dataFileNames, sep = ""), x = RF_scores)
  ######## END RF performance test ###############
  
  ######## Plot results of test and save in pdf file ##
  # create results directory if not exists
  dir.create(path = resultsDirectory, showWarnings = FALSE)
  
  # destination file
  plotFilename <- paste(resultsDirectory, "/Performance_Plot_", dataFileNames, ".pdf", sep = "")
  # merge data 
  scoresData <- matrix(PLSDA_scores, nrow=1)
  scoresData <- rbind(scoresData, PLSR_scores, SVM_Radial_scores, 
                      SVM_Polynomial_scores, RF_scores)
  # create vector with names for data
  scoresNames <- c("PLS-DA", "PLS-R", "SVM Radial", "SVM Polynomial", "RF")
  # draw plot
  generate_performance_plot(data = scoresData, names = scoresNames, file = plotFilename)
}