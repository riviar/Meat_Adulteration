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
VMDataFile <- "../../Data/Videometer_allbatches_kural_format.csv"
FTIRDataFile <- "../../Data/FTIR_batch4_kural_format.csv"

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

######## Process data files and load data ######
# load file for Videometer
DATA <- load_file_for_regression(filepath = VMDataFile)

#extract wavelengths and classes for Videometer
X_VM <- DATA[[1]]
CLASS_VM <- DATA[[3]]

# load file for Videometer
DATA <- load_file_for_regression(filepath = FTIRDataFile)

#extract wavelengths and classes for Videometer
X_FTIR <- DATA[[1]]
CLASS_FTIR <- DATA[[3]]
######## END Process data files and load data ######


######## PLS-DA performance test ###############
PLSDA_VM_scores <- PLSDA_performance(X_VM, CLASS_VM, splitRatio, ncomp, iterations)
write.csv(file = paste(resultsDirectory, "/PLSDA_VM_scores.csv", sep = ""), x = PLSDA_VM_scores)

PLSDA_FTIR_scores <- PLSDA_performance(X_FTIR, CLASS_FTIR, splitRatio, ncomp, iterations)
write.csv(file = paste(resultsDirectory, "/PLSDA_FTIR_scores.csv", sep = ""), x = PLSDA_FTIR_scores)
######## END PLS-DA performance test ###########

######## PLS regression performance test #######
PLSR_VM_scores <- PLSR_performance(X_VM, CLASS_VM, splitRatio, maxncomp, iterations)
write.csv(file = paste(resultsDirectory, "/PLSR_VM_scores.csv", sep = ""), x = PLSR_VM_scores)

PLSR_FTIR_scores <- PLSR_performance(X_FTIR, CLASS_FTIR, splitRatio, maxncomp, iterations)
write.csv(file = paste(resultsDirectory, "/PLSR_FTIR_scores.csv", sep = ""), x = PLSR_FTIR_scores)
######## END PLS regression performance test ###

######## SVM performance test ##################
SVM_Radial_VM_scores <- SVM_performance(X_VM, CLASS_VM, splitRatio, "radial", iterations)
write.csv(file = paste(resultsDirectory, "/SVM_Radial_VM_scores.csv", sep = ""), x = SVM_Radial_VM_scores)

SVM_Radial_FTIR_scores <- SVM_performance(X_FTIR, CLASS_FTIR, splitRatio, "radial", iterations)
write.csv(file = paste(resultsDirectory, "/SVM_Radial_FTIR_scores.csv", sep = ""), x = SVM_Radial_FTIR_scores)

SVM_Polynomial_VM_scores <- SVM_performance(X_VM, CLASS_VM, splitRatio, "polynomial", iterations)
write.csv(file = paste(resultsDirectory, "/SVM_Polynomial_VM_scores.csv", sep = ""), x = SVM_Polynomial_VM_scores)

SVM_Polynomial_FTIR_scores <- SVM_performance(X_FTIR, CLASS_FTIR, splitRatio, "polynomial", iterations)
write.csv(file = paste(resultsDirectory, "/SVM_Polynomial_FTIR_scores.csv", sep = ""), x = SVM_Polynomial_FTIR_scores)
######## END SVM performance test ##############

######## RF performance test ###################
RF_VM_scores <- RF_performance(X_VM, CLASS_VM, splitRatio, iterations)
write.csv(file = paste(resultsDirectory, "/RF_VM_scores.csv", sep = ""), x = RF_VM_scores)

RF_FTIR_scores <- RF_performance(X_FTIR, CLASS_FTIR, splitRatio, iterations)
write.csv(file = paste(resultsDirectory, "/RF_FTIR_scores.csv", sep = ""), x = RF_FTIR_scores)
######## END RF performance test ###############

######## Plot results of test and save in pdf file ##
# create results directory if not exists
dir.create(path = resultsDirectory, showWarnings = FALSE)

# destination file
exp2_VM_FTIR_filename <- paste(resultsDirectory, "/Performance_Plot_Experiment2_VM_FTIR.pdf", sep = "")
# merge data 
exp2_VM_FTIR_data <- matrix(PLSDA_VM_scores, nrow=1)
exp2_VM_FTIR_data <- rbind(exp2_VM_FTIR_data, PLSDA_FTIR_scores, PLSR_VM_scores, PLSR_FTIR_scores,
                      SVM_Radial_VM_scores, SVM_Radial_FTIR_scores, SVM_Polynomial_VM_scores, 
                      SVM_Polynomial_FTIR_scores, RF_VM_scores, RF_FTIR_scores)
# create vector with names for data
exp2_VM_FTIR_names <- c("PLS-DA VM", "PLS-DA FTIR", "PLS-R VM", "PLS-R FTIR", "SVM Radial VM",
                        "SVM Radial FTIR", "SVM Polynomial VM", "SVM Polynomial FTIR", "RF VM", "RF FTIR")
# draw plot
generate_performance_plot(data = exp2_VM_FTIR_data, names = exp2_VM_FTIR_names, file = exp2_VM_FTIR_filename)
