##############################################
# Tests performance of models created using
# SVD, PLS-DA, PLS-R, KNN, RF algorithms
# Generates pdf file with performance/stability graph
#
# Add parallelization if possible (should be)
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
source("run_Ptest.r")

# parallelisation imports
library(foreach)
library(doMC)

######## Performance test settings ############
# number of CPU cores to use
registerDoMC(2) 
# path to data directory
dataDirectory <- "../../Data/"
# vector of data file names (each one will get separate analysis and plot)
dataFileNames <- c("Exp2_GCMS_corrected_NAs.csv", "Exp2_HPLC.csv", 
                   "Exp1_GCMS_corrected_NAs.csv", "Exp1_VM.csv", "Exp1_microarray.csv")
# vector of proximity settings for each data file 
#(exp1 has only beef/mixed/pork classes so we need to include only direct hits)
proximityAllowed <- c(TRUE, TRUE, FALSE, FALSE, FALSE)


# vector of proximity scores for regression (by how much can result deviate to count as hit)
# Exp1 has classes 0-beef/1-mixed/2-pork
# Exp 2 has classes 0/10/20/../100
allowedDeviation <- c(10, 10, 0.5, 0.5, 0.5)

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

# run analysis for all files in parallel foreach
foreach(i=1:length(dataFileNames)) %do% {
  run_Ptest(dataDirectory = dataDirectory, dataFileNames = dataFileNames[i], ratio = splitRatio,
            ncomp = ncomp, maxncomp = maxncomp, iterations = iterations,
            proximityAllowed = proximityAllowed[i], allowedDeviation = allowedDeviation[i])
}
