#####################################################################
#performs pca via eigenvectors                                      #
#generates pdf files with PCA plots of first 3 PCs                  #
#X - input data matrix                                              #
#samplenames - matrix with names of samples                         #
#append_filename - what to append at the end of filename for plots  #
#saves pca plots one directory above in Results/PLA_Plots           #
#                                                                   #
#Rafal Kural                                                        #
#####################################################################
pca_eigen <- function(X, samplenames, CLASS, append_filename) {
  source("generate_plots_pdf.r")
  
  COVmatrix <- cov(X)
  EIGEN <- eigen(COVmatrix)
  LOADS1 <- t(EIGEN$vectors)
  W1 <- EIGEN$values
  ILOADS1 <- solve(LOADS1)
  SCORES1 <- as.matrix(X) %*% ILOADS1
  
  # create directory for outputs (does nothing if it exists)
  outputDir = "../Results/PCA_Plots"
  dir.create(path = outputDir, showWarnings = FALSE)
  
  generate_plots_pdf(SCORES1, samplenames, CLASS, W1, paste(outputDir, "/PCA_EIGEN_PLOTS_", append_filename, sep=""))
}