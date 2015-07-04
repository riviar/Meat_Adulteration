####################################################################
#performs pca via SVD                                              #
#generates pdf file with PCA plots of first 3 PCs                  #
#X - input data matrix                                             #
#samplenames - matrix with names of samples                        #
#append_filename - what to append at the end of filename for plots #
#                                                                  #
#Rafal Kural                                                       #
####################################################################

pca_svd <- function (X, samplenames, CLASS, append_filename) {
  source("generate_plots_pdf.r")
  
  #SVD <- svd(X)
  #LOADS2 <- t(SVD$v)
  #Wraw <- SVD$d^2
  #ILOADS2 <- solve(LOADS2)
  #SCORES2 <- as.matrix(X) %*% ILOADS2
  
  #alternate svd PCA
  PCA <- prcomp(X, retX=T, center=F, scale=F)
  #extract scores
  SCORES2 <- PCA$x
  Wraw <- PCA$sdev^2
  
  generate_plots_pdf(SCORES2, samplenames, CLASS, Wraw, paste("../PCA_Plots/PCA_SVD_PLOTS_", append_filename, sep=""))
  }