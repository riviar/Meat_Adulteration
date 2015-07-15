##########################################
# Scales data with Savitzky-Golay filter and
# Multivarate Scatter Correction combined and runs PCA
# Results available in Results/PCA_plots
#
# Rafal Kural
##########################################

run_pca_sg_msc_combined <- function(X, dataName) {
  require(prospectr)
  require(pls)
  source("pca_svd.r")
  
  #savitzky parameters
  derivatives <- c(0,1,2)
  polynomials <- c(3,5)
  windows <- c(5,11,17,27)
  
  #loop over params and create plots for each combination
  for (i in derivatives) {
    for (j in polynomials) {
      for (z in windows) {
        # don't try to calculate when polynomial degree is smaller than window size
        if (j >= z) {next}
        cat(paste("Loop status: D", i, "_P", j, "_W", z, "\n", sep=""))
        #run scaling
        Xsg <- savitzkyGolay(X, m = i, p = j, w = z)
        Xcomb <- msc(Xsg)
        #run pca and create plots
        pca_svd(Xcomb, samplenames, CLASS, paste(dataName, "_S-G_MSC_D", i, "_P", j, "_W", z, sep=""))
      }
    }
  }
}