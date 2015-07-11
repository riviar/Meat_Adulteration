##########################################
# Scales data with Savitzky-Golay filter and
# Standard Normal Variate combined and runs PCA
# Results available in Results/PCA_plots
#
# Rafal Kural
##########################################

run_pca_sg_snv_combined <- function(X, dataName) {
  require(prospectr)
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
        Xcomb <- standardNormalVariate(Xsg)
        #run pca and create plots
        pca_svd(Xcomb, samplenames, CLASS, paste(dataName, "_S-G_SNV_D", i, "_P", j, "_W", z, sep=""))
      }
    }
  }
  
}