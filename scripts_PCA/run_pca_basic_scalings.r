##########################################
# Runs basic PCA with simple scalings on the data
# Scalings used: autoscale, meanscale, rangecentering
# Results available in Results/PCA_plots
#
# Rafal Kural
##########################################

run_pca_basic_scalings <- function(X, dataName) {
  require(matlab)
  source("pca_svd.r")
  source("../Toolbox/scalings/auto.r")
  source("../Toolbox/scalings/mncn.r")
  source("../Toolbox/scalings/rangescale.r")
  #simple scalings
  Xas <- auto(X)
  Xmncn <- mncn(X)
  Xrs <- rangescale(X)
  #noscale
  pca_svd(X, samplenames, CLASS, paste(dataName, "noscale", sep="_"))
  #autoscale
  pca_svd(Xas, samplenames, CLASS, paste(dataName, "autoscale", sep="_"))
  #meancentered
  pca_svd(Xmncn, samplenames, CLASS, paste(dataName, "meancentered", sep="_"))
  #rangescaled
  pca_svd(Xrs, samplenames, CLASS, paste(dataName, "rangescale", sep="_"))
}