################################
# Removes columns of data where variance is
# less than specified value
#
# X - original data set
# cutOffValue - bottom value for acceptable variance
#
# Rafal Kural
################################

cut_off_by_variance <- function(X, cutOffValue) {
  varTable <- rep(0, ncol(X))
  for (i in 1:ncol(X)) {
    varTable[i] <- var(X[,i])
  }
  
  columnsToRemove <- which(varTable <= cutOffValue)
  treatedX <- X[,-columnsToRemove]
  cat(paste("Removed ", length(columnsToRemove), " spectral bands..\n", sep = ""))
  return(treatedX)
}