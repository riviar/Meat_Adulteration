###################################
# Calculate variance across specific spectra
#
# X - data frame with spectral data
#
# Rafal Kural
###################################

create_variance_table <- function(X) {
  varTable <- rep(0, ncol(X))
  for (i in 1:ncol(X)) {
    varTable[i] <- var(X[,i])
  }
  #save results table in csv file
  write.csv(varTable, file = paste(outputDir, "/Variance Table.csv", sep=""))  
}