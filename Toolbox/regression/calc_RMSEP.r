################################
# Calculate RMSEP 
#
# Rafal Kural
################################

calc_RMSEP <- function(observed, predictions) {
  errorSum <- 0
  for (i in 1:length(observed)) {
    errorSum = errorSum + ((predictions[i] - observed[i])^2)
  }
  errorSum = errorSum / length(observed)
  return(errorSum)
}