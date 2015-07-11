################################################
# Locally weighted linear regression algorithm
# Sets high weights to samples around target
# that grow smaller moving away from target
#
# testSample - sample to predict, must be part
# of Xtrain, supports single sample now
# Xtrain - training matrix
# CLASStrain - vector of observed values
# decay - rate at which weights grow smaller
#
# Author: Peter Harrington
# Translated to R by: Rafal Kural
#
# note: original, though short, wanted me to 
# find this guy and teach him something about 
# naming his variables
# This being an algorithm is no excuse for do this to others
################################################

lwlr <- function(testSample, Xtrain, CLASStrain, decay) {
  # count number of samples in training set
  sampleNum <- nrow(Xtrain)
  
  # create identity matrix for weigths
  weights <- diag(sampleNum)
  
  # calculate weigths, decaying as going away from testSample
  for (i in 1:sampleNum) {
    diffMatrix <- testSample - Xtrain[i,]
    weights[i,i] <- exp(diffMatrix %*% t(diffMatrix) / (-2.0 * decay * decay))
  }
  
  # calculate matrix of something, that is a training set multiplied 
  #by its inverse that is multiplied by weight matrix
  xTx <- t(Xtrain) %*% (weights %*% Xtrain)
  
  # check if it is possible to inverse xTx, stop if not
  if (det(xTx) == 0.0) {
    cat("Cannot inverse this matrix. Stopping the algorithm..")
    return
  }
  
  ws = t(xTx) %*% (t(Xtrain) * (weights * CLASStrain))
  
  return(testSample * ws)
}