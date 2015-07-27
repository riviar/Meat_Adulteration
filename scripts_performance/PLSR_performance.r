#####################################################
# Function that tests PLS regression
# Does not scale the data
# Returns vector of attained accuracy scores
# Result is considered hit if it falls within +/- 10% of its observed class
#
#
# X - matrix of all samples data (only wavelengths)
# CLASS - vector of samples classes
# ratio - ratio of how to split training/test sets
# maxncomp - maximum number of components to use
# iterations - how many times should test be repeated
# allowedDeviation - by how much can score deviate from observed value to count as hit
#
# Rafal Kural
#####################################################

PLSR_performance <- function(X, CLASS, ratio, maxncomp, iterations, allowedDeviation) {
  require(plsdepot)
  source("../Toolbox/data_manipulation/pick_random_sets.r")
  
  # initialize accuracy scores vector
  accuracyScores <- 0
  accuracyScoresMeans <- 0
    
  for(i in 1:iterations) {
    
    # print current iteration
    cat(paste("PLS-R iteration: ", i, "..\n", sep = ""))
    
    # split data into training and test sets
    randomizedSets <- pick_random_sets(X = X, CLASS = CLASS, ratio = ratio)
    Xtrain = randomizedSets$Xtrain
    CLASStrain = randomizedSets$CLASStrain
    Xtest = randomizedSets$Xtest
    CLASStest = randomizedSets$CLASStest
    
    # create model and predict test classes
    model <- plsreg1(Xtrain, CLASStrain, comps = maxncomp, crosval = TRUE)
    regressionCoefficients <- as.matrix(model$reg.coefs[-1])
    regressionIntercept <- model$reg.coefs[1]
    predictedValues <- as.matrix(Xtest) %*% regressionCoefficients + regressionIntercept
    
    # vectors creating boundary of "successfull" classification
    upperSuccessCLASS <- CLASStest + allowedDeviation
    bottomSuccessCLASS <- CLASStest - allowedDeviation
    
    # count how many samples were predicted correctly
    #hitCount <- length(which((as.vector(model$predclass) >= bottomSuccessCLASS) & 
    #                    (as.vector(model$predclass) <= upperSuccessCLASS)))
    #hitCount <- length(which((as.vector(model) == bottomSuccessCLASS))) + 
    #  length(which((as.vector(model) == upperSuccessCLASS))) + 
    #  length(which((as.vector(model) == CLASStest))) 
    
    #none of above worked well, so..
    hitCount <- 0
    for(j in 1:length(predictedValues)) {
      if (predictedValues[j] >= bottomSuccessCLASS[j] &&
          predictedValues[j] <= upperSuccessCLASS[j]) {
        hitCount = hitCount + 1
      }
    }
    
    # calculate accuracy percentage and round to 2 decimal places
    accuracy <- round((hitCount/length(CLASStest)), 4)
    
    # simply push accuracy to scores vector
    accuracyScores[i] = accuracy
    # calculate mean of all accuracy scores up to now and push it to scores mean vector
    accuracyScoresMeans[i] = round((sum(accuracyScores) / length(accuracyScores)), 4)
  }
  
  # return vector of accuracy scores
  return(accuracyScoresMeans)
}