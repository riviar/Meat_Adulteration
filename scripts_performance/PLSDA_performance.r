#####################################################
# Function that tests PLS-DA classification
# Does not scale the data
# Returns vector of attained accuracy scores
# Result is considered hit if it falls within +/- 10% of its observed class
#
# X - matrix of all samples data (only wavelengths)
# CLASS - vector of samples classes
# ratio - ratio of how to split training/test sets
# ncomp - vector containing number of components to use in PLS-DA
# iterations - how many times should test be repeated
# allowProximity - TRUE to treat predicting adjecent class as a hit, 
# FALSE - only perfect hits count, default FALSE
#
# Rafal Kural
#####################################################

PLSDA_performance <- function(X, CLASS, ratio, ncomp, iterations, allowProximity) {
  require(plsgenomics)
  source("../Toolbox/data_manipulation/pick_random_sets.r")
  
  # set allowProximity to FALSE if missing
  if (missing(allowProximity)) {
    allowProximity <- FALSE
  }
  
  # initialize accuracy scores vector
  accuracyScores <- 0
  accuracyScoresMeans <- 0
  
  for(i in 1:iterations) {
  
    # print current iteration
    cat(paste("PLS-DA iteration: ", i, "..\n", sep = ""))
    
    # split data into training and test sets
    randomizedSets <- pick_random_sets(X = X, CLASS = CLASS, ratio = ratio)
    Xtrain = randomizedSets$Xtrain
    CLASStrain = randomizedSets$CLASStrain
    Xtest = randomizedSets$Xtest
    CLASStest = randomizedSets$CLASStest
    
    cat("Started generating mode..\n")
    model <- pls.lda(Xtrain = Xtrain, Ytrain = CLASStrain, Xtest = Xtest, ncomp = ncomp ,nruncv = 40)
    cat("Model finished..\n")
    
    # count how many samples were predicted correctly
    if (allowProximity) {
      # vectors creating boundary of "successfull" classification (+/- 10% of observed)
      upperSuccessCLASS <- CLASStest + 10
      bottomSuccessCLASS <- CLASStest - 10
      
      hitCount <- length(which((as.vector(model$predclass) == bottomSuccessCLASS))) + 
        length(which((as.vector(model$predclass) == upperSuccessCLASS))) + 
        length(which((as.vector(model$predclass) == CLASStest))) 
    }
    if (allowProximity == FALSE) {
      hitCount <- length(which((as.vector(model$predclass) == CLASStest)))
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