#####################################################
# Function that tests RF regression
# Does not scale the data
# Returns vector of attained accuracy scores
# Result is considered hit if it falls within +/- 10% of its observed class
#
#
# X - matrix of all samples data (only wavelengths)
# CLASS - vector of samples classes
# ratio - ratio of how to split training/test sets
# iterations - how many times should test be repeated
# allowedDeviation - by how much can score deviate from observed value to count as hit
#
# Rafal Kural
#####################################################

RF_performance <- function(X, CLASS, ratio, iterations, allowedDeviation) {
  require(randomForest)
  source("../Toolbox/data_manipulation/pick_random_sets.r")
  
  # initialize accuracy scores vector
  accuracyScores <- 0
  
  for(i in 1:iterations) {
    
    # print current iteration
    cat(paste("RF iteration: ", i, "..\n", sep = ""))
    
    # split data into training and test sets
    randomizedSets <- pick_random_sets(X = X, CLASS = CLASS, ratio = ratio)
    Xtrain = randomizedSets$Xtrain
    CLASStrain = randomizedSets$CLASStrain
    Xtest = randomizedSets$Xtest
    CLASStest = randomizedSets$CLASStest
    
    # perform random forest cross validation in order to find optimal ntree
    testModel <- rfcv(Xtrain, CLASStrain, cv.fold = 40)
    # extract id of ntree with smallest error
    optimalNTreeID <- which(testModel$error.cv == min(testModel$error.cv))
    # train random forest model using optimal ntree
    model <- randomForest(x = Xtrain, y = CLASStrain, xtest = Xtest, ytest = CLASStest, ntree = testModel$n.var[optimalNTreeID])
    
    # extract predicted values
    predictedValues <- model$test$predicted
    
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
    accuracy <- round((hitCount/length(CLASStest)), 2)
    
    # simply push accuracy to scores vector if it is first iteration
    if (i == 1) {
      accuracyScores[i] = accuracy
    } else {
      # calculate mean of all accuracy scores up to now and push it to scores vector
      accuracyScores[i] = round(((sum(accuracyScores) + accuracy) / (length(accuracyScores) + 1)), 2)
    }
  }
  
  # return vector of accuracy scores
  return(accuracyScores)
}