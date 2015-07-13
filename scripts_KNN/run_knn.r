######################################
# Runs KNN classification algorithm
# Saves results in csv files
#
# Xtrain - training set
# Xtest - testing set
# CLASStrain - training set classes
# numberOfNeighboursVector - vector with number of neighbours to consider
# resultDirectory - path to directory where results will be saved
# appendText - text to append to result files
#
# Rafal Kural
######################################

run_knn <- function(Xtrain, Xtest, CLASStrain, numberOfNeighboursVector, resultDirectory, appendText) {
  for (numberOfNeighbours in numberOfNeighboursVector) {
    cat(paste("Running for ", numberOfNeighbours, " neighbours..\n", sep = ""))
    model <- knn(train = Xtrain, test = Xtest, cl = CLASStrain, k = numberOfNeighbours)
    
    #create hit/miss vector in form: 1 - hit, 0 - missclassification
    hitmiss <- c(rep("0", length(model)))
    for (i in 1:length(model)) {
      if (CLASStest[i] == model[i]) {
        hitmiss[i] <- "1"
      }
    }
    
    #calculate accuracy
    accuracy <- sum(as.numeric(hitmiss))/length(hitmiss)
    
    #display accuracy
    cat(paste(accuracy, "\n",sep = ""))
    
    #create frame for results of test
    #table will take form: ExpectedClass, PredictedClass, HitOrMiss, OverallAccuracy
    resultTable <- data.frame(CLASStest, model, hitmiss, accuracy)
    
    #save results table in csv file
    write.csv(resultTable, file = paste(resultDirectory, "/Result_", appendText, "_K", numberOfNeighbours, ".csv", sep=""))  
  }
}