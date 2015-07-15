######################################
# Runs Random Forest classification algorithm
# Saves results in csv files
#
# Xtrain - training set
# Xtest - testing set
# CLASStrain - training set classes
# CLASStest - test set classes
# ntrees - vector with number of trees
# outputDir - path to directory where results will be saved
# addedFileAppend - text to append to result files
#
# Rafal Kural
######################################

run_rf <- function(Xtrain, Xtest, CLASStrain, CLASStest , ntrees, outputDir, addedFileAppend) {
  require(randomForest)
  source("../Toolbox/regression/calc_RMSEP.r")
  
  #prepare variable for storing plots generated in loop
  #later will be retrieved and saved in single pdf file
  savedPlots <- vector(length(ntrees), mode='list')
  
  for (ntree in ntrees) {
    cat(paste("Running for ", ntree, " trees..\n", sep = ""))
    model <- randomForest(x = Xtrain, y = CLASStrain, xtest = Xtest, ytest = CLASStest, ntree = ntree)
    predictedValues <- model$test$predicted
    
    #create hit/miss vector in form: 1 - hit, 0 - missclassification
    hitmiss <- c(rep("0", length(predictedValues)))
    for (i in 1:length(predictedValues)) {
      if (CLASStest[i] == predictedValues[i]) {
        hitmiss[i] <- "1"
      }
    }
    
    
    #calculate and display RMSEP
    rmsep <- calc_RMSEP(CLASStest, predictedValues)
    cat(paste("RMSEP = ", rmsep, "\n", sep = ""))
    
    ############ SCATTERPLOT #################
    #generate plot of observed vs predicted values
    #prepare central line + 2 borders
    # copying class vector as lm does strange things if both arguments use it, no problems with copied one
    # create vector of all classes
    classVector <- unique(CLASStest)
    centralLineHelpVector <- classVector # I will never understand R
    centralLine <- lm(classVector ~ centralLineHelpVector)
    borderTop <- classVector + 10
    borderBot <- classVector - 10
    upperBorderLine <- lm(classVector ~ borderTop)
    bottomBorderLine <- lm(classVector ~ borderBot)
    
    # R tries to open and close window for plots every time
    #it significantly slows down the script
    
    # create regression scatterplot
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
    #find indices of missclassified values (falling outside 10% error region)
    # create border values vectors for CLASStest
    CLASStrainBorderTop <- CLASStest + 10
    CLASStrainBorderBot <- CLASStest - 10
    missclassificationIDs <- which(predictedValues > CLASStrainBorderTop)
    missclassificationIDs <- c(missclassificationIDs, which(predictedValues < CLASStrainBorderBot))
    
    plot(CLASStest[-missclassificationIDs], predictedValues[-missclassificationIDs], 
         xlim = c(min(CLASStest), max(CLASStest)), ylim = c(min(predictedValues), max(predictedValues)), 
         col="green", xlab = "Observed", ylab = "Predicted", 
         main = paste("RF scatterplot ", ntree, " trees", sep = ""))
    points(CLASStest[missclassificationIDs], predictedValues[missclassificationIDs], col = "red")
    #add regression line and border lines
    abline(centralLine)
    abline(upperBorderLine)
    abline(bottomBorderLine)
    
    #save plot for later use
    savedPlots[[which(ntrees == ntree)]] <- recordPlot()
    
    #close plot window
    dev.off()
    
    ################## CLASSIFICATION DATA TABLE #########################
    #create missclasification value vector (by how many % did prediction miss its class)
    missclassification <- predictedValues - CLASStest
    #calculate and display prediction accuracy (prediction is a 'hit' if within +/-10%)
    accuracy <- ((nrow(Xtest) - length(missclassificationIDs)) / nrow(Xtest)) * 100
    cat(paste("Accuracy: ", accuracy, "\n", sep = ""))
    
    #create frame for results of test
    #table will take form: ExpectedClass, PredictedClass, missclassification value, rmsep, accuracy
    resultTable <- data.frame(CLASStest, predictedValues, missclassification, rmsep, accuracy)
    #save results table in csv file
    write.csv(resultTable, file = paste(outputDir, "/Result_", addedFileAppend, "_", ntree, "_trees", ".csv", sep="")) 
    
    ################# CLASS RANGES DATA TABLE ############################
    #retrieve sample ids by classes
    id00 <- which(CLASStest == 0)
    id10 <- which(CLASStest == 10)
    id20 <- which(CLASStest == 20)
    id30 <- which(CLASStest == 30)
    id40 <- which(CLASStest == 40)
    id50 <- which(CLASStest == 50)
    id60 <- which(CLASStest == 60)
    id70 <- which(CLASStest == 70)
    id80 <- which(CLASStest == 80)
    id90 <- which(CLASStest == 90)
    id100 <- which(CLASStest == 100)
    # push ids to list of vectors
    id <- list(id00, id10, id20, id30, id40, id50, id60, id70, id80, id90, id100)
    
    # retrieve min and max predicted values for each class
    classMin <- rep(0, length(classVector))
    classMax <- rep(0, length(classVector))
    for (i in 1:length(classVector)) {
      classMin[i] <- min(predictedValues[id[[i]]])
      classMax[i] <- max(predictedValues[id[[i]]])
    }
    
    # generate data table
    rangeTable <- data.frame(classVector, classMin, classMax)
    # save data table as csv file
    write.csv(rangeTable, file = paste(outputDir, "/Class_Ranges_", addedFileAppend, "_", ntree, "_trees", ".csv", sep="")) 

  }
  
  # create pdf for plot
  pdf(paste(outputDir, "/Regression_Scatterplot_", addedFileAppend, ".pdf", sep=""))
  
  for (plot in savedPlots) {
    replayPlot(plot)
  }
  
  # close file
  dev.off()
}