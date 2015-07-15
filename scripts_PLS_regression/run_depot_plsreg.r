##################################################
# Runs PLS regression using algorithms from 
# plsdepot package.
# 1. Creates files with observed - predicted comparison
# Includes information about accuracy
# 2. Creates files with class association border values
# Does 1. and 2. for each component number from ncomp
#
# Xtrain - matrix with training data for regression
# CLASStrain - vector of observed values (in intended case
# translated class association)
# ncomp - vector of component numbers for which algorithms
# should be ran
# outputDir - path to the directory where results will be saved
#
# Rafal Kural
##################################################


run_depot_plsreg <- function(Xtrain, CLASStrain, Xtest, CLASStest, ncomp, outputDir) {
  require(plsdepot)
  source("calc_RMSEP.r")

  #prepare variable for storing plots generated in loop
  #later will be retrieved and saved in single pdf file
  savedPlots <- vector(length(ncomp), mode='list')
  
  for (componentsNumber_temp in ncomp) {
    #Display analysis status
    cat(paste("Processing data for componentNumber: ", componentsNumber_temp, "..\n", sep=""))
    
    #perform pls regression
    PLSregr <- plsreg1(Xtrain, CLASStrain, comps = componentsNumber_temp)
    #predictedValues <- PLSregr$y.pred #those predictions are for training set
    #predict values for testing set
    regressionCoefficients <- as.matrix(PLSregr$reg.coefs[-1])
    regressionIntercept <- PLSregr$reg.coefs[1]
    predictedValues <- as.matrix(Xtest) %*% regressionCoefficients + regressionIntercept
    
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
         col="green", xlab = "Observed", ylab = "Predicted", main = "Regression scatterplot")
    points(CLASStest[missclassificationIDs], predictedValues[missclassificationIDs], col = "red")
    #add regression line and border lines
    abline(centralLine)
    abline(upperBorderLine)
    abline(bottomBorderLine)
    
    #save plot for later use
    savedPlots[[componentsNumber_temp]] <- recordPlot()
    
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
    write.csv(resultTable, file = paste(outputDir, "/Result_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
    
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
    write.csv(rangeTable, file = paste(outputDir, "/Class_Ranges_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
    
    ################### ERRORS DATA TABLE ###########################
    #save errors table
    write.csv(PLSregr$Q2, file = paste(outputDir, "/Errors_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
    
    
  }
  
  # create pdf for plot
  pdf(paste(outputDir, "/Regression_Scatterplot_", addedFileAppend, ".pdf", sep=""))
  
  for (plot in savedPlots) {
    replayPlot(plot)
  }
  
  # close file
  dev.off()
}