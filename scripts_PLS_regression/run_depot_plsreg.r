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
    
    #round predicted to nearest decimal number
    #further improvements would include: separate criteria for each range of values
    #some would give better results when floor'red, some when ceiling'ed, and most would need more complicated means of classification
    predictedValuesRounded <- round(predictedValues, -1)
    #create missclasification value vector (by how many % did prediction miss its class)
    missclassification <- predictedValuesRounded - CLASStest
    #display how many samples were missclasified overall
    missCount <- length(which(missclassification != 0))
    cat(paste("Missclassification occured ", missCount, " times..\n", sep = ""))
    #calculate and display RMSEP
    rmsep <- calc_RMSEP(CLASStest, predictedValues)
    cat(paste("RMSEP = ", rmsep, "\n", sep = ""))
    
    #create frame for results of test
    #table will take form: ExpectedClass, PredictedClass
    resultTable <- data.frame(CLASStest, predictedValues, missclassification, missCount, rmsep)
    #save results table in csv file
    write.csv(resultTable, file = paste(outputDir, "/Result_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
    
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
    classGrade <- as.vector(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
    classMin <- rep(0, length(classGrade))
    classMax <- rep(0, length(classGrade))
    for (i in 1:length(classGrade)) {
      classMin[i] <- min(predictedValues[id[[i]]])
      classMax[i] <- max(predictedValues[id[[i]]])
    }
    
    # generate data table
    rangeTable <- data.frame(classGrade, classMin, classMax)
    # save data table as csv file
    write.csv(rangeTable, file = paste(outputDir, "/Class_Ranges_", addedFileAppend, "_", componentsNumber_temp, "_components", ".csv", sep="")) 
    
    #generate plot of observed vs predicted values
    #prepare regression line + 2 borders
    regressionLine <- lm(CLASStest ~ predictedValues)
    CLASStrainBorderTop <- CLASStest + 10
    CLASStrainBorderBot <- CLASStest - 10
    upperBorder <- lm(CLASStrainBorderTop ~ predictedValues)
    bottomBorder <- lm(CLASStrainBorderBot ~ predictedValues)
    
    # R tries to open and close window for plots every time
    #it significantly slows down the script
    
    # create regression scatterplot
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
    #find indices of missclassified values (falling outside 10% error region)
    missclassificationIDs <- which(predictedValues > CLASStrainBorderTop)
    missclassificationIDs <- c(missclassificationIDs, which(predictedValues < CLASStrainBorderBot))
    
    plot(CLASStest[-missclassificationIDs], predictedValues[-missclassificationIDs], col="green", xlab = "Observed", ylab = "Predicted", main = "Regression scatterplot")
    points(CLASStest[missclassificationIDs], predictedValues[missclassificationIDs], col = "red")
    #add regression line and border lines
    abline(regressionLine)
    abline(upperBorder)
    abline(bottomBorder)
    
    #save plot for later use
    savedPlots[[componentsNumber_temp]] <- recordPlot()
    
    #close plot window
    dev.off()
    
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