##############################################
# Performs SVM                               #
# Requires running 1.PCA_analysis.r before   #
#                                            #
# Rafal Kural                                #
##############################################
require(e1071)

## !! consider ensuring that test array has representatives of ALL classes) !! ##
#retrieve training variables
Xtrain <- X[-seq(1, nrow(X), 4),] #original data
#SCORES_SVD <- SCORESrs[,1:9]
#Xtrain <- SCORES_SVD[-seq(1, nrow(SCORES_SVD), 4),] #PCA scores
CLASStrain <- CLASS[-seq(1, length(CLASS), 4)]
#retrieve test variables (testing with entire data so commented it)
#Xtest <- X[seq(1, nrow(X), 10),]
#CLASStest <- CLASS[seq(1, length(CLASS), 10)]

#### SETTINGS START ###################################
# Settings allow automating analysis process
# Kernel type polynomial/radial for SVM, allows vector of values
kernelType = c("radial", "polynomial")
# Cost - determines cost of violating separation margin
# Highes Cost values makes it harder to violate margin, default 1, allows vector of values
cost = c(0.5, 1, 2, 3, 4)
# gamma - variable influencing radial separation, default 1/numberOfVariablesPerSample, allows vector of values
gammaVar <- c(0.5, 1, 2, 3, 4, 5)
# degree - degree of polynomial used for separation in polynomial kernel, allows vector of values
degreeVar <- c(3, 4, 5, 6, 7, 8, 9, 10)
# additional file append - scaling or other, will be added to result file in form Result_VM_addFileAppend_rest
addFileAppend <- "noscale_0.75"
#### SETTINGS END ######################################

#iterate over all chosen kernels
for (kernelType_temp in kernelType) {
  cat (paste("Processing data for ", kernelType_temp, " kernel..\n", sep = ""))
  #kernel is radial
  if (kernelType_temp == "radial") {
    #create and test models for all costs and gammas
    for (cost_temp in cost) {
      cat (paste("--Processing data for cost: ", cost_temp, " ..\n", sep = ""))
      for (gammaVar_temp in gammaVar) {
        cat (paste("----Processing data for gamma: ", gammaVar_temp, " ..\n", sep = ""))
        #train model
        SVM_model <- svm(Xtrain, CLASStrain, scale=FALSE,type="C-classification",kernel=kernelType_temp, gamma=gammaVar_temp/ncol(Xtrain), cost=cost_temp)
        #predict classes
        CLASSpredicted <- (predict(SVM_model,X)) # original variables
        #CLASSpredicted <- (predict(SVM_model,SCORES_SVD)) #PCA scores
        
        #create hit/miss vector
        hitmiss <- c(rep("0", length(CLASSpredicted)))
        for (i in 1:length(CLASSpredicted)) {
          if (CLASS[i] == CLASSpredicted[i]) {
            hitmiss[i] <- "1"
          }
        }
        
        #calculate accuracy
        accuracy <- sum(as.numeric(hitmiss))/length(hitmiss)
        #display accuracy
        cat(paste(accuracy, "\n",sep = ""))
        #create frame for results of test
        resultTable <- data.frame(CLASS, CLASSpredicted, hitmiss, accuracy)
        #save results table in csv file
        write.csv(resultTable, file = paste("../SVM_results/Result_VM_", addFileAppend, "_", kernelType_temp, "_C", cost_temp, "_gamma", gammaVar_temp, ".csv", sep=""))      
        }
    }
  }
  #kernel is polynomial
  else {
    #create and test models for all costs and degrees
    for (cost_temp in cost) {
      cat (paste("--Processing data for cost: ", cost_temp, " ..\n", sep = ""))
      for (degreeVar_temp in degreeVar) {
        cat (paste("----Processing data for degree: ", degreeVar_temp, " ..\n", sep = ""))
        #train model
        SVM_model <- svm(Xtrain, CLASStrain, scale=FALSE,type="C-classification",kernel=kernelType_temp, degree=degreeVar_temp, cost=cost_temp)
        #predict classes
        CLASSpredicted <- (predict(SVM_model,X)) #original variables
        #CLASSpredicted <- (predict(SVM_model,SCORES_SVD)) #PCA scores
        
        #create hit/miss vector
        hitmiss <- c(rep("0", length(CLASSpredicted)))
        for (i in 1:length(CLASSpredicted)) {
          if (CLASS[i] == CLASSpredicted[i]) {
            hitmiss[i] <- "1"
          }
        }
        
        #calculate accuracy
        accuracy <- sum(as.numeric(hitmiss))/length(hitmiss)
        #display accuracy
        cat(paste("Accuracy: ", accuracy, "\n",sep = ""))
        #create frame for results of test
        resultTable <- data.frame(CLASS, CLASSpredicted, hitmiss, accuracy)
        #save results in csv file
        write.csv(resultTable, file = paste("../SVM_results/Result_VM_", addFileAppend, "_", kernelType_temp, "_C", cost_temp, "_degree", degreeVar_temp, ".csv", sep=""))      
      }
    }
  }
}
#train model
#SVM_model <- svm(Xtrain, CLASStrain, scale=FALSE,type="C-classification",kernel="polynomial", degree=7)
#SVM_model <- svm(Xtrain, CLASStrain, scale=FALSE,type="C-classification",kernel="radial", gamma=4/ncol(Xtrain), cost=0.5)

#predict classes
#CLASSpredicted <- (predict(SVM_model,X))

#create hit/miss vector
#hitmiss <- c(rep("0", length(CLASSpredicted)))
#for (i in 1:length(CLASSpredicted)) {
#  if (CLASS[i] == CLASSpredicted[i]) {
#    hitmiss[i] <- "1"
#  }
#}

#create frame for results of test
#resultTable <- data.frame(CLASS, CLASSpredicted, hitmiss)
#write.csv(resultTable, file = "Result_SVM_VM_noscale_Radial.csv")
