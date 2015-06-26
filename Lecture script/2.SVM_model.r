##############################################
# Performs SVM                               #
# Requires running 1.PCA_analysis.r before   #
#                                            #
# Rafal Kural                                #
##############################################
require(e1071)

## !! consider ensuring that test array has representatives of ALL classes) !! ##
#retrieve training variables
Xtrain <- X[-seq(1, nrow(X), 10),]
CLASStrain <- CLASS[-seq(1, length(CLASS), 10)]
#retrieve test variables (testing with entire data so commented it)
#Xtest <- X[seq(1, nrow(X), 10),]
#CLASStest <- CLASS[seq(1, length(CLASS), 10)]

#train model
#SVM_model <- svm(Xtrain, CLASStrain, scale=FALSE,type="C-classification",kernel="polynomial", degree=5)
SVM_model <- svm(Xtrain, CLASStrain, scale=FALSE,type="C-classification",kernel="radial", gamma=7/ncol(Xtrain))

#predict classes
CLASSpredicted <- (predict(SVM_model,X))

#create hit/miss vector
hitmiss <- c(rep("0", length(CLASSpredicted)))
for (i in 1:length(CLASSpredicted)) {
  if (CLASS[i] == CLASSpredicted[i]) {
    hitmiss[i] <- "1"
  }
}

#create frame for results of test
resultTable <- data.frame(CLASS, CLASSpredicted, hitmiss)
write.csv(resultTable, file = "Result_SVM_VM_noscale_Radial.csv")
