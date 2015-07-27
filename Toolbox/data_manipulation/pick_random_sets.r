########################################################
# Creates training and testing sets with random samples
# Ensures that training set has representatives of all classes
# Refactorium needum. But later ~~ Works fine anyway
# 
# X - matrix of samples values
# CLASS - vector of samples classes (numeric vector needed)
# ratio - ratio of training-to-test split (default 0.75)
#
# Returns sets as a list in form TrainSet/TrainClass/TestSet/TestClass
#
# Rafal Kural
########################################################

# main function
pick_random_sets <- function(X, CLASS, ratio) {
  
  # set missimg values to defaults
  if (missing(ratio)) {
    ratio <- 0.75
  }

  ####### SPLIT X BY CLASSES #######
  
  #find unique types of classes
  classTypes <- unique(CLASS)
  
  # create list of sample matrices split by classes
  Xlist <- list(X[which(CLASS == classTypes[1]),])
  for (j in 2:length(classTypes)) {
    Xlist[[j]] <- (X[which(CLASS == classTypes[j]),])
  }
  ####### X SPLITTING END #######
  
  ####### GENERATE TRAINING SET ##########
  # determine target length of test set
  trainLength <- round(ratio * nrow(X))
  
  #initialize variables
  Xtrain = NULL
  CLASStrain = NULL
  
  # fill training set with values
  for (i in 1:trainLength) {
    #set new seed for random numbers based on system time and process id
    set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
    
    # ensure that training set will have samples from all classes
    # it is done by selecting from different class every loop iteration
    # I am proud of how difficult to understand I made this part
    # But I cut this loop code to 1/10nth
      # pick class from which a random sample will be selected
      targetClass <- (i %% (length(classTypes))) + 1
      #ensure that doesn't try to retrieve from already empty class
      counter = i
      while(TRUE) {
        # select random ID from target class list
        randomID <- round(runif(1, 1, nrow(Xlist[[targetClass]])))
        if (!(is.nan(randomID))) {
          break
        }
        # try to pick from next class
        targetClass <- (counter %% (length(classTypes))) + 1
        counter = counter + 1
      }
      
      # push sample of chosen ID to training set
      Xtrain <- rbind(Xtrain, Xlist[[targetClass]][randomID,])
      
      # push sample class of chosen to training set
      CLASStrain <- c(CLASStrain, classTypes[targetClass])
      
      # remove chosen sample from main data list
      Xlist[[targetClass]] <- Xlist[[targetClass]][-randomID,]
  }
  
  # push remaining samples to test set
  Xtest = NULL
  CLASStest = NULL
  for(i in 1:length(classTypes)) {
    for(j in 1:nrow(Xlist[[i]])) {
      # do not try to append if class is empty
      if(is.na(Xlist[[i]][j,1])) {
        break
      }
      Xtest <- rbind(Xtest, Xlist[[i]][j,])
      CLASStest <- c(CLASStest, classTypes[i])
    }
  }
  
  pickedSets <- list(Xtrain = Xtrain, CLASStrain = CLASStrain, Xtest = Xtest, CLASStest = CLASStest)
  return(pickedSets)
}