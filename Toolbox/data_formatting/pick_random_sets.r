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
  # no idea how to do it in separate function
  # and return uneven matrices
  # it IS doable, R just won't tell me how
  
  #vectors of samples indices
  i00 = which(CLASS == 0)
  i10 = which(CLASS == 10)
  i20 = which(CLASS == 20)
  i30 = which(CLASS == 30)
  i40 = which(CLASS == 40)
  i50 = which(CLASS == 50)
  i60 = which(CLASS == 60)
  i70 = which(CLASS == 70)
  i80 = which(CLASS == 80)
  i90 = which(CLASS == 90)
  i100 = which(CLASS == 100)
  
  # create matrices of split samples
  X00 = X[i00,]
  X10 = X[i10,]
  X20 = X[i20,]
  X30 = X[i30,]
  X40 = X[i40,]
  X50 = X[i50,]
  X60 = X[i60,]
  X70 = X[i70,]
  X80 = X[i80,]
  X90 = X[i90,]
  X100 = X[i100,]
  ####### X SPLITTING END #######
  
  ####### GENERATE TRAINING SET ##########
  # determine target length of test and training sets
  trainLength <- round(ratio * nrow(X))
  testLength <- nrow(X) - trainLength #may not be needed
  
  #initialize variables
  Xtrain = NULL
  CLASStrain = NULL
  
  # fill training set with values
  for (i in 1:trainLength) {
    #set new seed for random numbers based on system time and process id
    set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
    
    # ensure that training set will have samples from all classes
    # it is done by selecting from different class every loop iteration
    if ((i %% length(unique(CLASS))) == 0) {
      randomID <- round(runif(1, 1, nrow(X00)))
      Xtrain <- rbind(Xtrain, X00[randomID,])
      CLASStrain <- c(CLASStrain, 0)
      X00 <- X00[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 1) {
      randomID <- round(runif(1, 1, nrow(X10)))
      Xtrain <- rbind(Xtrain, X10[randomID,])
      CLASStrain <- c(CLASStrain, 10)
      X10 <- X10[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 2) {
      randomID <- round(runif(1, 1, nrow(X20)))
      Xtrain <- rbind(Xtrain, X20[randomID,])
      CLASStrain <- c(CLASStrain, 20)
      X20 <- X20[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 3) {
      randomID <- round(runif(1, 1, nrow(X30)))
      Xtrain <- rbind(Xtrain, X30[randomID,])
      CLASStrain <- c(CLASStrain, 30)
      X30 <- X30[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 4) {
      randomID <- round(runif(1, 1, nrow(X40)))
      Xtrain <- rbind(Xtrain, X40[randomID,])
      CLASStrain <- c(CLASStrain, 40)
      X40 <- X40[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 5) {
      randomID <- round(runif(1, 1, nrow(X50)))
      Xtrain <- rbind(Xtrain, X50[randomID,])
      CLASStrain <- c(CLASStrain, 50)
      X50 <- X50[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 6) {
      randomID <- round(runif(1, 1, nrow(X60)))
      Xtrain <- rbind(Xtrain, X60[randomID,])
      CLASStrain <- c(CLASStrain, 60)
      X60 <- X60[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 7) {
      randomID <- round(runif(1, 1, nrow(X70)))
      Xtrain <- rbind(Xtrain, X70[randomID,])
      CLASStrain <- c(CLASStrain, 70)
      X70 <- X70[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 8) {
      randomID <- round(runif(1, 1, nrow(X80)))
      Xtrain <- rbind(Xtrain, X80[randomID,])
      CLASStrain <- c(CLASStrain, 80)
      X80 <- X80[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 9) {
      randomID <- round(runif(1, 1, nrow(X90)))
      Xtrain <- rbind(Xtrain, X90[randomID,])
      CLASStrain <- c(CLASStrain, 90)
      X90 <- X90[-randomID,]
    }
    else if ((i %% length(unique(CLASS))) == 10) {
      randomID <- round(runif(1, 1, nrow(X100)))
      Xtrain <- rbind(Xtrain, X100[randomID,])
      CLASStrain <- c(CLASStrain, 100)
      X100 <- X100[-randomID,]
    }
  }
  
  # push remaining samples to test set
  Xtest <- rbind(X00, X10, X20, X30, X40, X50, X60, X70, X80, X90, X100)
  CLASStest <- c(rep(0, nrow(X00)), rep(10, nrow(X10)),
                 rep(20, nrow(X20)), rep(30, nrow(X30)),
                 rep(40, nrow(X40)), rep(50, nrow(X50)),
                 rep(60, nrow(X60)), rep(70, nrow(X70)),
                 rep(80, nrow(X80)), rep(90, nrow(X90)),
                 rep(100, nrow(X100)))
  
  pickedSets <- list(Xtrain, CLASStrain, Xtest, CLASStest)
}