###########################################
# Loads indicated file and extracts
# key variables.
# Function is intended to use for my specific
# data formatting and regression algorithms.
# Refactoring required to use for other
# purposes.
#
# filepath - path to the file, either absolute
# or relative
#
# Returns list of data in form [X, BATCH, CLASS]
#
# Rafal Kural
###########################################

load_file_for_regression <- function(filepath) {
  # load chosen file
  DATA <- read.table(filepath, sep = ",", header = TRUE, row.names = 1)
  
  # columns to ignore when extracting wavelength data
  fieldsToIgnore <- c("Batch", "Class")
  # extract wavelength data
  X <- DATA[,!(names(DATA) %in% fieldsToIgnore)]
  
  # extract batch data
  BATCH <- as.vector(DATA$Batch)
  
  # extract classes (need numerical values only)
  CLASS <- as.vector(DATA$Class)
  # remove % signs, repeat for all classes
  CLASS[CLASS == "0%"] <- 0
  CLASS[CLASS == "10%"] <- 10
  CLASS[CLASS == "20%"] <- 20
  CLASS[CLASS == "30%"] <- 30
  CLASS[CLASS == "40%"] <- 40
  CLASS[CLASS == "50%"] <- 50
  CLASS[CLASS == "60%"] <- 60
  CLASS[CLASS == "70%"] <- 70
  CLASS[CLASS == "80%"] <- 80
  CLASS[CLASS == "90%"] <- 90
  CLASS[CLASS == "100%"] <- 100
  # change data type to numeric
  CLASS <- as.numeric(CLASS)
  
  return(list(X, BATCH, CLASS))
}