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

load_data_from_file <- function(filepath) {
  # load chosen file
  DATA <- read.table(fileToLoad, sep = ",", header = TRUE, row.names = 1)
  
  # columns to ignore when extracting wavelength data
  fieldsToIgnore <- c("Batch", "Class")
  # extract wavelength data
  X <- DATA[,!(names(DATA) %in% fieldsToIgnore)]
  
  # extract batch data
  BATCH <- as.vector(DATA$Batch)
  
  # extract classes (need numerical values only)
  CLASS <- as.vector(DATA$Class)
  
  return(list(X, BATCH, CLASS))
}