############################
#Loads csv data and formats it: ID/wavelenths.../batch/class
#works only for formats Bx_yya/wavelengths, where x is batch number, yy is class
#Rafal Kural
############################

rm(list = ls())
graphics.off()

#set working directory to script directory
test <- dirname(sys.frame(1)$ofile)
setwd(test)

#load files with data to format
filesToLoad = c("../../../Data/batch1.csv", "../../../Data/batch2.csv", "../../../Data/batch3.csv", "../../../Data/batch4.csv")

#initialize list of data (1 element per file)
Data <- list(rep(-1, length(filesToLoad)))

#fill list with data
for (i in 1:length(filesToLoad)){
  Data[[i]] <- read.table(filesToLoad[i], sep = ",", header = TRUE)
}

#reconstruct data into one dataframe
DataReconstructed <- Data[[1]]
for (i in 2:length(Data)) {
  DataReconstructed = rbind(DataReconstructed, Data[[i]])
}

# find indices of samples signed test and remove them - callibration error
errorIDs <- grep("test", DataReconstructed$Sample.ID)
DataReconstructed <- DataReconstructed[-errorIDs,]

#add and fill new batch and class columns to dataframe
DataReconstructed$Batch[1] = 1
DataReconstructed$Class[1] = "0%"
for (i in 1:nrow(DataReconstructed)){
  #find and assign batch number
  if (grepl("b1",DataReconstructed$Sample.ID[i])) {DataReconstructed$Batch[i] = "B1"}
  else if (grepl("b2",DataReconstructed$Sample.ID[i])) {DataReconstructed$Batch[i] = "B2"}
  else if (grepl("b3",DataReconstructed$Sample.ID[i])) {DataReconstructed$Batch[i] = "B3"}
  else if (grepl("b4",DataReconstructed$Sample.ID[i])) {DataReconstructed$Batch[i] = "B4"}
  else {DataReconstructed$Batch[i] = "Unknown"}
  
  #Find and assign class (00, 100 and 10 swapped to avoid using regex)
  if (grepl("100",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "100%"}
  else if (grepl("00",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "0%"}
  else if (grepl("20",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "20%"}
  else if (grepl("30",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "30%"}
  else if (grepl("40",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "40%"}
  else if (grepl("50",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "50%"}
  else if (grepl("60",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "60%"}
  else if (grepl("70",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "70%"}
  else if (grepl("80",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "80%"}
  else if (grepl("90",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "90%"}
  else if (grepl("10",DataReconstructed$Sample.ID[i])) {DataReconstructed$Class[i] = "10%"}
  else {DataReconstructed$Class[i] = "Unknown"}
}

#save new data frame to csv file
write.csv(DataReconstructed, file = "../../../Data/Videometer_allbatches_kural_format_errors_removed.csv", row.names = FALSE)