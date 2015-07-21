############################
#Loads FTIR data and formats it: ID/wavelenths.../batch/class
#works only for formats Batch_x-Beef_yya1.csv, where x is batch number, yy is class
#Rafal Kural
############################

rm(list = ls())
graphics.off()

#set working directory to script directory
test <- dirname(sys.frame(1)$ofile)
setwd(test)

#load file with data to format
fileToLoad = c("../../../Data/FTIR_Batch4_2.csv")

#read data from file 
Data <- read.table(fileToLoad, sep = ",", header = TRUE)

# find indices of samples signed "f" and "g" and remove them - callibration error
errorIDs <- c((grep("\\d{2}f", Data$wavelenghts)), 
              (grep("\\d{2}g", Data$wavelenghts)))
Data <- Data[-errorIDs,]

#add and fill new batch and class columns to dataframe
Data$Batch[1] = 1
Data$Class[1] = "0%"
for (i in 1:nrow(Data)){
  #find and assign batch number
  if (grepl("Batch_1",Data$wavelenghts[i])) {Data$Batch[i] = "B1"}
  else if (grepl("Batch_2",Data$wavelenghts[i])) {Data$Batch[i] = "B2"}
  else if (grepl("Batch_3",Data$wavelenghts[i])) {Data$Batch[i] = "B3"}
  else if (grepl("Batch_4",Data$wavelenghts[i])) {Data$Batch[i] = "B4"}
  else {Data$Batch[i] = "Unknown"}
  
  #Find and assign class (00, 100 and 10 swapped to avoid using regex)
  if (grepl("100",Data$wavelenghts[i])) {Data$Class[i] = "100%"}
  else if (grepl("00",Data$wavelenghts[i])) {Data$Class[i] = "0%"}
  else if (grepl("20",Data$wavelenghts[i])) {Data$Class[i] = "20%"}
  else if (grepl("30",Data$wavelenghts[i])) {Data$Class[i] = "30%"}
  else if (grepl("40",Data$wavelenghts[i])) {Data$Class[i] = "40%"}
  else if (grepl("50",Data$wavelenghts[i])) {Data$Class[i] = "50%"}
  else if (grepl("60",Data$wavelenghts[i])) {Data$Class[i] = "60%"}
  else if (grepl("70",Data$wavelenghts[i])) {Data$Class[i] = "70%"}
  else if (grepl("80",Data$wavelenghts[i])) {Data$Class[i] = "80%"}
  else if (grepl("90",Data$wavelenghts[i])) {Data$Class[i] = "90%"}
  else if (grepl("10",Data$wavelenghts[i])) {Data$Class[i] = "10%"}
  else {Data$Class[i] = "Unknown"}
}

#save new data frame to csv file
write.csv(Data, file = "../../../Data/FTIR_batch4_kural_format_errors_removed.csv", row.names = FALSE)