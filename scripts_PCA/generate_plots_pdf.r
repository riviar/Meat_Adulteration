######################################################################
#generates 3 plots: PC1vPC2, PC2vPC3, PC1vPC3                        #
#outputs 1 pdf file                                                  #
#SCORES - pca scores matrix, uses only first 3                       #
#samplenames - matrix with sample names                              #
#varvector - vector of eigenvalues for eigen pca, W vector for SVD   #
#filename - name for the pdf                                         #
#annotate - TRUE to annotate samples on plot, default FALSE          #
#                                                                    #
#Rafal Kural                                                         #
######################################################################

generate_plots_pdf <- function(SCORES,samplenames,CLASS,varvector,filename,annotate){
  
  # check if user provided annotate value, set FALSE if not
  if (missing(annotate)) {
    annotate = FALSE
  }
  
  # calculate % variance for first 3 PCs
  variance <- c(varvector[1] / sum(varvector) * 100,
                varvector[2] / sum(varvector) * 100,
                varvector[3] / sum(varvector) * 100)
  
  # create gradient to sign classes
  colorPalette <- colorRampPalette(c("blue", "green", "red"))
  
  #calculate how many times each class appears
  classAppearances <- table(CLASS)
  
  # use gradient to assign each class a different color
  colors <- colorPalette(length(classAppearances))
  
  # initialize colors vector
  colorsVector <- NULL
  
  #fill colors vector
  for (i in 1:length(CLASS)) {
    #assign color depeding on class
    if (grepl("100%", CLASS[i])) {colorsVector[i] = colors[11]}
    else if (grepl("10%", CLASS[i])) {colorsVector[i] = colors[2]}
    else if (grepl("20%", CLASS[i])) {colorsVector[i] = colors[3]}
    else if (grepl("30%", CLASS[i])) {colorsVector[i] = colors[4]}
    else if (grepl("40%", CLASS[i])) {colorsVector[i] = colors[5]}
    else if (grepl("50%", CLASS[i])) {colorsVector[i] = colors[6]}
    else if (grepl("60%", CLASS[i])) {colorsVector[i] = colors[7]}
    else if (grepl("70%", CLASS[i])) {colorsVector[i] = colors[8]}
    else if (grepl("80%", CLASS[i])) {colorsVector[i] = colors[9]}
    else if (grepl("90%", CLASS[i])) {colorsVector[i] = colors[10]}
    else if (grepl("0%", CLASS[i])) {colorsVector[i] = colors[1]}
    #black color for unknown samples
    else {colorsVector[i] == "#FFF"}
  }
  
  # create pdf for plots
  pdf(filename)
  
  # plot PC1 v PC2
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(SCORES[,1], SCORES[,2], col=colorsVector, xlab = sprintf("PC1 (%1.0f%%)", variance[1]), ylab = sprintf("PC2 (%1.0f%%)", variance[2]), main = "PC1 vs PC2")
  if (annotate == TRUE) {
    text(SCORES[,1], SCORES[,2], as.character(samplenames), col="gray", pos = 1, cex = 0.4)
  }
  legend("topright", inset=c(-0.2,0), legend=unique(CLASS), cex=0.8, col=colors, pch=1)

  # plot PC2 v PC3
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(SCORES[,2], SCORES[,3], col=colorsVector, xlab = sprintf("PC2 (%1.0f%%)", variance[2]), ylab = sprintf("PC3 (%1.0f%%)", variance[3]), main = "PC2 vs PC3")
  if (annotate == TRUE) {
    text(SCORES[,2], SCORES[,3], as.character(samplenames), col="gray", pos = 1, cex = 0.4)
  }
  legend("topright", inset=c(-0.2,0), legend=unique(CLASS), cex=0.8, col=colors, pch=1)

  # plot PC1 v PC3
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(SCORES[,1], SCORES[,3], col=colorsVector, xlab = sprintf("PC1 (%1.0f%%)", variance[1]), ylab = sprintf("PC3 (%1.0f%%)", variance[3]), main = "PC1 vs PC3")
  if (annotate == TRUE) {
    text(SCORES[,1], SCORES[,3], as.character(samplenames), col="gray", pos = 1, cex = 0.4)
  }
  legend("topright", inset=c(-0.2,0), legend=unique(CLASS), cex=0.8, col=colors, pch=1)
  
  # close file
  dev.off()
}