generate_plots_pdf <- function(SCORES,samplenames,CLASS,varvector,filename,annotate){
  #generates 3 plots: PC1vPC2, PC2vPC3, PC1vPC3
  #outputs 1 pdf file
  #SCORES - pca scores matrix
  #samplenames - matrix with sample names
  #varvector - vector of eigenvalues for eigen pca, W vector for SVD
  #filename - name for the pdf
  #annotate - TRUE to annotate samples on plot, default FALSE
  #
  #Rafal Kural
  if (missing(annotate)) {
    annotate = FALSE
  }
  variance <- c(varvector[1] / sum(varvector) * 100,
                varvector[2] / sum(varvector) * 100,
                varvector[3] / sum(varvector) * 100)
  #create 10-colors gradient to sign classes
  colorPalette <- colorRampPalette(c("blue", "green", "red"))
  colors <- colorPalette(11)
  #calculate how many times a class appears (REMEMBER DATA HAS TO BE SORTED BY CLASSES)
  classAppearances <- table(CLASS)
  #vector of colors of size of X storing colors appropriate for given sample class
  colorsVector <- NULL
  for (i in 1:length(colors)) {
    #last element is NA, ignore it
    if (is.na(classAppearances[i]))
    {
      break
    }
    colorsVector <- c(colorsVector, rep(colors[i], classAppearances[i]))
  }
  
  pdf(filename)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(SCORES[,1], SCORES[,2], col=colorsVector, xlab = sprintf("PC1 (%1.0f%%)", variance[1]), ylab = sprintf("PC2 (%1.0f%%)", variance[2]), main = "PC1 vs PC2")
  if (annotate == TRUE) {
    text(SCORES[,1], SCORES[,2], as.character(samplenames), col="gray")
  }
  legend("topright", inset=c(-0.2,0), legend=unique(CLASS), cex=0.8, col=colors, pch=1)

  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(SCORES[,2], SCORES[,3], col=colorsVector, xlab = sprintf("PC2 (%1.0f%%)", variance[2]), ylab = sprintf("PC3 (%1.0f%%)", variance[3]), main = "PC2 vs PC3")
  if (annotate == TRUE) {
    text(SCORES[,1], SCORES[,2], as.character(samplenames), col="gray")
  }
  legend("topright", inset=c(-0.2,0), legend=unique(CLASS), cex=0.8, col=colors, pch=1)

  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(SCORES[,1], SCORES[,3], col=colorsVector, xlab = sprintf("PC1 (%1.0f%%)", variance[1]), ylab = sprintf("PC3 (%1.0f%%)", variance[3]), main = "PC1 vs PC3")
  if (annotate == TRUE) {
    text(SCORES[,1], SCORES[,2], as.character(samplenames), col="gray")
  }
  legend("topright", inset=c(-0.2,0), legend=unique(CLASS), cex=0.8, col=colors, pch=1)
  dev.off()
}