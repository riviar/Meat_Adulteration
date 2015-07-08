######################################################################
#generates loadings plots                                            #
#outputs 1 pdf file                                                  #
#LOADINGS - loadings matrix                                          #                                         #
#filename - name for the pdf                                         #
#ncompNum - max number of latent variables used in pls               #
#                                                                    #
#Rafal Kural                                                         #
######################################################################

generate_loading_plot_pdf <- function(LOADINGS,filename, ncompNum){
  
  # create pdf for plots
  pdf(filename)
  
  # generate loadings plots
  for (i in 1:ncompNum) {
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(LOADINGS[,i], main = paste("LOADINGS (ncomp= ", ncompNum, " )", sep = ""))
    text(LOADINGS[,i], as.character(1:nrow(LOADINGS)), cex = 0.8, col="blue")
  }
  
  # close file
  dev.off()
}