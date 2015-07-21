##################################################
# Generates pdf file with performance plot 
#
# data - matrix of data to plot
# names - vector of names for data
# file - output file
#
# Rafal Kural
##################################################

generate_performance_plot <- function(data, names, file) {
  require(colorRamps)
  
  # generate different colors for every data entry
  colors <- primary.colors(nrow(data), steps = 5, no.white = TRUE)
  
  # create pdf file
  pdf(file)
  
  # draw plot
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(c(0, iterations), c(0, 100), xlab = "Iterations", 
       ylab = "Accuracy", main = "Models Performance", type = "n")
  axis(side=2, at=seq(0, 100, by=10))
  
  for(i in 1:nrow(data)) {
    lines(1:ncol(data), data[i,]*100, col = colors[i])
  }
  
  # generate legend for plot
  legend("topright", inset=c(-0.42,0), names, lty=c(1), col=colors, cex = 0.8)
  
  # close file
  dev.off()
}