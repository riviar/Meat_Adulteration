pca_eigen_3d <- function(X) {
  source("generate_plots_pdf.r")
  require(gWidgets)
  require(gWidgetsRGtk2)
  #performs pca via eigenvectors
  #generates 3d PCA plot
  #X - input data matrix
  #
  #Rafal Kural
  COVmatrix <- cov(X)
  EIGEN <- eigen(COVmatrix)
  LOADS1 <- t(EIGEN$vectors)
  W1 <- EIGEN$values
  ILOADS1 <- solve(LOADS1)
  SCORES1 <- as.matrix(X) %*% ILOADS1
  
  #Create 3D plot
  w = gwindow("3D plot Viewer")
  g = glayout(cont=w)
  g[1,1] <- (g1 = ggroup(horizontal = FALSE, cont = g))
  g[1,2] <- "Angle"
  g[2,1] <- (g2 = ggroup(horizontal = FALSE, cont = g))
  g[2,2] <- "Plot type"
  g[3,1] <- (gg = ggraphics(cont = g))
  size(gg) <- c(700,700)
  
  #event handler function
  update <- function(h,...) {
    plotType <- 0
    if(svalue(radioB) == "Scores") {plotType <- 0}
    else if (svalue(radioB) == "Biplot") {plotType <- 1}
    biplot3d2(X, colnames(X), row.names(X), 1, 2, 3, svalue(angle), plotType)
  }
  #gui elements: radio buttons and slider
  radioB <- gradio(c("Score", "Biplot"), cont=g2, handler = update)
  angle <- gslider(0, 180, by = 1, cont=g1, expand=TRUE, handler = update)
}