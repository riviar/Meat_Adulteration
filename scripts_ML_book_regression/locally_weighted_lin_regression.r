##############################################
# Performs PLS regression                    #
# Results available as csv files             #
# Main use to adjust class prediction model  #
# using numerical values                     #
#                                            #
# Rafal Kural                                #
##############################################
# clear workspace and open windows
rm(list = ls())
graphics.off()

#set working directory to script directory
test <- dirname(sys.frame(1)$ofile)
setwd(test)