library(gplots)
library(riverplot)
library(dplyr)
library(plyr)
library(RColorBrewer)


data <- data.frame("ID" = c(1:500), 
                   "A"  = factor(sample(c(1:5), 500, replace = T), 
                                 labels = c("A", "B", "C", "D","e")),
                   "B"  = factor(sample(c(1:3), 500, replace = T), 
                                 labels = c("Big", "Mid", "Small")))

#####################################
########Create My Own Data###########
#####################################
mydata = read.csv("C:/DataAnalysis/Visuals/Sankey/Riverdata.csv")
######################################

river <- makeRivPlot(mydata, "A", "B")
riverplot(river, srt=0)

#####################################
##River Plot Function
#####################################
makeRivPlot <- function(data, var1, var2) {

  require(dplyr)          # Needed for the count function
  require(riverplot)      # Does all the real work
  require(RColorBrewer)   # To assign nice colours

  names1 <- levels(data[, var1])
  names2 <- levels(data[, var2])

  var1   <- as.numeric(data[, var1])
  var2   <- as.numeric(data[, var2])

  edges  <- data.frame(var1, var2 + max(var1, na.rm = T))
  edges  <- count(edges)

  colnames(edges) <- c("N1", "N2", "Value")

  nodes <- data.frame(
           ID     = c(1:(max(var1, na.rm = T) + 
                         max(var2, na.rm = T))),  
           x      =  c(rep(1, times = max(var1, na.rm = T)), 
                       rep(2, times = max(var2, na.rm = T))),       
           labels = c(names1, names2) , 
           col    = c(brewer.pal(max(var1, na.rm = T), "Set1"), 
                      brewer.pal(max(var2, na.rm = T), "Set1")),
           stringsAsFactors = FALSE)

  nodes$col <- paste(nodes$col, 95, sep = "")

  river <- makeRiver(nodes, edges)

  return(river)
}