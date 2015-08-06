
#HOW TO Install rCharts
##require(devtools)
##install_github('rCharts','ramnathv')

require(igraph)
require(rCharts)
library(igraph)
library(rCharts)
g <- graph.tree(40, children = 4)

E(g)$weight = 1
edgelist <- get.data.frame(g) #this will give us a data frame with from,to,weight
colnames(edgelist) <- c("source","target","value")
#make character rather than numeric for proper functioning
edgelist$source <- as.character(edgelist$source)
edgelist$target <- as.character(edgelist$target)
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('libraries/widgets/d3_sankey')
sankeyPlot$setTemplate(script = "libraries/widgets/d3_sankey/layouts/chart.html")
sankeyPlot$set(
  data = edgelist,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 500
)
sankeyPlot$print(chartId = 'sankey1')