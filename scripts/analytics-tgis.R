#
# author: cgranell
# aim: analytic scripts for exploratory data analysis for TGIS paper
#

library(ggplot2)## ggplot
library(plyr)   ## ddply
library(googleVis) # for sankey diagram 

workingPath <- "C:/Users/cgranell/Data/MyCode/paper-vgi-science/"
setwd(workingPath)

url <- "https://github.com/cgranell/paper-vgi-science/raw/master/data/tgis/cleandata.rda"
dataFile <- "cleandata.rda"
pathToDataFile <- paste("./data/tgis/", dataFile, sep="")

if (!file.exists(pathToDataFile)) {
    file <- download.file(url, destfile=pathToDataFile)
}

load(pathToDataFile)

# Note: dateset is loaded into R object called "data"
summary(data)

# number of representative papers 
numPapers <- length(unique(data$id))  
numPapers # 54

# Distribution of data.replicable
ggplot(data, aes(x=data.replicable)) + geom_bar()

qplot(data.replicable, methods.replicable, data = data, geom = "jitter")
qplot(data.replicable, methods.reproducible, data = data, geom = "jitter")
qplot(data.reproducible, methods.replicable, data = data, geom = "jitter")
qplot(data.reproducible, methods.reproducible, data = data, geom = "jitter")

# Sankey diagram

sankeydata <- data.frame(
    origin=c(
        rep("data.repro",3), 
        rep("data.repli",2),
        "meth.repro", 
        "data.repro",
        "data.repli",
        "meth.repro",
        "meth.repli",
        "no.data"),
    visit=c(
        "meth.repro",
        "meth.repli", 
        "data.repli", 
        "meth.repro", 
        rep("meth.repli",2),
        rep("no.meth", 2),
        rep("no.data", 2),
        "no.meth"),
    weights=c(0, 2, 2, 1, 16, 3, 0, 1, 2, 13, 25))
    
plot(
    gvisSankey(sankeydata, from="origin", 
               to="visit", weight="weight",
               options=list(
                   width= 600,
                   height=300,
                   sankey="{link: {color: {fill: '#lightgray', fillOpacity: 0.4, stroke: 'black', strokeWidth: 0.51}},
                            node: { width: 10, labelPadding: 10, nodePadding: 80, 
                                    label: { bold: true, fontSize: 12, color: '#871b47'}}}"
               ))
)


sessionInfo()
