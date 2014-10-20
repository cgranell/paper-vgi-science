#
# author: cgranell
# aim: transformation and cleaning scripts to transform raw data into processed data
# 
#

require(plyr)

workingPath <- "C:/Users/cgranell/Data/MyCode/paper-vgi-science/"
setwd(workingPath)

url <- "https://github.com/cgranell/paper-vgi-science/raw/master/data/rawData.csv"
rawFile <- "rawData.csv"
dataFile <- "reviewData.rda"

pathToRawFile <- paste("./data/", rawFile, sep="")
if (!file.exists(pathToRawFile)) {
    file <- download.file(url, destfile=pathToRawFile)
}

# load raw data
data <- read.csv(pathToRawFile, colClasses="character", header=TRUE, strip.white=TRUE)

dim(data) # 92 obs. of 29 variables

# convert columns names into lowercase  
names(data) <- tolower(names(data))


str(data)

# data type cource
data$p.year <- as.integer(data$p.year)
data$p.venue <- as.factor(data$p.venue)
data$p.venuetype <- as.factor(data$p.venuetype)
data$p.geo <- as.factor(data$p.geo)
data$d.official <- as.factor(data$d.official)
data$a.type <- as.factor(data$a.type)
data$a.tool <- as.factor(data$a.tool)
data$f.cat0 <- as.factor(data$f.cat0)
data$f.cat1 <- as.factor(data$f.cat1)
data$f.cat2 <- as.factor(data$f.cat2)
data$f.uc0 <- as.factor(data$f.uc0)
data$f.user <- as.factor(data$f.user)
data$d.source <- as.factor(data$d.source)


# number of representative papers 
length(unique(data$p.id))  # 57

# Save data frame object into a local file 
save(data, file=paste("./data/", dataFile, sep=""))
