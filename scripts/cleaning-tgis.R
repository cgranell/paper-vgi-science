#
# author: cgranell
# aim: transformation and cleaning scripts to transform raw data into processed data for the TGIS study
# 
#

workingPath <- "C:/Users/cgranell/Data/MyCode/paper-vgi-science/"
setwd(workingPath)

url <- "https://github.com/cgranell/paper-vgi-science/raw/master/data/tgis/rawdata.csv"
rawFile <- "rawdata.csv"
dataFile <- "cleandata.rda"

pathToRawFile <- paste("./data/tgis/", rawFile, sep="")
if (!file.exists(pathToRawFile)) {
    file <- download.file(url, destfile=pathToRawFile)
}

# load raw data
data <- read.csv(pathToRawFile, colClasses="character", header=TRUE, strip.white=TRUE)

dim(data) # 54 obs. of 9 variables

# convert columns names into lowercase  
names(data) <- tolower(names(data))


str(data)

# data type cource
data$data.reproducible <- as.factor(data$data.reproducible)
data$data.replicable <- as.factor(data$data.replicable)
data$methods.reproducible <- as.factor(data$methods.reproducible)
data$methods.replicable <- as.factor(data$methods.replicable)

# drop verdict column
data$verdict <- NULL

# Save data frame object into a local file 
save(data, file=paste("./data/tgis/", dataFile, sep=""))
