#
# author: cgranell
# aim: transformation and cleaning scripts to transform raw data into processed data
# 
#

workingPath <- "C:/Users/cgranell/Data/MyCodeDev/papers-vgi-science/"
setwd(workingPath)

# load raw data
data <- read.csv("./data/rawData.csv", colClasses="character", header = TRUE, strip.white=TRUE, nrow=92)


dim(data) # 92 obs. of 29 variables
# convert columns names into lowercase  
names(data) <- tolower(names(data))
# view raw data
View(data)


# type conversion   
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
numPapers <- length(unique(data$p.id))  # 57
numPapers


# Save clean data (data frame object) into a local file 
save(data, file=paste(workingPath, "/data/reviewData.rda", sep=""))

