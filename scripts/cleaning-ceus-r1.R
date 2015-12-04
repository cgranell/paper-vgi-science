#
# author: cgranell
# aim: transformation and cleaning scripts to transform raw data into processed data
# 
#

url <- "https://github.com/cgranell/paper-vgi-science/raw/master/data/ceus-r1/rawdata-ceus-R1.csv"
rawFile <- "rawdata-ceus-R1.csv"
dataFile <- "cleandata-ceus-R1.rda"

pathToRawFile <- paste("./data/ceus-r1/", rawFile, sep="")
if (!file.exists(pathToRawFile)) {
    file <- download.file(url, destfile=pathToRawFile)
}

# load raw data
data <- read.csv(pathToRawFile, colClasses="character", header=TRUE, strip.white=TRUE)

dim(data) # 97 obs. of 30 variables

# convert columns names into lowercase  
names(data) <- tolower(names(data))


str(data)

# data type cource
data$p.year <- as.integer(data$p.year)
data$p.venue <- as.factor(data$p.venue)
data$p.venuetype <- as.factor(data$p.venuetype)
data$a.type <- as.factor(data$a.type)
data$a.tool <- as.factor(data$a.tool)
data$f.cat0 <- as.factor(data$f.cat0)
data$f.cat1 <- as.factor(data$f.cat1)
data$f.cat2 <- as.factor(data$f.cat2)
data$f.uc0 <- as.factor(data$f.uc0)
data$f.uc1 <- as.factor(data$f.uc1)
data$f.user <- as.factor(data$f.user)
data$d.source <- as.factor(data$d.source)
data$d.official <- as.factor(data$d.official)


# Names of subcategories within "application-centric" are shorthen  
levels(data$f.cat1)[levels(data$f.cat1)=="crisis management (coordination and organization)"] <- "coordination and organization"
levels(data$f.cat1)[levels(data$f.cat1)=="crisis management (detection and prediction)"] <- "detection and prediction"
levels(data$f.cat1)[levels(data$f.cat1)=="crisis management (health)"] <- "health"
levels(data$f.cat1)[levels(data$f.cat1)=="crisis management (monitoring)"] <- "monitoring"
levels(data$f.cat1)[levels(data$f.cat1)=="crisis management (recovery and response)"] <- "recovery and response"


# We group some sub-categories (f.cat1) which are quite similar. This will ease readiness:
# "data quality" and "data assessment" are integrated into a new "data quality and assessment" level
levels(data$f.cat1)[levels(data$f.cat1)=="data quality"] <- "data quality and assessment"
levels(data$f.cat1)[levels(data$f.cat1)=="data assessment"] <- "data quality and assessment"

# We group some intended uses (f.cat2) which are quite similar. This will ease readiness:
# "topic classification" and "topic selection" are integrated into 
# the existing "topic selection and classification" 
levels(data$f.cat2)[levels(data$f.cat2)=="topic classification"] <- "topic selection and classification"
levels(data$f.cat2)[levels(data$f.cat2)=="topic selection"] <- "topic selection and classification"

# "location classification" and location selection" are integrated into 
# the existing "location selection and classification" level
levels(data$f.cat2)[levels(data$f.cat2)=="location classification"] <- "location selection and classification"
levels(data$f.cat2)[levels(data$f.cat2)=="location selection"] <- "location selection and classification"

# "actionable information" and "credible information" form a new level named "actionable and credible information"
levels(data$f.cat2)[levels(data$f.cat2)=="actionable information"] <- "actionable and credible information"
levels(data$f.cat2)[levels(data$f.cat2)=="credible information"] <- "actionable and credible information"


# number of representative papers 
length(unique(data$p.id))  # 59

# Save data frame object into a local file 
save(data, file=paste("./data/ceus-r1/", dataFile, sep=""))
