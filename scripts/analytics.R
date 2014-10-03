#
# author: cgranell
# aim: analytic scripts for exploratory data analysis 
#

library(ggplot2)## ggplot
library(plyr)   ## ddply

workingPath <- "C:/Users/cgranell/Data/MyCodeDev/papers-vgi-science/"
setwd(workingPath)
load("./data/reviewData.rda")

# Note: object in is called "data"
summary(data)

# number of representative papers 
numPapers <- length(unique(data$p.id))  # 57
numPapers


######################################################
### R code and functions for exploratory graphs  
######################################################

getUniqueVenues <- function() {
    papers <- unique(data$p.id)
    numPapers = length(papers)
    
    venues <- data.frame(
        p.id=character(numPapers), 
        p.venue=character(numPapers),
        p.venuetype=character(numPapers),
        stringsAsFactors=FALSE)
        
    
    for (i in 1:numPapers) {
        firstVenue <- sapply(data[data$p.id==papers[i], c("p.venue", "p.venuetype")], function(d) {d[1]})        
        # Add a new row (paper id + first ocurrence of venue) to the data.frame
        venues [i, ] <- c(papers[i], 
                          as.character(firstVenue[c("p.venue")]), 
                          as.character(firstVenue[c("p.venuetype")]))   
    }
    return (venues)
}


getUniqueSources <- function() {
    papers <- unique(data$p.id)
    numPapers = length(papers)
    
    sources <- data.frame(
        p.id=character(numPapers), 
        d.source=character(numPapers),
        d.official=character(numPapers),
        stringsAsFactors=FALSE)
    
    
    for (i in 1:numPapers) {
        firstSource <- sapply(data[data$p.id==papers[i], c("d.source", "d.official")], function(d) {d[1]})        
        # Add a new row (paper id + first ocurrence of source) to the data.frame
        sources [i, ] <- c(papers[i],
                           as.character(firstSource[c("d.source")]), 
                           as.character(firstSource[c("d.official")]))   
    }
    return (sources)
}


#################
###  RQ: Publication venues
#### What are the publication venues VGI research & data science have been published in? 
#### What are the most popular venues (> 1 occurrence)?
#################
subsetVenues <- getUniqueVenues()

#### What are the publication venues VGI research & data science have been published in? 
## Run the function table() on the value of "p.venue" for each group (p.venue, p.venuetype)
subsetVenues0 <- ddply(subsetVenues, c("p.venue", "p.venuetype"), summarise, 
           countVenue  = as.integer(table(p.venue)))

## graph of counts, reorder() to show counts of "p.venue" in order
ggplot(subsetVenues0, aes(x=reorder(p.venue, countVenue), y=countVenue, fill=p.venuetype)) +
    geom_bar(stat="bin") + coord_flip() + 
    #theme_bw(base_family = "sans family", base_size=10) + 
    theme_bw(base_size=10) + 
    labs(x = "Venues") + 
    labs(y = "Count") + 
    labs(title = "Main venues") +
    geom_text(aes(label=countVenue), hjust=1.5, colour="white")


#### What are the most popular venues (> 1 occurrence)?
ggplot(subsetVenues0[subsetVenues0$countVenue>1,], aes(x=reorder(p.venue, countVenue), y=countVenue, fill=p.venuetype)) +
    geom_bar(stat="bin") + coord_flip() +
    #theme_bw(base_family = "Avenir", base_size=10) + 
    theme_bw(base_size=10) + 
    labs(x = "Venues") + 
    labs(y = "Count") + 
    labs(title = "Most popular venues") +
    geom_text(aes(label=countVenue), hjust=1.5, colour="white")


#################
### RQ: Data sources 
#### What are the most frequently VGI data sources? 
#### What are the most popular sources? 
#### Are sources used in isolation or in combination?
#################

subsetSources <- getUniqueSources()

#### What are the most frequently VGI data sources? 
## Run the function table() on the value of "d.source" for each group (d.source, d.official)
subsetSources0 <- ddply(subsetSources, c("d.source"), summarise, 
                       countSource  = as.integer(table(d.source)))

## graph of counts, reorder to show counts of "d.source" in order
ggplot(subsetSources0, aes(x=reorder(d.source, countSource), y=countSource)) +
    geom_bar(stat="bin") + coord_flip() +
    #theme_bw(base_family = "Avenir", base_size=10) + 
    theme_bw(base_size=10) + 
    labs(x = "Source") + 
    labs(y = "Count") + 
    labs(title = "Main VGI sources") +
    geom_text(aes(label=countSource), hjust=1.5, colour="white")

#### What are the most popular sources? 
## we must "read" the previous plot accordingly

#### Are sources used in isolation or in combination?
## we must "read" the previous plot accordingly


#################
### RQ: Focus/Intended  uses 
#### What are the main categories of the paper?
#### What are the most frequently focus within each category?
#### What are the most frequently intended uses within the main focus?

#### Notes on the variables:
#### f.cat0: Factor (data-centric, human-centric, crisis-centric), group or main category of the paper
#### f.cat1: Factor (several levels), the focus within each category
#### f.cat2: Factor (several levels), the intended use within each focus
#################

subsetCat <- data[,c("f.cat0","f.cat1", "f.cat2", "p.year")]


#### What are the main categories of the paper?
## Run the function length() on the value of "f.cat0" for each group (f.cat0) 
subsetCat0 <- ddply(subsetCat, c("f.cat0"), summarise, 
                    countCat0  = length(f.cat0))

## graph of counts, reorder to show counts of "f.cat0" in order
ggplot(subsetCat0, aes(x=reorder(f.cat0, countCat0), y=countCat0, fill=f.cat0)) +
    geom_bar(stat="identity") + coord_flip() +
    #theme_bw(base_family = "Avenir", base_size=10) + 
    theme_bw(base_size=10) + 
    geom_text(aes(label=countCat0), hjust=1.5, colour="white") +
    labs(x = "Focus") + 
    labs(y = "Count") + 
    labs(title = "Main categories")


#### What are the most frequently focus within each category?
## Run the function length() on the value of "f.cat1" for each group (f.cat0, f.cat1) 
## to sum the ocurrences  of f.cat1 within each group
subsetCat1 <- ddply(subsetCat, c("f.cat0", "f.cat1"), summarise, 
                        countCat1 = length(f.cat1))

ggplot(subsetCat1, aes(x=reorder(f.cat1, countCat1), y=countCat1, fill=f.cat0)) +
    geom_bar(stat="identity") + coord_flip() +
    #theme_bw(base_family = "Avenir", base_size=10) + 
    theme_bw(base_size=10) + 
    theme(panel.grid.major.y = element_blank()) +
    geom_text(aes(label=countCat1), hjust=1.5, colour="white") +
    labs(x = "Focus") + 
    labs(y = "Count") + 
    labs(title = "Main focus broken by categories")


#### What are the most frequently intended uses within each focus?
## Run the function length() on the value of "f.cat2" for each group (f.cat0, f.cat1, f.cat2) 
## to sum the ocurrences  of f.cat2 within each group
subsetCat2 <- ddply(subsetCat, c("f.cat0", "f.cat1", "f.cat2"), summarise, 
                    countCat2 = length(f.cat2))

ggplot(subsetCat2, aes(x=reorder(f.cat2, countCat2), y=countCat2, fill=f.cat1)) +
    geom_bar(stat="identity") + coord_flip() +
    #theme_bw(base_family = "Avenir", base_size=10) + 
    theme_bw(base_size=10) + 
    theme(panel.grid.major.y = element_blank()) +   # No horizontal grid lines
    geom_text(aes(label=countCat2), hjust=1.5, colour="white") +
    labs(x = "Intended uses") + 
    labs(y = "Count") + 
    labs(title = "Intended uses broken by focus") 
    

# Alternative graph: Split subsetCat2 into 3 dataframes by f.cat0. 
# Then create a balloon plot with f.cat1 adn f.cat2 as categorical axes
dataCentric <- subset(subsetCat2,f.cat0=="data-centric")

ggplot(dataCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="cornsilk") +
    #theme_bw(base_family = "Avenir", base_size=10) +
    theme_bw(base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus ") + 
    labs(y = "Intended uses") + 
    labs(title = "Data-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) # Rotating the text 30 degrees
    #geom_text(aes(y=as.numeric(f.cat2)-sqrt(countCat2)/15, label=countCat2), vjust=1, colour="grey60", size=4)

humanCentric <- subset(subsetCat2,f.cat0=="human-centric")

ggplot(humanCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="cornsilk") +
    #theme_bw(base_family = "Avenir", base_size=10) +
    theme_bw(base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus ") + 
    labs(y = "Intended uses") + 
    labs(title = "Human-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) # Rotating the text 30 degrees
    
#geom_text(aes(y=as.numeric(f.cat2)-sqrt(countCat2)/15, label=countCat2), vjust=1, colour="grey60", size=4)

crisisCentric <- subset(subsetCat2,f.cat0=="crisis-centric")

ggplot(crisisCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="cornsilk") +
    #theme_bw(base_family = "Avenir", base_size=10) +
    theme_bw(base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus ") + 
    labs(y = "Intended uses") +
    labs(title = "Crisis-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) # Rotating the text 30 degrees
#geom_text(aes(y=as.numeric(f.cat2)-sqrt(countCat2)/15, label=countCat2), vjust=1, colour="grey60", size=4)


#################
### RQ: Analysis questions (Note: Summary tables in the paper are the main source to draw conclusions)
#### What are the most frequently (spatial) data analyses methods, and in what context/application?
#### What are the most frequently (spatial) data analyses methods in clujntion with VGI sources?
#### ...
#################


#################
### RQ: Misc and time-related questions 
#### In what context/application are VGI sources being used?
#### What types of VGI research have been reported, to what extent, and how is the temporal evolution? 
#### Which categories/focus/intended uses of VGI research received most coverage and how have coverage changed over time? 
#### What are the research topics (keywords) being addressed in VGI research, to what extent are they covered and how is coverage evolving? 
#### ...
#################


########################### BEYOND THIS IS NOT VALID ***************************
########################### BEYOND THIS IS NOT VALID ***************************
########################### BEYOND THIS IS NOT VALID ***************************

# library(treemap)    # treemap function 
# 
# 
# # copy defaults settings
# opar <- par() 
# 
# par(mfrow=c(1, 1), mar=c(1, 1, 1, 1))
# 
# treemap(subsetCat0, 
#         index = c("f.cat0", "f.cat1"), 
#         vSize = "countCat1" , 
#         vColor = "f.cat1", 
#         type = "categorical", 
#         title="Categories and focus",  
#         fontsize.labels=c(12, 8), 
#         align.labels=list(c("center", "center"), c("left", "top")),
#         force.print.labels=TRUE,
#         lowerbound.cex.labels=1, 
#         palette="PiYG",
#         bg.labels="#DCDCDC00", # color is "#RRGGBBAA" and the AA portion is the trasparency
#         border.col = c("black","white"), # defines line colour 
#         border.lwds = c(2,1), # defines line width
#         title.legend="Focus",
#         position.legend = "bottom") # remove legend)
# 
# # restore settings
# par(opar) 
# 
# 
# # points
# 
# 
# 
#     geom_segment(aes(yend=f.cat1), xend=0, colour="grey50") +
#     geom_point(size=3, aes(colour=f.cat0)) +
#     theme_bw() +
#     theme(panel.grid.major.y = element_blank()) +    
#     facet_grid(f.cat0 ~ ., scales="free_y", space="free_y")
# 
# 
#     
# ggplot(subsetCat0, aes(x=f.cat1, y=countCat1)) + geom_point() + facet_wrap(. ~ f.cat0)
# 
# ggplot(subsetCat0, aes(f.cat1)) + 
#     geom_tile(aes(fill = countCat1), colour = "white") + 
#     scale_fill_gradient(low = "white", high = "steelblue") +
#     facet_wrap(f.cat0 ~ .)
# 
# 
# 
# # Heatmap
# ggplot(subsetCat0, aes(f.cat1)) + 
#     geom_tile(aes(fill = countCat1), colour = "white") + 
#     scale_fill_gradient(low = "white", high = "steelblue") +
#     facet_wrap(f.cat0 ~ .)
# 
# 
# ggplot(data, aes(x=p.year)) + geom_bar(stat="bin")
# ggplot(data, aes(x=a.type)) + geom_bar(stat="bin")
# ggplot(data, aes(x=p.geo)) + geom_bar(stat="bin")
# ggplot(data, aes(x=f.cat0)) + geom_bar(stat="bin")
# ggplot(data, aes(x=f.cat1)) + geom_bar(stat="bin")
# ggplot(data, aes(x=f.uc0)) + geom_bar(stat="bin")
# ggplot(data, aes(x=f.user)) + geom_bar(stat="bin")
# ggplot(data, aes(x=d.source)) + geom_bar(stat="bin")
# 
# ggplot(data, aes(x=p.year)) + geom_histogram(binwidth=1)
# ggplot(data, aes(x=a.type)) + geom_histogram(binwidth=1)
# ggplot(data, aes(x=p.geo)) + geom_histogram(binwidth=1)
# 
# ggplot(data, aes(x=p.year, fill=p.geo)) + geom_bar(position="dodge", binwidth=4)
# ggplot(data, aes(x=p.year, fill=a.type)) + geom_bar(position="dodge", binwidth=2)
# ggplot(data, aes(x=f.cat0, fill=a.type)) + geom_bar(position="dodge", binwidth=2)
# 
# ggplot(data, aes(x=d.source, y=f.cat1, fill=a.type)) + geom_point(alpha = 1/3) # + facet_wrap(. ~ f.cat1)
# 
# # extract the lsit of keywords per submission and insret each ne into a different column 
# #   1. create a data.frame preallocated with 11 columns: submission id and 10 columns for 10 keywords 
# keywords.data <- data.frame(
#     id=integer(num.submissions), 
#     kw.1=character(num.submissions),
#     kw.2=character(num.submissions), 
#     kw.3=character(num.submissions), 
#     kw.4=character(num.submissions), 
#     kw.5=character(num.submissions), 
#     kw.6=character(num.submissions), 
#     kw.7=character(num.submissions), 
#     kw.8=character(num.submissions), 
#     kw.9=character(num.submissions), 
#     kw.10=character(num.submissions), 
#     stringsAsFactors=FALSE)
# 
# 
# for(i in 1:num.submissions) {
# #   2. For each submission, extract id and list of keywords, split them, and add  
#     id <- papers[i, 1]
#     keywords <- sapply(strsplit(papers[i, 5],";"), function (x) {
#         ifelse (x=="", NA, gsub("^\\s+|\\s+$", "", x))
#     })
#     keywords <- as.vector(keywords)
# #   3. If a submission has less than 10 keywords, then add whitespaces
#     keywords[length(keywords)+1:10] <- ""
# #   4. Add a new row (submission id + 10 keywords) to the newly-created data.frame
#     keywords.data[i, ] <- c(id, keywords)
# }
# 
# 
# keywords.data[,1] <- as.integer(keywords.data[,1])
# 
# #   5. Merge both data frames by the common column: submission id
# submissions.clean <- merge(submissions, keywords.data, by="id")
# 
# dim(submissions.clean) 
# # 153 obs. of 15 variables
# 
# # Not run: Save clean data (data frame) into a local file 
# write.csv(submissions.clean, "./data/submissions-clean.csv")
