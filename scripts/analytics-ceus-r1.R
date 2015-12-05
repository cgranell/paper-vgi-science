#
# author: cgranell
# aim: analytic scripts for exploratory data analysis (CEUS)
#

library(ggplot2)## ggplot
library(plyr)   ## ddply


url <- "https://github.com/cgranell/paper-vgi-science/raw/master/data/ceus-r1/cleandata-ceus-R1.rda"
dataFile <- "cleandata-ceus-R1.rda"
pathToDataFile <- paste("./data/ceus-r1/", dataFile, sep="")

if (!file.exists(pathToDataFile)) {
    file <- download.file(url, destfile=pathToDataFile)
}

load(pathToDataFile)

# Note: dateset is loaded into R object called "data"
summary(data)

# number of representative papers 
numPapers <- length(unique(data$p.id))  # 59


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
        f.cat0=character(numPapers),
        f.cat1=character(numPapers),
        d.source=character(numPapers),
        d.official=character(numPapers),
        stringsAsFactors=FALSE)
    
    
    for (i in 1:numPapers) {
        firstSource <- sapply(data[data$p.id==papers[i], c("f.cat0", "f.cat1", "f.uc0", "d.source", "d.official")], function(d) {d[1]})        
        # Add a new row (paper id + first ocurrence of source) to the data.frame
        sources [i, ] <- c(papers[i],
                           as.character(firstSource[c("f.cat0")]), 
                           as.character(firstSource[c("f.cat1")]), 
                           as.character(firstSource[c("d.source")]), 
                           as.character(firstSource[c("d.official")]))   
    }
    return (sources)
}


percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


## REVISIT FOR CEUS 
## Distribution of publication venues 
## What are the publication venues VGI research & data science have been published in? 
subsetVenues <- getUniqueVenues()

#### What are the publication venues VGI research & data science have been published in? 
## Run the function table() on the value of "p.venue" for each group (p.venue, p.venuetype)
subsetVenues0 <- ddply(subsetVenues, c("p.venue", "p.venuetype"), summarise, 
           countVenue  = as.integer(table(p.venue)))


# Journal and conference rates. Note that total is 57 because one paper is a technical report
sumJournals <- sum(subsetVenues0[subsetVenues0$p.venuetype=="journal", c("countVenue")])
sumConferences <- sum(subsetVenues0[subsetVenues0$p.venuetype=="conference", c("countVenue")])
rateJournals <- (sumJournals / (sumJournals + sumConferences))
rateConferences <- (sumConferences / (sumJournals + sumConferences))

# Legend labels including  percent
legend.txt = c(paste0("Conference: ", percent(rateConferences)), paste0("Journal: ", percent(rateJournals)))
legend.txt

## Print list of venues in a tabular format to add it to the paper as annex
print.data.frame(subsetVenues0[,c("p.venue", "p.venuetype")])
print.data.frame(subsetVenues0[,c("p.venue", "countVenue")])

#################
### RQ3: Data sources 
#### What are the most frequently VGI data sources? 
#### What are the most popular sources? 
#### Are sources used in isolation or in combination?
#################

subsetSources <- getUniqueSources()

#### What are the most frequently VGI data sources? 
## Run the function table() on the value of "d.source" for each group (d.source, d.official)
subsetSources0 <- ddply(subsetSources, c("f.cat0", "f.cat1", "d.source"), summarise, 
                       countSource  = as.integer(table(d.source)))

subsetSources0 <- ddply(subsetSources, c("f.cat0", "d.source"), summarise, 
                        countSource  = length(d.source))

# Get the sources  (d.source), sorted first by category (f.cat0), then by count  
sourceorder <- subsetSources0$d.source[order(subsetSources0$f.cat0, subsetSources0$countSource)]
sourceorder <- subsetSources0$d.source[order(subsetSources0$countSource)]
# Turn d.source into a factor, with levels in the order of sourceorder
subsetSources0$d.source <- factor(subsetSources0$d.source, levels=sourceorder)


# Turn NA as a factor level
subsetSources0$d.source <- addNA(subsetSources0$d.source)
# Rename level of a factor by index: change fourh item, NA, to "Not specified".
levels(subsetSources0$d.source)[24] <- "Not specified"
levels(subsetSources0$d.source)

ppi=600
jpeg(filename = "./figures/ceus-fig8-new.jpg",width=8*ppi, height=5*ppi, res=ppi, quality=100)
ggplot(subsetSources0, aes(x=f.cat0, y=d.source, fill=f.cat0)) +
  geom_point(aes(size=countSource), shape=21, colour="black") +
  scale_size_area(max_size=15, guide=FALSE) + 
  theme_bw(base_family = "Times", base_size=10) + 
  guides(fill=FALSE) +
  labs(x = "Main categories") + 
  labs(y = "VGI source") +
  #labs(title = "Figure 10:  VGI sources used by main category") +
  scale_colour_brewer(palette="Set2") +
  geom_text(aes(label=countSource), vjust=-0.1, colour="grey30", size=2)+   # Add labels from data
  theme(panel.grid.major.x = element_blank()) # Hide the veritical grid lines
dev.off()

############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/fig10.jpg",width=8*ppi, height=5*ppi, res=ppi, quality=100)
## graph of counts, reorder to show counts of "d.source" in order
ggplot(subsetSources0, aes(x=d.source, y=countSource, fill=f.cat0)) +
    geom_bar(stat="identity", width=0.6, colour="black") +
    coord_flip() +
    theme_bw(base_family = "Times", base_size=10) + 
    scale_colour_brewer(palette="Set1") +
    scale_y_continuous(breaks=c(seq(0,28,2))) +
    labs(x = "VGI source") + 
    labs(y = "Number of papers") + 
    #labs(title = "Counts of main VGI sources") +
    #geom_text(aes(label=countSource), hjust=1.5, colour="black", size=3) +
    guides(fill=guide_legend(title="Category")) +  # Set the legend title
    theme(legend.position=c(1,0), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    theme(panel.grid.major.y = element_blank()) # Hide the horizontal grid lines
dev.off()
############ END FINAL FIGURE #################

#### What are the most popular sources? 
## we must "read" the previous plot accordingly
## Twitter is by large the most used data source, followed by Flickr

#### Are sources used in isolation or in combination?
## we must "read" the previous plot accordingly
## Most studies used one data source in isolation. Combination of distinct data sources is a rare exception


#################
## REVISIT FOR CEUS 
## RQ 2: Focus/Intended  uses 
## What are the main categories of the paper?
## What are the most frequently focus within each category?
## What are the most frequently intended uses within the main focus?

#### Notes on the variables:
#### f.cat0: Factor (data-centric, human-centric, application-centric), group or main category of the paper
#### f.cat1: Factor (several levels), the focus within each category
#### f.cat2: Factor (several levels), the intended use within each focus
#################

# how many papers are classified as data-centric
dc <- data[data$f.cat0=="data-centric",]
length(unique(dc$p.id)) #41

# how many papers are classified as data-centric
hc <- data[data$f.cat0=="human-centric",]
length(unique(hc$p.id)) #12

# how many papers are classified as applicaiton-centric
ac <- data[data$f.cat0=="application-centric",]
length(unique(ac$p.id)) #24

total.data <- length(unique(dc$p.id))
total.human <- length(unique(hc$p.id))
total.app <- length(unique(ac$p.id))

# How to visualiza these sets? Radial Sets use an alternative visual metaphor to represent overlapping sets compared to Euler Diagram 
dcu <- unique(dc$p.id)
hcu <- unique(hc$p.id)
acu <- unique(ac$p.id)
count(hcu %in% dcu)
count(acu %in% dcu)
count(acu %in% hcu)


subsetCat <- data[,c("p.id", "f.cat0","f.cat1", "f.cat2", "p.year")]

#### What are the distribution of papers along the main categories?
## Run the function length() on the value of "f.cat0" for each group (f.cat0) 
subsetCat0 <- ddply(subsetCat, c("f.cat0"), summarise, 
                    countCat0  = length(unique(p.id)))

############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig04a.jpg",width=4*ppi, height=5*ppi, res=ppi, quality=100)

## graph of counts, reorder to show counts of "f.cat0" in order
ggplot(subsetCat0, aes(x=reorder(f.cat0, countCat0), y=countCat0, fill=f.cat0)) +
    geom_bar(stat="identity", width=0.4, colour="black") + 
    #coord_flip() +
    theme_bw(base_family = "Times", base_size=10) + 
    geom_text(aes(label=countCat0), vjust=1.5, colour="black", size=3) +
    scale_y_continuous(breaks=c(seq(0,60,5))) +
    labs(x = "Main categories") + 
    labs(y = "Number of papers") + 
    #labs(title = "Number of papers by main categories") +
    scale_colour_brewer(palette="Set2") +
    theme(legend.position="none") + # remove legend
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) # Hide the horizontal grid lines
dev.off()
############ END FINAL FIGURE #################


subsetUseCases <- data[,c("p.id", "f.cat0","f.cat1", "f.cat2", "f.uc0", "f.uc1", "p.year")]

#### What are the distribution of use cases along the main categories?
## Run the function length() on the value of "f.cat0" for each group (f.cat0) 
subsetUseCases0 <- ddply(subsetUseCases, c("f.cat0", "f.uc0"), summarise, 
                         countCat0  = length(unique(p.id)))
#                         pct = countCat0 / subsetCat0[subsetUseCases$f.cat0,]$countCat0)

levels(subsetUseCases0$f.uc0)[levels(subsetUseCases0$f.uc0)=="natural harzards and man-made events"] <- "natural harzards and\n man-made events"

# Turn NA as a factor level
subsetUseCases0$f.uc0 <- addNA(subsetUseCases0$f.uc0)
# Rename level of a factor by index: change fourh item, NA, to "Not specified".
levels(subsetUseCases0$f.uc0)[4] <- "not specified"
levels(subsetUseCases0$f.uc0)


subsetUseCases0[1, c("pct")] <- percent(subsetUseCases0[1, c("countCat0")] / total.app)
subsetUseCases0[2, c("pct")] <- percent(subsetUseCases0[2, c("countCat0")] / total.app)
subsetUseCases0[3, c("pct")] <- percent(subsetUseCases0[3, c("countCat0")] / total.app)
subsetUseCases0[4, c("pct")] <- percent(subsetUseCases0[4, c("countCat0")] / total.data)
subsetUseCases0[5, c("pct")] <- percent(subsetUseCases0[5, c("countCat0")] / total.data)
subsetUseCases0[6, c("pct")] <- percent(subsetUseCases0[6, c("countCat0")] / total.data)
subsetUseCases0[7, c("pct")] <- percent(subsetUseCases0[7, c("countCat0")] / total.data)
subsetUseCases0[8, c("pct")] <- percent(subsetUseCases0[8, c("countCat0")] / total.human)
subsetUseCases0[9, c("pct")] <- percent(subsetUseCases0[9, c("countCat0")] / total.human)
subsetUseCases0[10, c("pct")] <- percent(subsetUseCases0[10, c("countCat0")] / total.human)
subsetUseCases0[11, c("pct")] <- percent(subsetUseCases0[11, c("countCat0")] / total.human)

############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig04b.jpg",width=4*ppi, height=5*ppi, res=ppi, quality=100)

## graph of counts, reorder to show counts of "f.cat0" in order
ggplot(subsetUseCases0, aes(x=reorder(f.cat0, countCat0), y=countCat0, fill=f.uc0)) +
  geom_bar(stat="identity", width=0.4, colour="black") + 
  #coord_flip() +
  theme_bw(base_family = "Times", base_size=10) + 
  #geom_text(aes(label=pct), vjust=1.5*subsetUseCases0$pct, colour="black", size=3) +
  scale_y_continuous(breaks=c(seq(0,40,5))) +
  labs(x = "Main categories") + 
  labs(y = "Number of papers") + 
  #labs(title = "Number of papers by main categories") +
  scale_color_brewer(palette="Set2") +
  labs(fill="Use cases") +   # set the legend title
  theme(legend.position=c(0,1), legend.justification=c(0,1)) +  # set legend position inside graphic, tpo-left position    
  theme(legend.background=element_blank()) + # Remove overall border of legend
  # theme(legend.position="none") + # remove legend
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) # Hide the horizontal grid lines
dev.off()
############ END FINAL FIGURE #################





#### What are the distribution of sub-categories/focuses within each category?
## Run the function length() on the value of "f.cat1" for each group (f.cat0, f.cat1) 
## to sum the ocurrences  of f.cat1 within each group
subsetCat1 <- ddply(subsetCat, c("f.cat0", "f.cat1"), summarise, 
                        countCat1 = length(f.cat1))
                        #countCat1 = length(unique(f.cat1)))

# Get the sub-category (f.cat1), sorted first by category (f.cat0), then by count  
cat1order <- subsetCat1$f.cat1[order(subsetCat1$f.cat0, subsetCat1$countCat1)]
# Turn f.cat1 into a factor, with levels in the order of cat1order
subsetCat1$f.cat1 <- factor(subsetCat1$f.cat1, levels=cat1order)
############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/fig06.jpg",width=6*ppi, height=5*ppi, res=ppi, quality=100)

ggplot(subsetCat1, aes(x=f.cat1, y=countCat1, fill=f.cat0)) +
    geom_bar(stat="identity", width=0.6, colour="black") + 
    coord_flip() +
    theme_bw(base_family = "Times", base_size=10) + 
    theme(panel.grid.major.y = element_blank()) +
    scale_colour_brewer(palette="Set1") +
    #scale_fill_brewer("clarity") +
    geom_text(aes(label=countCat1), hjust=1.5, colour="black", size=3) +
    scale_y_continuous(breaks=c(seq(0,30,5))) +
    labs(x = "Sub-category / focus") + 
    labs(y = "Number of papers") +
    guides(fill=guide_legend(title="Main categories")) +  # Set the legend title
    theme(legend.position=c(1,.1), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    theme(legend.background=element_blank()) # Remove overall border of legend
    #labs(title = "Number of paper by focus (fcat1) and categories (fcat0)") +
dev.off()    

############ END FINAL FIGURE #################

### Put Figure 5 and use case distribution  side by side
ppi=600
jpeg(filename = "./figures/fig05-06.jpg",width=5*ppi, height=5*ppi, res=ppi, quality=100)

plot1<- ggplot(subsetCat0, aes(x=reorder(f.cat0, countCat0), y=countCat0, fill=f.cat0)) +
        geom_bar(stat="identity", width=0.4, colour="black") + 
        #coord_flip() +
        theme_bw(base_family = "Times", base_size=10) + 
        geom_text(aes(label=countCat0), vjust=1.5, colour="black", size=3) +
        scale_y_continuous(breaks=c(seq(0,60,5))) +
        labs(x = "Main categories") + 
        labs(y = "Number of papers") + 
        #labs(title = "Number of papers by main categories") +
        scale_colour_brewer(palette="Set1") +
        theme(legend.position="none") + # remove legend
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) # Hide the horizontal grid lines
plot2<-ggplot(subsetCat1, aes(x=f.cat1, y=countCat1, fill=f.cat0)) +
    geom_bar(stat="identity", width=0.4, colour="black") + 
    coord_flip() +
    theme_bw(base_family = "Times", base_size=10) + 
    theme(panel.grid.major.y = element_blank()) +
    scale_colour_brewer(palette="Set1") +
    #scale_fill_brewer("clarity") +
    geom_text(aes(label=countCat1), hjust=1.5, colour="black", size=3) +
    scale_y_continuous(breaks=c(seq(0,30,5))) +
    labs(x = "Sub-category / focus") + 
    labs(y = "Number of papers") +
    guides(fill=guide_legend(title="Main categories")) +  # Set the legend title
    theme(legend.position=c(1,.1), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    theme(legend.background=element_blank()) # Remove overall border of legend
    #labs(title = "Number of paper by focus (fcat1) and categories (fcat0)") +


library(grid)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

dev.off()    
#### What are the most frequently intended uses within each focus?
## Run the function length() on the value of "f.cat2" for each group (f.cat0, f.cat1, f.cat2) 
## to sum the ocurrences  of f.cat2 within each group
subsetCat2 <- ddply(subsetCat, c("f.cat0", "f.cat1", "f.cat2"), summarise, 
                    #countCat2 = length(f.cat2))
                    countCat2 = length(unique(p.id)))

dataCentric <- subset(subsetCat2,f.cat0=="data-centric")

# Get the intended uses (f.cat2), sorted first by cat1 (focus), then by count  
cat2order <- dataCentric$f.cat2[order(dataCentric$f.cat1, dataCentric$countCat2)]
# Turn f.cat2 into a factor, with levels in the order of cat2order
dataCentric$f.cat2 <- factor(dataCentric$f.cat2, levels=cat2order)

# A trick to change legend title is to rename the column in the dataframe
names(dataCentric)[names(dataCentric)=="f.cat1"]  <- "Focus"

# Change legend labels to add the total number of papers that belong to f.cat1. This make previus figure unecessary
dataCentric.limits <- c("data preservation",
                        "data preparation",
                        "data policies", 
                        "data contextualization", 
                        "data quality and assessment")

dataCentric.legend <- c("data preservation (1)",
                        "data preparation (26)",
                        "data policies (2)",
                        "data contextualization (15)",
                        "data quality and assessment (15)")

############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig05.jpg",width=9*ppi, height=5*ppi, res=ppi, quality=100)

ggplot(dataCentric, aes(x=f.cat2, y=countCat2, fill=Focus)) +    
    geom_bar(stat="identity", width=0.6, colour="black") + 
    coord_flip() +
    theme_bw(base_family = "Times", base_size=10) +
    scale_colour_brewer(palette="Set2") +
    scale_y_continuous(breaks=c(seq(0,15,1))) +
    labs(y = "Number of papers") + 
    labs(x = "Intended uses within data-centric category") + 
    #labs(title = "Data-centric category broken by Focus and Intended Use") +
    geom_text(aes(label=countCat2), hjust=1.5, colour="black", size=3) +
    theme(legend.position=c(1,.1), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-roght position    
    theme(legend.background=element_blank()) + # Remove overall border of legend
    scale_fill_discrete(
      limits=dataCentric.limits,
      labels=dataCentric.legend) +  # custom legend labels
    #theme(legend.key=element_blank()) + # Remove border around each item of legend
    theme(panel.grid.major.y = element_blank()) # No horizontal grid lines

dev.off()
############ END FINAL FIGURE #################
names(dataCentric)[names(dataCentric)=="Focus"]  <- "f.cat1"

# A trick to change legend title is to rename the column in the dataframe
names(dataCentric)[names(dataCentric)=="f.cat1"]  <- "Focus"

ppi=300
jpeg(filename = "./figures/fig07-dots.jpg",width=9*ppi, height=5*ppi, res=ppi, quality=100)

ggplot(dataCentric, aes(x=countCat2, y=f.cat2)) +    
    geom_point(size=3, aes(colour=Focus)) +    # Use a larger dot
    geom_segment(aes(yend=f.cat2), xend=0, colour="grey50") +
    theme_bw(base_family = "Times", base_size=10) +
    scale_colour_brewer(palette="Set1") +
    scale_x_continuous(breaks=c(seq(0,15,1))) +
    labs(x = "Number of papers") + 
    labs(y = "Intended uses within data-centric category") + 
    #labs(title = "Data-centric category broken by Focus and Intended Use") +
    theme(legend.position=c(1,.1), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-roght position    
    theme(legend.background=element_blank()) + # Remove overall border of legend
    #theme(legend.key=element_blank()) + # Remove border around each item of legend
    theme(panel.grid.major.y = element_blank()) # No horizontal grid lines

dev.off()
names(dataCentric)[names(dataCentric)=="Focus"]  <- "f.cat1"


ppi=300
jpeg(filename = "./figures/fig07-optionA.jpg",width=9*ppi, height=5*ppi, res=ppi, quality=100)

## We use a dot plot faceted by focus (f.cat1)
ggplot(dataCentric, aes(x=countCat2, y=f.cat2)) +    
    geom_point(size=3, aes(colour=f.cat1)) +    # Use a larger dot
    geom_segment(aes(yend=f.cat2), xend=0, colour="grey50") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_colour_brewer(palette="Set1", guide=FALSE) +
    scale_x_continuous(breaks=c(seq(0,8,1))) +
    labs(x = "Number") + 
    labs(y = "Intended uses (fcat2)") + 
    labs(title = "Data-centric category by Focus and Intended Use") +
    theme(panel.grid.major.y = element_blank()) + # No horizontal grid lines
    facet_grid(. ~ f.cat1, scales="free_y", space="free_y")
  
dev.off()


ggplot(dataCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="grey90") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus (fcat1)") + 
    labs(y = "Intended uses (fcat2)") + 
    labs(title = "Data-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + # Rotating the text 30 degrees
    geom_text(aes(label=countCat2), vjust=0, colour="grey30", size=3)   # Add labels from data


#### What are the most frequently intended uses within each focus?
ggplot(subsetCat2, aes(x=reorder(f.cat2, countCat2), y=countCat2, fill=f.cat1)) +
    geom_bar(stat="identity") + coord_flip() +
    theme_bw(base_family = "Avenir", base_size=10) + 
    theme(panel.grid.major.y = element_blank()) +   # No horizontal grid lines
    geom_text(aes(label=countCat2), hjust=1.5, colour="white") +
    labs(x = "Intended uses") + 
    labs(y = "Count") + 
    labs(title = "Counts of intended use (fcat2) broken by focus (fcat1)") 
    

# Alternative graph: Split subsetCat2 into 3 dataframes by f.cat0. 
dataCentric <- subset(subsetCat2,f.cat0=="data-centric")

# Then create a balloon plot with f.cat1 and f.cat2 as categorical axes
ggplot(dataCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="grey90") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus (fcat1)") + 
    labs(y = "Intended uses (fcat2)") + 
    labs(title = "Data-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + # Rotating the text 30 degrees
    geom_text(aes(label=countCat2), vjust=0, colour="grey30", size=3)   # Add labels from data


humanCentric <- subset(subsetCat2,f.cat0=="human-centric")
# Get the intended uses (f.cat2), sorted first by cat1 (focus), then by count  
cat2order <- humanCentric$f.cat2[order(humanCentric$f.cat1, humanCentric$countCat2)]
# Turn f.cat2 into a factor, with levels in the order of cat2order
humanCentric$f.cat2 <- factor(humanCentric$f.cat2, levels=cat2order)

# A trick to change legend title is to rename the column in the dataframe
names(humanCentric)[names(humanCentric)=="f.cat1"]  <- "Focus"

# Change legend labels to add the total number of papers that belong to f.cat1.
humanCentric.limits <- c("human relations",
                         "human perception",
                         "human mobility", 
                         "human activities")
                         
humanCentric.legend <- c("human relations (3)",
                         "human perception (1)",
                         "human mobility (4)", 
                         "human activities (5)")
                        
############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig06.jpg",width=6*ppi, height=5*ppi, res=ppi, quality=100)

    
ggplot(humanCentric, aes(x=f.cat2, y=countCat2, fill=Focus)) +    
    geom_bar(stat="identity", width=0.6, colour="black") +
    coord_flip() +
    theme_bw(base_family = "Times", base_size=10) +
    scale_colour_brewer(palette="Set2") +
    scale_y_continuous(breaks=c(seq(0,3,1))) +
    labs(y = "Number of papers") + 
    labs(x = "Intended uses within human-centric category") + 
    #labs(title = "Human-centric category broken by Focus and Intended Use") +
    geom_text(aes(label=countCat2), hjust=1.5, colour="black", size=3) +
    guides(fill=guide_legend(title="Focus")) +  # Set the legend title
    scale_fill_discrete(
      limits=humanCentric.limits,
      labels=humanCentric.legend) +  # custom legend labels
    #theme(legend.position=c(1.5,.1), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    #theme(legend.background=element_blank()) + # Remove overall border of legend
    #theme(legend.key=element_blank()) + # Remove border around each item of legend
    theme(panel.grid.major.y = element_blank()) # No horizontal grid lines

dev.off()
############ END FINAL FIGURE #################
names(humanCentric)[names(humanCentric)=="Focus"]  <- "f.cat1"


# A trick to change legend title is to rename the column in the dataframe
names(humanCentric)[names(humanCentric)=="f.cat1"]  <- "Focus"

ppi=300
jpeg(filename = "./figures/fig08-dots.jpg",width=7*ppi, height=5*ppi, res=ppi, quality=100)

ggplot(humanCentric, aes(x=countCat2, y=f.cat2)) +    
    geom_point(size=3, aes(colour=Focus)) +    # Use a larger dot
    geom_segment(aes(yend=f.cat2), xend=0, colour="grey50") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_colour_brewer(palette="Set1") +
    scale_x_continuous(breaks=c(seq(0,3,1))) +
    labs(x = "Number of papers") + 
    labs(y = "Intended uses within human-centric category") + 
    #labs(title = "Human-centric category broken by Focus and Intended Use") +
    guides(fill=guide_legend(title="Focus")) +  # Set the legend title
    #theme(legend.position=c(1.5,.1), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    #theme(legend.background=element_blank()) + # Remove overall border of legend
    #theme(legend.key=element_blank()) + # Remove border around each item of legend
    theme(panel.grid.major.y = element_blank()) # No horizontal grid lines

dev.off()
names(humanCentric)[names(humanCentric)=="Focus"]  <- "f.cat1"



ggplot(humanCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="grey90") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus (fcat1)") + 
    labs(y = "Intended uses (fcat2)") + 
    labs(title = "Human-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + # Rotating the text 30 degrees
    geom_text(aes(label=countCat2), vjust=0, colour="grey30", size=3)   # Add labels from data
    

applicationCentric <- subset(subsetCat2,f.cat0=="application-centric")
# Get the intended uses (f.cat2), sorted first by cat1 (focus), then by count  
cat2order <- applicationCentric$f.cat2[order(applicationCentric$f.cat1, applicationCentric$countCat2)]
# Turn f.cat2 into a factor, with levels in the order of cat2order
applicationCentric$f.cat2 <- factor(applicationCentric$f.cat2, levels=cat2order)

# A trick to change legend title is to rename the column in the dataframe
names(applicationCentric)[names(applicationCentric)=="f.cat1"]  <- "Focus"

# Change legend labels to add the total number of papers that belong to f.cat1. This make previus figure unecessary
appCentric.limits <- c("recovery and response",
                       "monitoring",
                       "health",
                       "detection and prediction",
                       "coordination and organization")

appCentric.legend <- c("recovery and response (8)",
                       "monitoring (9)",
                       "health (2)",
                       "detection and prediction (5)",
                       "coordination and organization (1)")

############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig07.jpg",width=6*ppi, height=4*ppi, res=ppi, quality=100)

ggplot(applicationCentric, aes(x=f.cat2, y=countCat2, fill=Focus)) +    
    geom_bar(stat="identity", width=0.6, colour="black") +
    coord_flip() +
    theme_bw(base_family = "Times", base_size=10) +
    scale_colour_brewer(palette="Set1") +
    scale_y_continuous(breaks=c(seq(0,4,1))) +
    labs(y = "Number of papers") + 
    labs(x = "Intended uses within application-centric category") + 
    #labs(title = "Application-centric category broken by Focus and Intended Use") +
    geom_text(aes(label=countCat2), hjust=1.5, colour="black", size=3) +
    scale_fill_discrete(
      limits=appCentric.limits,
      labels=appCentric.legend) +  # custom legend labels
    #theme(legend.position=c(1,0), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    #theme(legend.background=element_blank()) + # Remove overall border of legend
    #theme(legend.key=element_blank()) + # Remove border around each item of legend
    theme(panel.grid.major.y = element_blank()) # No horizontal grid lines

dev.off()
############ END FINAL FIGURE #################
names(applicationCentric)[names(applicationCentric)=="Focus"]  <- "f.cat1"


# A trick to change legend title is to rename the column in the dataframe
names(applicationCentric)[names(applicationCentric)=="f.cat1"]  <- "Focus"

ppi=300
jpeg(filename = "./figures/fig09-dots.jpg",width=6*ppi, height=5*ppi, res=ppi, quality=100)
ggplot(applicationCentric, aes(x=countCat2, y=f.cat2)) +    
    geom_point(size=3, aes(colour=Focus)) +    # Use a larger dot
    geom_segment(aes(yend=f.cat2), xend=0, colour="grey50") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_colour_brewer(palette="Set1") +
    scale_x_continuous(breaks=c(seq(0,4,1))) +
    labs(x = "Number of papers") + 
    labs(y = "Intended uses within application-centric category") + 
    #labs(title = "Application-centric category broken by Focus and Intended Use") +
    theme(legend.position=c(1,0), legend.justification=c(1,0)) +  # set legend position inside graphic, bottom-right position    
    theme(legend.background=element_blank()) + # Remove overall border of legend
    #theme(legend.key=element_blank()) + # Remove border around each item of legend
    theme(panel.grid.major.y = element_blank()) # No horizontal grid lines

names(applicationCentric)[names(applicationCentric)=="Focus"]  <- "f.cat1"


ggplot(applicationCentric, aes(x=f.cat1, y=f.cat2)) +
    geom_point(aes(size=countCat2), shape=21, colour="black", fill="grey90") +
    theme_bw(base_family = "Avenir", base_size=10) +
    scale_size_area(max_size=15, guide=FALSE) +
    labs(x = "Focus (fcat1)") + 
    labs(y = "Intended uses (fcat2)") +
    labs(title = "Appplication-centric category broken by Focus and Intended Use") +
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + # Rotating the text 30 degrees
    geom_text(aes(label=countCat2), vjust=0, colour="grey30", size=3)   # Add labels from data



#### Summary: distribution of intended uses over time
## Run the function length() on the value of "f.cat2" for each group (f.cat0, f.cat1, f.cat2, p.year) 
## to sum the ocurrences  of f.cat2 within each group
subsetCat3 <- ddply(subsetCat, c("f.cat0", "f.cat1", "f.cat2", "p.year"), summarise, 
                    countCat3 = length(f.cat2))

# Get the intended uses (f.cat2), sorted first by cat1 (focus), then by count
cat2order <- subsetCat3$f.cat2[order(subsetCat3$f.cat1, subsetCat3$countCat3)]
# Turn f.cat2 into a factor, with levels in the order of cat2order
subsetCat3$f.cat2 <- factor(subsetCat3$f.cat2, levels=cat2order)

############ FINAL FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig9-new.jpg",width=6*ppi, height=9*ppi, res=ppi, quality=100)
ggplot(subsetCat3, aes(x=p.year, y=f.cat2, colour=f.cat0)) +
    geom_point(aes(size=countCat3)) + 
    scale_size_continuous(range=c(1,6)) +  # range of values for dots size (number of papers)
    theme_bw(base_family = "Times", base_size=10) + 
    scale_size_area(max_size=12) +   # scale dots to make them bigger
    labs(colour="Category", size="Number of\n papers") +
    labs(x = "Year of publication") + 
    labs(y = "Intended uses") + 
    #labs(title = "Intended uses over time") +
    geom_text(aes(label=countCat3), vjust=0.1, colour="black", size=2)   # Add labels from data
    
dev.off()
############ END FINAL FIGURE #################


#################
## What about the distribution of end users? How are they related to intended uses
## What about the specification of user case scenarios related to intended uses

## Notes on the variables:
## f.uc0: Factor(natural harzards, man-made events), it groups the overall use case of the paper into main categories
## f.uc1: Character, it describes the overall use case of the paper
## f.user: Factor(several levels), it describes the stakeholders or end users targeted in the paper
#################

###########################################################
#                       ONGOING
###########################################################

treatment <- factor(rep(c(1, 2), c(43, 41)), levels = c(1, 2),
                    labels = c("placebo", "treated"))
improved <- factor(rep(c(1, 2, 3, 1, 2, 3), c(29, 7, 7, 13, 7, 21)),
                   levels = c(1, 2, 3),
                   labels = c("none", "some", "marked"))

# The widths of the columns is proportional to the number of samples in each of the corresponding age categories
spineplot(improved, treatment)

subsetUseCases <- data[,c("f.cat0","f.cat1", "f.cat2", "f.uc0", "f.uc1", "p.year")]

#### What are the distribution of use cases along the main categories?
## Run the function length() on the value of "f.cat0" for each group (f.cat0) 
subsetUseCases0 <- ddply(subsetUseCases, c("f.cat0", "f.uc0"), summarise, 
                    countCat0  = length(f.cat0), 
                    pct = countCat0 / sum(countCat0))

levels(subsetUseCases0$f.uc0)[levels(subsetUseCases0$f.uc0)=="natural harzards and man-made events"] <- "natural harzards and\n man-made events"

# Turn NA as a factor level
subsetUseCases0$f.uc0 <- addNA(subsetUseCases0$f.uc0)
# Rename level of a factor by index: change fourh item, NA, to "Unknown".
levels(subsetUseCases0$f.uc0)[4] <- "Unknown"
levels(subsetUseCases0$f.uc0)

############ TENTATIVE FIGURE #################
ppi=600
jpeg(filename = "./figures/ceus-fig05b.jpg",width=4*ppi, height=5*ppi, res=ppi, quality=100)

## graph of counts, reorder to show counts of "f.cat0" in order
ggplot(subsetUseCases0, aes(x=reorder(f.cat0, countCat0), y=countCat0, fill=f.uc0)) +
    geom_bar(stat="identity", width=0.4, colour="black") + 
    #coord_flip() +
    theme_bw(base_family = "Times", base_size=10) + 
    #geom_text(aes(label=countCat0), vjust=1.5, colour="black", size=3) +
    #scale_y_continuous(breaks=c(seq(0,60,5))) +
    labs(x = "Main categories") + 
    labs(y = "Number of papers") + 
    #labs(title = "Number of papers by main categories") +
    scale_color_brewer(palette="Set2") +
    labs(fill="Use cases") +   # set the legend title
    theme(legend.position=c(0,1), legend.justification=c(0,1)) +  # set legend position inside graphic, tpo-left position    
    theme(legend.background=element_blank()) + # Remove overall border of legend
    # theme(legend.position="none") + # remove legend
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) # Hide the horizontal grid lines
dev.off()
############ END TENTATIVE FIGURE #################


subsetUsers <- data[,c("f.cat0","f.cat1", "f.cat2", "f.uc0", "f.uc1", "f.user", "p.year")]

#### What are the distribution of use cases along the main categories?
## Run the function length() on the value of "f.cat0" for each group (f.cat0) 
subsetUsers0 <- ddply(subsetUsers, c("f.cat0", "f.uc0", "f.user"), summarise, 
                         countCat0  = length(f.cat0), 
                         pct = countCat0 / sum(countCat0))






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


