# Role of VGI in Science

## Synopsis
This is a bit of exploration to discover current trends in analysis methods applied to the use of VGI (Volunteered Geographic Information) in scientific contexts in general and GIScience in particular. The aim is to perform a systematic mapping review to know the focus and intended use of VGI along with the type of data analsysis and analitical methods used. We are aware that VGI is being used in multiple domains (health monitoring, health prediction adn detection, environmental monitoring, citizen science, data journalism, social sciences, etc) but here the representaive set of papers is mainly limited to the field of natural harzards and crisis management, for which there exists a great deal of related literature since these tow domains have been traditionaly early adopters of VGI.

## Context

The work makes sense in the context of a recent GIScience 2014 workshop on the [Role of Volunteered Geographic Information in Advancing Science: Effective Utilization](http://web.ornl.gov/sci/gist/workshops/2014/index.shtml). We submitted a [position paper](https://web.ornl.gov/registration_resumes/GIScience@workshop-submitted.pdf) which is a brief introduction of this work. The workshop was driven by a set of research questions around two main themes. 

### Opportunities and challenges of VGI utilization

Q1. How do you see the uprising of 'Big Data' era influencing the way VGI is created and used and what are opportunities/challenges in that context?

Q2. How does one generate future requirements for pervasive VGI data creation and on-the-fly processing/analysis?

Q3. What new methods and standards are needed for incorporating VGI in scientific analysis?

Q4. How do we address the data life cycle for VGI?

 
### Applications of VGI in science and operations
 
Q1. How is the spatial component, such as scale and accuracy, of VGI being handled in scientific or operational applications?

Q2. How are the limitations of VGI being dealt with in an applied spatial modeling environment?

Q3. What would be one specific aspect that you consider missing or under-represented in the current 'VGI landscape'?

Q4. Does VGI have a potential to impact governance?

## Results

We analysed a list of scientific documents that are hosted in the `papers` folder. by reviewing these papers, we produced two distinct datasets. Each dataset is the source data for two research works, which results have been submitted to GEIN and TGIS. That's why the data folder is divided into GEIN and TGIS, and scripts names for the TGIS journal have the suffix "tgis". Scripts for the GEIN do not append "gein" to their file names. 

### Folder organization (GEIN)

* Data folder contains: 

** the original data file `data.xlxs` we used for taking notes while we conducted the review of papers. This file is not used in the data analysis; 

** the raw data file `rawData.csv`, which is the csv version of the previous file, is the starting point for the  R data analysis in R; 

** and the `reviewData.rda`, which is the result of doing some data cleaning and preparation tasks for the subsequest analysis.  

* Text folder keeps associated documents such as the `codebook` that describes each column of the `reviewData.rda` dataset, and will contain the markdown file of the final analysis.

* Paper folder will keep a pdf copy of the set of eligible papers for the review.

* scripts folder contains the R scripts used in the exploratory data analysis. 

### Folder organization (TGIS)

## How to run the scripts

To reproduce all analysis steps:

1. Update the variable `workingPath` in script `cleaning.R` or `cleaning.tgis.R` to your local working directory.
2. Run the script `cleaning.R` (or `cleaning.tgis.R`() , which downloads the required input data and produces the file `cleandata.rda` in your working directory.  
3. Update the variable `workingPath` in script `analytics.R` (or `analytics.tgis.R`) to your local working directory.
4. Run the script `analytics.R` (`analytics-tgis.R`) which takes the file in step 2 as input. 

Alternatively, go straight to step 3 to skip data cleaning step.

