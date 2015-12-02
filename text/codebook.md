Codebook
========================================================

It describes the list of variables of our dataset of eligible papers

## Variables about paper's bibliographic details, prefix "p" stands for "paper"

* **p.id**: Character, the unique identifier of the paper (ex. p01) which matches the file name of the PDF file.
* **p.title**: Character, the title of the paper.
* **p.year**: Integer, the publication year of the paper.
* **p.venue**: Factor (several levels), the name of the venue where the paper has been presented/published. 
* **p.venuetype**: Factor (journal/conference), publication type. 
* **p.keyword**: Character, the list of keywords (sep ;) as they appear in the paper. Otherwise NA
* **p.url**: Character, the link to the paper when possible. 
* **p.geo**: Factor (strong, medium, weak), it subjectivelly qualifies the overall use of VGI and geospatial methods used in the paper.
* **p.notes**: Character, additional comments if required. Otherwise blank space.
* **p.abstract**: Character, the abstract of the paper.

## Variables about focus and intended use, prefix "f" stands for "focus"
* **f.cat0**: Factor (data-centric, human-centric, application-centric), group or main category of the paper given by us.
* **f.cat1**: Factor (several levels), the main focus of the paper given by us.
* **f.cat2**: Factor (several levels), the intended use within the main focus given by us.
* **f.uc0**: Factor (natural harzards, man-made events, natural harzards and man-made events), it classifies the overall scope of a paper as it is reported in the paper. Otherwise NA.
* **f.uc1**: Factor (several factors), it specified the thematic scenario (e.g.floods, earthquake, riots, etc) as reported in the paper. Otherwise NA.
* **f.uc2**: Character, it describes the particular event or disaster being reported by the paper, that is, where it happened. Otherwise NA.
* **f.user**: Factor (several levels), it describes the stakeholders or end users of a paper when they are explicitely mentioned. Otherwise NA.

## Variables about data sources employed in each paper, prefix "d" stands for "data"
* **d.source**: Character, it lists the social media/VGI sources employed. 
* **d.vgi**: Character, additional comments/notes about how the VGI sources are used. 
* **d.official**: Factor (yes/no), it indicates if official data is used in conjunction with social media/VGI sources.
* **d.reference**: Character, it specifies what reference/official data is beiong used.  

## Variables about type of analysis and analytical methods employed in each paper, prefix "a" stands for "analysis"
* **a.type**: Factor (methodological, descriptive, exploratory, inferential, predictive), the overall type of analysis conducted.
* **a.collec**: Character, it describes methods/protocol used for data collection
* **a.prep1**: Character, it describes manual data analysis and crowdsourcing methods used during the data preparation phase.
* **a.prep2**: Character, it describes automated data analysis methods (NLP, ML, etc.) used during the data preparation phase.
* **a.prep3**: Character, it describes specific geospatial-realted methods used during the data preparation phase.
* **a.analysis1**: Character, it describes (social) network analysis used in data analysis
* **a.analysis2**: Character, it describes applied statistics analysis used in data analysis
* **a.analysis3**: Character, it describes specific geospatial analysis used in data analysis
* **a.viz**: Character, it describes specific geospatial methods to show or visualize results 
* **a.tool**: Factor (yes/no), if the paper describes a real-world application or tool.  This may be serve as an indicator of a paper's immediate practical relevance.
