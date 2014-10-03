Codebook
========================================================

It describes the list of variables of our datase

## Variables about paper's bibliographic details, prefix "p" stands for "paper"

* **p.id**: Character, the unique identifier of the paper (ex. p01) which matches the file name accrodingly
* **p.title**: Character, the title of the paper
* **p.year**: Integer, the publication year of the paper
* **p.venue**: Character/factor, the name of the venue where the paper is published 
* **p.venuetype**: Factor (journal/conference/technical report), type of the paper 
* **p.keyword**: Character, the list of keywords of the paper
* **p.url**: Character, the link to the paper when possible
* **p.geo**: Factor (strong, medium, weak), it subjectivelly qualifies the overall use of VGI and geospatial methods used in the paper
* **p.notes**: Character, additional comments referred to the qualitative level fo geo, when required

## Variables about focus and intended use, prefix "f" stands for "focus"
* **f.cat0**: Factor (data-centric, human-centric, crisis-centric), group or main category of the paper
* **f.cat1**: Factor (several levels), the main focus of the paper
* **f.cat2**: Factor (several levels), the intended  use within the main focus
* **f.uc0**: Factor(natural harzards, man-made events, naturzal harzards and man-made events), it groups the overall use case of the paper into main categories
* **f.uc1**: Character, it describes the overall use case of the paper
* **f.user**: Factor(several levels), it describes the stakeholders or end users targeted in the paper

## Variables about data sources employed in each paper, prefix "d" stands for "data"
* **d.source**: Character, it lists the social media soruces employed i nthe paper
* **d.vgi**: Character, it denotes additional comments/notes regarding VGI sources
* **d.official**: Factor (yes/no), it indicates if official data is used in conjunction with social sources
* **d.reference**: Character, it denotes what reference/official data is used  

## Variables about type of analisys and analytical methods employed in each paper, prefix "a" stands for "analysis"
* **a.type**: Factor (descriptive, exploratory, inferential, predictive), the overall type of analysis conducted
* **a.collec**: Character, it decribes methods/protocol used for data collection
* **a.prep1**: Character, it describes manual data analysis and crowdsourcing methods used in data preparation
* **a.prep2**: Character, it describes automated data analysis methods (NLP, ML, etc.) used in data preparation
* **a.prep3**: Character, it describes specific geospatial data analysis used in data preparation
* **a.analysis1**: Character, it describes (social) network analysis used in data analysis
* **a.analysis2**: Character, it describes applied statistics analysis used in data analysis
* **a.analysis3**: Character, it describes specific geospatial analysis used in data analysis
* **a.viz**: Character, it describes specific geospatial methods to show or visualize results 
* **a.tool**: Factor (yes/no), is the analytical outcome of the paper a tool?
