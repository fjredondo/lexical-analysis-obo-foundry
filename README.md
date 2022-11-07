# lexical-analysis-obo-foundry
Supplementary data for the readability and the structural accuracy analysis of the OBO Foundry ontologies. The ontologies used in this analysis are available at [https://doi.org/10.5281/zenodo.6363060](https://doi.org/10.5281/zenodo.6363060).

# Description of the files and the folders

- The **data** folder initially contains the several files with links to download both the member and the candidate OBO Foundry ontologies. Additionally, a csv table including both the acronym and the complete title of the OBO Foundry ontologies has been included.

- The **scripts** folder contains the following scripts:
    - **metrics.jar**: Command line based java application to obtain the readability and the structural accuracy metrics for a set of ontologies. The source code is available [here](https://github.com/fjredondo/ontology-metrics).
    - **getMetricsMembers.sh**: Bash script to obtain the metrics for the OBO Foundry member ontologies. This script read the folder */data/ontologies/member_ontologies* and put the results in */results/members_results*.
    - **getMetricsMembers.sh**: Bash script to obtain the metrics for the OBO Foundry candidate ontologies. This script read the folder */data/ontologies/candidate_ontologies* and put the results in */results/candidates_results*.
    - **data_analysis.R**: R script intended to be opened in an interactive rstudio session. It contains the R commands used to perform the descriptive analysis, including basic statistics and plots. It also includes commented lines that help to follow the script.

- The **results** folder contains several subfolders and files with the results provided by each step, namely:
    - **candidates_results**: Folder including TSV files for each candidate ontology, including its metric values. A file called *allMetrics.tsv* is also included, in which the information about all the candidate ontologies is merged.
    - **members_results**: Folder including TSV files for each member ontology, including its metric values. A file called *allMetrics.tsv* is also included, in which the information about all the member ontologies is merged.
    - **detailed_files**: Folder including detailed files for each (ontology, metric) pair, including extra information at entity level.

# How to reproduce the results

## Requirements
- Git
- Java (tested on openjdk 11.0.11)
- R (tested on version 4.2.1)
- RStudio (tested on version 2022.07.2-576)
- R libraries:
    - caret (tested on version 6.0-92)
    - dplyr (tested on version 1.0.10)
    - ggplot2 (tested on version 3.3.6)
    - ggpubr (tested on version 0.4.0)
    - outForest (tested on version 0.1.2)
    - pvclust (tested on version 2.2-0)
    - stringr (tested on version 1.4.1)
    - tidyr (tested on version 1.2.1)

## Get the repository
The first step to reproduce our results is to download this repository into your computer with the command:
`git clone https://github.com/fjredondo/lexical-analysis-obo-foundry.git`

Once the repo is downloaded, enter into the folder:

`cd lexical-analysis-obo-foundry`

### Download the ontologies from Zenodo
In order to assess reproducibility, we uploaded the concrete versions of the ontologies we used for the study in the Zenodo platform at [https://doi.org/10.5281/zenodo.4701571](https://doi.org/10.5281/zenodo.4701571). Download the provided zip files for the candidate ontologies ([candidate_ontologies.zip](https://zenodo.org/record/4701572/files/candidate_ontologies.zip?download=1)) as well as the member ontologies ([member_ontologies.zip](https://zenodo.org/record/4701572/files/member_ontologies.zip?download=1)) and extract them in the folder *lexical-analysis-obo-foundry/data/ontologies/* so that you have the following folders:

- *lexical-analysis-obo-foundry/data/ontologies/candidate_ontologies*
- *lexical-analysis-obo-foundry/data/ontologies/member_ontologies*


## Calculate the metrics
The next step is to calculate the metrics for the ontologies previously downloaded. To achieve this, we provide the java application *metrics.jar*, located at the [scripts folder](./scripts). This application has the following parameters:

- -i,--input arg      The input. It can be a single ontology provided in OWL format, or a folder containing a set of ontologies in OWL format.
- -o,--output arg     The output. It will be a TSV file including the metrics for the ontology in the input.
- -v,--detailed-files If present, generate a report for each metric-ontology pair, consisting in detailed information at ontology class level.

Furthermore, we provide two bash scripts that encapsulates the use of *metrics.jar* to calculate the metrics for candidate and member ontologies:

- [getMetricsCandidates.sh](./scripts/getMetricsCandidates.sh): Calculate the metrics for the candidate ontologies, which are taken from the folder *lexical-analysis-obo-foundry/data/ontologies/candidate_ontologies*. The results are saved into the folder *lexical-analysis-obo-foundry/results/candidates_results*. This will create one TSV file per ontology including all the metrics for that ontology, together with a log file. Furthermore, a file called *allMetrics.tsv* will be also created, including all the metrics for all the ontologies. Moreover, detailed_files will be created into the detailed_files folder.

- [getMetricsMembers.sh](./scripts/getMetricsMembers.sh): Calculate the metrics for the member ontologies, which are taken from the folder *lexical-analysis-obo-foundry/data/ontologies/member_ontologies*. The results are saved into the folder *lexical-analysis-obo-foundry/results/members_results*. This will create one TSV file per ontology including all the metrics for that ontology, together with a log file. Furthermore, a file called *allMetrics.tsv* will be also created, including all the metrics for all the ontologies. Moreover, detailed_files will be created into the detailed_files folder.

Run the following commands to execute these scripts:
```
# Go to the scripts folder (assuming that you are on the root folder of this repository)
cd scripts

# Get metrics for the member ontologies
bash getMetricsMembers.sh

# Get metrics for the candidate ontologies
bash getMetricsCandidates.sh

# Move detailed files to the results folder
mv detailed_files ../results/
```

Once the above commands are executed, your results folder will contain a three folders:

- *candidates_results*: logs and TSV files with the metrics obtained for each candidate ontology. The file *allMetrics.tsv* contains the information about all the candidate ontologies and their metrics.
- *members_results*: logs and TSV files with the metrics obtained for each member ontology. The file *allMetrics.tsv* contains the information about all the member ontologies and their metrics.
- *detailed_files*: Set of TSV files including a more detailed information about each ontology-metric pair.

## Perform analysis
The descriptive analysis were performed by using the rstudio software. In order to reproduce them, open rstudio and load the corresponding file ([data_analysis.R](./scripts/data_analysis.R) for the descriptive analysis. Then, you can execute any of them line by line in an interactive way. Furthermore, these files include comments to clarify each section.
