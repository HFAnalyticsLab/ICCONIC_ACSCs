## International Comparison of Equity Gradients For Ambulatory Care Sensitive Condition Hospitalizations 

## Project Status: [Ongoing]

## Project Description

Hospitalisation for ambultory care sensitive conditions (ACSCs) are conditions are seen to be prevantable in ambulatory care (including primary care) with timely prevention, management and interventions. It can also be used as an indicator to asses health systems. There is also socioeconomic inequalities between ACSCs hospitalisation. ACSC hospitalizations may therefore serve as both a proxy measure of primary care access and underlying inqualities. However, there is a lack cross-country evidence and whether these inequalities are consistent across different health systems. 

The International Collaborative on Costs, Outcomes, and Needs in Care (ICCONIC) program sought to examine the differences in the gradient of ACSC relative to socioeconomic status across eight high-incomehigh income countries. The aim of this study was to determine whether there are differences across countries in the SES gradient in ACSC hospitalizations. 

This repository outlines how the England data was extracted, aggregated and visualised for the cross-country comparison analysis. 

## Outputs

Peer review paper 

## Data sources:

* [Hospital Episode Statistics- Admitted Patient Care datatabse](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5837677/) 

* [ONS Population estimates by Index of Multiple Deprivation Deciles](https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12413deathregistrationsandpopulationsbyindexofmultipledeprivationimddecileenglandandwales2019/deathsandpopsbyimd2019final.xlsx)

## How does it work? 

This repository outlines how to prepare the prepare and process our cohort, analyse and visualise the dataset used in the cross-country analysis. 

It must be noted that we use AWS S3 cloud storage therefore where necessary the code for saving and retrieving files will need to be adapted based on your local storage structure. 

### Requirements 

These scripts were written in R version 4.0.02 and RStudio Workbench Version 1.1.383. The following R packages (available on CRAN) are needed:

* tidyverse
* curl
* gtsummary
* here
* readxl
* janitor
* ggplot2 
* tsibble 

In addition our plots make use of our in house style package [THFstyle](https://github.com/THF-evaluative-analytics/THFstyle) available here on GitHub.

### Getting started

The src folder contains 
* [ACSC_admissions_file_prep.R](https://github.com/HFAnalyticsLab/ICCONIC_ACSCs/blob/main/ACSC_admissions_file_prep.R) - Data extract of ACSC hospital admissions in 2019 from HES data.
* [ACSC_descriptive_file.R](https://github.com/HFAnalyticsLab/ICCONIC_ACSCs/blob/main/ACSC_descriptive_file.R)- Aggregation of the ACSC hospital admissions based on our age groups, gender and IMD groupings. 
* [pop_estimates.R](https://github.com/HFAnalyticsLab/ICCONIC_ACSCs/blob/main/pop_estimates.R) - Download population estimats data and aggregates the based on our age groups, gender and IMD groupings. 
* [calculate_rates.R](https://github.com/HFAnalyticsLab/ICCONIC_ACSCs/blob/main/calculate%20_rates.R)- Extracts the ACSC hospitalisaiton aggregate data, population data and compiles it based on the data tables produced by the primary investigators. 

## Authors
* Anne Alarilla - [Twitter](https://twitter.com/AlarillaAnne) - [GitHub](https://github.com/annealarilla)
* Mai Stafford - [Twitter](https://twitter.com/stafford_xm) - [GitHub](https://github.com/maistafford)
* George Stevenson 

Project team also consisted of member of the ICCONIC collaboration. 

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/ambulance_and_emergency_care/blob/main/LICENSE).


## Acknowledgements
