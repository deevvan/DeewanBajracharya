![picture](vaccine.png)

# Observations of COVID-19 vaccine coverage and vaccine hesitancy on COVID-19 outbreak: An American ecological study 

## Read more about manuscript here:
> [__Bajracharya D, Jansen RJ.__ Observations of COVID-19 vaccine coverage and vaccine hesitancy on COVID-19 outbreak: An American ecological study. *Vaccine*, 2024.](https://pubmed.ncbi.nlm.nih.gov/38103963/)

### This README file describes the R script for analyzing vaccine hesitancy and hospitalization rates across US states. The script utilizes various R packages for data manipulation, visualization, and statistical analysis.

# Table of Contents

1. [Data Sources](#Data-Sources)
1. [System requirements](#System-requirements)
1. [Usage](#Usage)
1. [Input](#Input)
1. [Output](#Output)
1. [Test and model data](#Test-and-model-data)


## Data Sources

###    •    CDC hospitalization data: https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD
###    •    COVID Tracking Project data: https://covidtracking.com/data/download/all-states-history.csv
###    •    US Census data: (paths provided in script)
###    •    CDC vaccination data: https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD
###    •    CDC vaccination rates by vaccination status: https://data.cdc.gov/api/views/k3na-u7xf/rows.csv?accessType=DOWNLOAD


## Script Steps:
###    1    Load Libraries: The script begins by loading required R libraries, including dplyr, tidyr, ggplot2, and others for data manipulation and visualization.
###    2    Load Data: Several datasets are loaded from various sources using read.csv function. The script handles missing values and date formatting for each dataset.
###    3    State Hospitalization Data:
####           ◦    Daily hospitalization data is calculated by aggregating hospital admissions for each state across dates.
####            ◦    Data for territories (marked by specific codes) is excluded.
###    4    State Vaccination Data:
####            ◦    Daily vaccination data is calculated by summing vaccinations administered for each state across dates.
####            ◦    Data for territories is excluded.
####            ◦    Vaccination data is further categorized by vaccine type (Pfizer, Moderna, etc.).###
###    5    Merging Data:
####            ◦    State hospitalization and vaccination data are merged by state name to create a combined dataset.
####            ◦    Vaccination rates are calculated by dividing the cumulative vaccination count by the population over 10 years old (obtained from Census data).
###    6    Visualization - Combined Graph:
####            ◦    A combinedggplot2 plot is created to visualize:
                    ▪    Cumulative vaccination rate per 1000 people (y-axis) for each state.
                    ▪    Daily hospitalization rate per 1000 people (right y-axis) for each state.
                    ▪    Line colors are used to differentiate between vaccination types and hospitalization rates.
####            ◦    The x-axis displays states sorted by their overall vaccination rate.
    7    Correlation Analysis (Optional):
####            ◦    A subset of the data is created to focus on dates with corresponding vaccination data.
####            ◦    Non-cumulative daily vaccinations are calculated.
####            ◦    Pearson's correlation coefficient is computed to assess the relationship between non-cumulative vaccinations and hospitalization rates.
####            ◦    A scatter plot is generated to visualize the correlation.
###    8    Vaccinated vs. Unvaccinated Hospitalization Rates:
####            ◦    Data on hospitalization rates by vaccination status is loaded from the CDC.
####            ◦    Monthly hospitalization rates for vaccinated and unvaccinated individuals are calculated.
####            ◦    A line chart is created to compare these rates over time.


```
##R packages required for to run the script

## Check for installed packages
pkgs <- c("dplyr", "tidyr", "ggplot2", "data.table", "cowplot", "zoo", 
          "olsrr", "utils", "nnet")
installed.pkgs <- pkgs %in% rownames(installed.packages())

## Install missing packages
if (length(installed.pkgs[installed.pkgs == FALSE]) > 0) {
  message("Installing missing packages...")
  install.packages(pkgs[!installed.pkgs])
  message("Packages installed successfully!")
} else {
  message("All required packages are already installed.")
}

```
