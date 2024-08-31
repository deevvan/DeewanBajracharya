<img width="779" alt="image" src="https://github.com/user-attachments/assets/4898a819-b640-4730-92fd-21e081e2bb57">


# Observations of COVID-19 vaccine coverage and vaccine hesitancy on COVID-19 outbreak: An American ecological study 

## Read more about manuscript here:
> [__Bajracharya D, Jansen RJ.__ Observations of COVID-19 vaccine coverage and vaccine hesitancy on COVID-19 outbreak: An American ecological study. *Vaccine*, 2024.](https://pubmed.ncbi.nlm.nih.gov/38103963/)

### This README file describes the R script for analyzing vaccine hesitancy and hospitalization rates across US states. The script utilizes various R packages for data manipulation, visualization, and statistical analysis.


## Data Sources:
> [CDC hospitalization data](https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD/)

> [COVID Tracking Project data](https://covidtracking.com/data/download/all-states-history.csv)

> [CDC vaccination data](https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD)

> [CDC vaccination rates by vaccination status](https://data.cdc.gov/api/views/k3na-u7xf/rows.csv?accessType=DOWNLOAD)

> US Census data: csv files provided in script

> CDC cases and deaths data: csv file provided in script

> US vaccination hesitancy data: csv files provided in script

> National available hospital bed data: csv files provided in script

> National VOC estimation data from covSPECTRUM: csv files provided in script


## Script Steps:
### 1. Load Libraries 

### 2. Load Data: 
Several datasets listed above must be loaded from various sources using read.csv function. 
      
### 3. State Hospitalization Data:
◦ Daily hospitalization data is calculated by aggregating hospital admissions for each state across dates.

◦ Data for territories (marked by specific codes) is excluded.
      
### 4. State Vaccination Data:
◦ Daily vaccination data is calculated by summing vaccinations administered for each state across dates.

◦ Data for territories is excluded.

◦ Vaccination data is further categorized by vaccine type (Pfizer, Moderna, Jansen and Novavax).

◦ Vaccination rates are calculated by dividing the cumulative vaccination count by the population over 10 years old 
      
### 5. State Cases and Deaths Data:
◦ Daily new cases and deaths reported for each state.

◦ Data for territories is excluded.

◦ Cases and Deaths related data were further separated and accounted into rates and cumulative rates.
      
### 6. Merging Data:
State hospitalization, case, death, and vaccination data are merged by state name to create a combined dataset.
            
### 7. Correlation Analysis (Optional):
◦ A subset of the data is created to focus on dates with corresponding vaccination data.

◦ Non-cumulative daily vaccinations are calculated.

◦ Pearson's correlation coefficient is computed to assess the relationship between non-cumulative vaccinations and hospitalization rates.

◦ A scatter plot is generated to visualize the correlation.
      
### 8. Vaccinated vs. Unvaccinated Hospitalization Rates:
◦ Data on hospitalization rates by vaccination status is loaded from the CDC.

◦ Monthly hospitalization rates for vaccinated and unvaccinated individuals are calculated.

◦ A line chart is created to compare these rates over time.

### 9. Visualization - Combined Graph:
◦ A combined ggplot2 plot is created to visualize:

   a) State-wise Cumulative hospitalizations per 1000 VS lab confirmed total COVID19 cases and pre Omicron cases per 1000 respectively.
  
   b) State-wise Cumulative hospitalizations per 1000 VS lab confirmed total COVID19 deaths and pre Delta deaths per 1000 respectively.
  
   c) Cumulative national vaccinations per 1000 VS daily hospital admissions with lab confirmed COVID19 hospitalizations.

   d) Vaccinated Vs Unvaccinated hospitalized populations

   e) National daily VOC estimates for Alpha, Delta and Omicron variants for the entire 3 year period of data collection.

   f) Regional distribution of daily disease outcomes in cases, hospitalizations and deaths compared to regional cumulative vaccinations per 1000.
  
```
## R packages required for to run the script

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
