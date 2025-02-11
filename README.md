# Cancer Services Dashboard

**Contributors:** Amelia Lee, Matthew Retford, Zach Brubert, Matthew Yu, Catherine Lam, Sydney Smith

The Cancer Services Dashboard provides summary visualisation for cancer services across NHS Trusts. A published version of the dashboard can be found [here](https://lshtm-rsc-data-challenge.shinyapps.io/Dashboard/#quarterly-summary)

## Key files and folder structure

-   data_cleaning_preprocessing.R

-   data_analysis_plots.R

-   Dashboard.qmd

-   data/

    -   raw_data/

        -   cancerdata_combined.xlsx

        -   cancer data dictionary.pdf

        -   data_quality_targets.csv

        -   performance_indicators_metric_type.csv

    -   processed_data/

        -   indicators_trust.csv

        -   data_quality.csv

        -   data_plot_1.csv

        -   data_plot_2.csv

        -   data_plot_3.csv

-   Exploratory data analysis/

## Access the Dashboard 

A published version of the dashboard can be found [here](https://lshtm-rsc-data-challenge.shinyapps.io/Dashboard/#quarterly-summary)

## Steps to generate the Dashboard locally

1.  **Pull/clone the GIT repository:** The latest repository can be found at <https://github.com/sydneyjo-smith/RCS_Data_Challenge/tree/assessment_project>
2.  **Install R and RStudio:** You can download and install R from CRAN - <https://cran.r-project.org/>. After installing R, get RStudio from RStudio's official website - <https://posit.co/download/rstudio-desktop/>. It is recommended you have R version 4.4.2 or greater
3.  **Open the RStudio Project:** Open RStudio and navigate to File -\> Open Project. Find and select the *RCS_Data_Challenge.Rproj* file
4.  [**Set working directory:** ](https://github.com/sydneyjo-smith/RCS_Data_Challenge/tree/assessment_project)In RStudio select Session -\> Set Working Directory -\> To Project Directory
5.  **Prepare data:** Open and run the following R scripts files. Note: this step is only required if there are updates to any of the raw data files, skip to step 6 if this is not the case.
    1.  **Run *data_cleaning_preprocessing.R*:** the script reads in the raw data, cleans and prepares the trust_indicators and data_quality datasets
    2.  **Run *data_analysis_plots.R*:** this script reads in the trust_indicators and data_quality datasets and prepares the datasets required for the dashboard visualisations *data_plot_1.csv, data_plot_2.csv, data_plot_3.csv*
6.  Generate dashboard: Open the Dashboard.qmd file and select "Render" in the top toolbar. This should generate the dashboard. It is best to select open in browser after the dashboard window shows.

## How to use the Dashboard

Details of how to use can be found on the [Dashboard Instructions](https://lshtm-rsc-data-challenge.shinyapps.io/Dashboard/#dashboard-instructions) page.
