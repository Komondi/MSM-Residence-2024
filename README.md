# MSM-Residence-2024

## Overview

This repository hosts the R scripts and associated files for the Multi-State-Model (MSM) project at the African Population and Health Research Center (APHRC). The project focuses on using MSM techniques to analyze residence demographic events in Nairobi urban slums, with an emphasis on key events such as:

- Births
- Deaths
- Migration (in-migration and out-migration)
- Changes in residence status (exit and entry)

## Project goals

The primary aim is to understand and model the demographic transitions in Nairobi's informal settlements, identifying factors that influence residence changes over time. By applying MSM, we gain insights into the dynamic nature of populations in these slums, which is crucial for policy and intervention planning.

## Repository structure

- **/code**: Contains R scripts for data cleaning, MSM modeling, and results visualization.
- **/results**: Contains output files, including graphs and statistical summaries.
- **/docs**: Documentation related to the project, including this README and other project notes.

## Getting Started

### Prerequisites

Ensure that you have the following software installed:

- R (version 4.0 or later)
- RStudio (optional, for an IDE)
- Required R packages (listed below)

### Installation

1. Clone the repository:

    ```bash
    git clone https://github.com/Komondi/MSM-Residence-2024.git
    cd msm-aphc
    ```

2. Install required R packages:

    ```R
    install.packages(c("msm", "dplyr", "ggplot2", "survival"))
    ```

### Running the analysis

1. Load the required datasets.
2. Run the scripts in the `/code` folder sequentially to perform the analysis.


### Example workflow

1. **Descriptive data analysis:** `code/Event_Descriptive.R`
2. **Female Model Fitting:** `code/MSM_female.R`
3. **Male model fitting:** `code/MSM_male.R`


## Contact

For more information or collaboration inquiries, please contact:

- African Population and Health Research Center (APHRC)
- Project Lead: Dr. Evans Omondi
- Email: [eomondi@aphrc.org]
