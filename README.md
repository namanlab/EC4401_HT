# Data Market Design: Equilibrium Analysis and Mechanism Design

This repository contains the code and results for the study on data market design, focusing on equilibrium analysis in asymmetric auctions and mechanism design for efficient allocation. The project explores how strategic interactions, information asymmetry, and auction mechanisms shape market outcomes, providing theoretical insights and practical applications.

## Repository Structure
```
.
├── LICENSE                # License information
├── codes_MD.R             # R script for Mechanism Design analysis
├── codes_eqm_analysis.R   # R script for Equilibrium Analysis
└── results                # Folder containing output visualizations
    ├── EA                 # Results for Equilibrium Analysis
    │   ├── pooling_1.png
    │   ├── pooling_2.png
    │   ├── pooling_3.png
    │   ├── sep_1.png
    │   ├── sep_2.png
    │   ├── sep_A.png
    │   └── sep_B.png
    └── MD                 # Results for Mechanism Design
        ├── MD_optimal_alpha.png
        └── MD_optimal_p2.png
```

## Running the Code
To run the analyses, ensure you have R installed along with necessary dependencies.

1. **Equilibrium Analysis (EA)**
   - Run `codes_eqm_analysis.R` to analyze strategic interactions and bidding behavior in asymmetric data auctions.
   - Output visualizations are saved in the `results/EA/` directory.

2. **Mechanism Design (MD)**
   - Run `codes_MD.R` to evaluate auction mechanisms optimizing allocation and incentives.
   - Results are stored in `results/MD/`.

## Distinction Between MD and Equilibrium Analysis
- **Equilibrium Analysis (`codes_eqm_analysis.R`)** focuses on strategic bidding behavior and market efficiency under asymmetric conditions.
- **Mechanism Design (`codes_MD.R`)** explores auction mechanisms that improve allocation efficiency and revenue while ensuring incentive compatibility.

For further details on methodology and findings, refer to the full project documentation.

## Installation Guide

To run this project, you’ll need to install several R packages listed in `requirements.txt`. Follow the steps below to set up the required packages:

**Prerequisites**

Ensure you have R installed. You can download R from CRAN.

Installing Packages
1. Save the Package List: The packages required for this project are listed in requirements.txt.  
2. Install Packages: Use the following code snippet in R to install all the packages listed in requirements.txt.

```r
packages <- readLines("requirements.txt")
install.packages(packages)
```


This code will read the list of packages from `requirements.txt` and install any missing packages in your R environment.

**Verifying Installation**

After running the above commands, load the packages in your R session to confirm they are installed correctly:

```r
# Load packages
library(MASS)
library(tidyverse)
library(latex2exp)
library(truncnorm)
```

If there are no errors, you are ready to proceed with the project!

## Acknowledgments

This project was developed as part of the EC4401 Final Year Project.  I would like to express my gratitude to my supervisor, Dr. Yingkai Li, for his invaluable guidance, support, and mentorship throughout this project. I am also grateful to the faculty and staff at NUS for their continuous support, and to my family for their encouragement along the way.


## Contact

For any questions or inquiries, please contact me at `naman.agr03@gmail.com`.


