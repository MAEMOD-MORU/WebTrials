# WebTrials <img src="man/figures/WebTrials.png" align="right" width="120" />

<!-- badges: start -->
[![R](https://img.shields.io/badge/R-%23276DC3.svg?logo=r&logoColor=white)](#)
<!-- badges: end -->
### Installation
* Install latest development version from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MAEMOD-MORU/WebTrials", dependencies = TRUE, build_vignettes = FALSE)
```
# Description: 
WebTrials is an R package that helps you easily create Shiny web apps for randomizing participants in clinical trials. 
#### Key Features:
*	Pre-built Templates: Creating of pre-built Shiny app templates.
*	User Authentication and Security: Supports user authentication and role-based access control to ensure that only authorized personnel can access (shinymanager)
*	Deployment Flexibility: Facilitates easy deployment of the Shiny apps either locally, on a server, or via cloud services.
#### Use Cases:
*	Clinical Trial Coordination: Helps trial coordinators and statisticians set up and manage randomization processes with minimal programming knowledge.
* Regulatory Compliance: Ensures that randomization procedures meet the necessary regulatory requirements through secure data handling and detailed audit trails.


