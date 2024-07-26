# Hatchery literature review app

This repository contains source code for an app displaying an article database 
pertaining to a fish hatchery literature review. See the app in action 
[here](https://clroberts2112.shinyapps.io/hatchery-lit-review/)


### Reproducibility

The shiny app was written in R 4.2.0 using the following packages:

- shiny 1.8.11
- bslib 0.7.0
- DT 0.23
- rlang 1.1.4
- purrr 1.0.1
- stringr 1.5.0
- dplyr 1.1.0
- rvest 1.0.3
- xml2 1.3.3
- renv 1.0.3

This repository uses `renv` for package management. To replicate the library with
which this app was made, clone the repo and install the project library using
`renv::restore()`.
