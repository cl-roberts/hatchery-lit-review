# Hatchery literature review app

This repository contains source code for an app displaying an article database 
pertaining to a fish hatchery literature review. See the app in action 
[here](https://clroberts2112.shinyapps.io/hatchery-lit-review/)


### Reproducibility

The shiny app was written in R 4.4.1 using the following packages:

- shiny 1.8.11
- bslib 0.7.0
- DT 0.33
- purrr 1.0.2
- stringr 1.5.1
- dplyr 1.1.4
- rvest 1.0.4
- xml2 1.3.6
- renv 1.0.7

This repository uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) 
for package management. To replicate the library with which this app was made, 
clone the repo and install the project library using `renv::restore()`.
