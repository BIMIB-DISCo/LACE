### LACEinterface.R
###
### The LACE GUI to manage longitudinal NGS datasets.
###
### See the file LICENSE in the main folder for licensing
### information.

#' @rawNamespace import(biomaRt, except = c("show","select"))
#'
#' @import foreach
#' @import doParallel
#' @import sortable
#' @rawNamespace import(shiny, except = c("runExample", "dataTableOutput", "renderDataTable","validate"))
#' @import shinythemes
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import shinyFiles
#' @import shinyjs
#' @import jsonlite
#' @import configr
#' @import DT
#' @import stringr
#' @import tools
#' @import fs
#' @rawNamespace import(data.table, except = c("first","last", "between"))
#' @import htmltools
#' @import shinyBS
#' @import bsplus
#' @import shinydashboard
#' @import TRONCO
#' @import callr

## Declare name

utils::globalVariables(".my_actual_wd",
                       package = "LACE",
                       add = F)


#' @md
#' @title LACE Preprocessing and Analysis Interface
#' @description `LACE2` displays a RStudio Shiny user interface to
#'     handle the VCF and BAM files processing that is needed to
#'     construct the input for the LACE inference algorithms.
#'     The function generates also the most likelihood longitudinal 
#'     clonal tree, and shows the output for further explorations of
#'     the results.
#'
#' @usage LACE2()
#' @section Installation: To install the package with all the
#'     dependecies, devtools and Bioconductor need to be present.
#'
#' The R version required is greater than 4.1.0.
#'
#' To install directly from github run:
#'
#' ```
#' remotes::install_github("https://github.com/BIMIB-DISCo/LACE2",  dependencies = T)
#' ```
#'
#' @section Dependencies:
#' The genetic variant annotation software ANNOVAR
#' (https://annovar.openbioinformatics.org/en/latest/) and the suite
#' of programs for interacting with high-throughput sequencing data
#' Samtools (http://www.htslib.org/) need to be installed.
#'
#' @note 
#' The function `LACE` is still available for retrocompatibility.
#' 
#' @export
LACE2 <- function() {
    appDir <- system.file("shinyapp", package = "LACE")
    if (appDir == "") {
        stop("LACE: Could not find package directory.",
             "Try re-installing `LACE`.",
             call. = FALSE)
    }

    ## Did you remember to set:  options(browser = 'firefox')  ?

    .GlobalEnv$.my_actual_wd <- getwd()
    .GlobalEnv$.my_pkg_dir <- appDir
    print(.GlobalEnv$.my_actual_wd)

    on.exit({
        setwd(.my_actual_wd)
        rm(list = c(".my_actual_wd"), pos = .GlobalEnv)
    })

    shiny::runApp(file.path(appDir, 'app.R'), display.mode = "normal")
}


### end of file -- LACEinterface.R
