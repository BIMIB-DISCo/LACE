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
#' @rawNamespace import(curl, except = c("parse_date"))
#' @import sortable
#' @rawNamespace import(shiny, except = c("runExample", "dataTableOutput", "renderDataTable","validate"))
#' @import shinythemes
#' @import dplyr
#' @import forcats
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
#' @import callr
#' @import shinyvalidate
#' @import logr

## Declare name

utils::globalVariables(".my_actual_wd",
                       package = "LACE",
                       add = FALSE)


#' @md
#' @title LACE Processing and Analysis Interface
#' @description `LACEview` displays a Shiny user interface to
#'     handle the VCF and BAM files processing that is needed to
#'     construct the input for the LACE inference algorithms.
#'     The function generates also the maximum likelihood longitudinal 
#'     clonal tree, and shows the output for further explorations of
#'     the results.
#'
#' @section Installation: 
#' The package is available on GitHub and Bioconductor.
#' LACE 2.0 requires R > 4.1.0 and Bioconductor.
#'
#' To install directly from github run:
#'
#' ```
#' remotes::install_github("https://github.com/BIMIB-DISCo/LACE", 
#'                         dependencies = TRUE)
#' ```
#'
#' @section Dependencies:
#' LACE 2.0 uses *Annovar* and *Samtools suite* as back-ends for variant calling
#' annotation and depth computation, respectively.
#' 
#' *Annovar* is a variant calling software written in *Perl* freely available upon 
#' registration to their website at 
#' [https://annovar.openbioinformatics.org/en/latest/](https://annovar.openbioinformatics.org/en/latest/).
#' 
#' *Perl* can be found and installed at [https://www.perl.org/](https://www.perl.org/). 
#' 
#' *Samtools suite* is a set of tools to handle SAM/BAM/BED file format. It is 
#' freely available at [http://www.htslib.org/](http://www.htslib.org/). To install *Samtools* follow the 
#' instructions in their website. 
#'  

#' 
#' @usage 
#' LACEview()
#' 
#' @return The GUI
#' 
#' @note 
#' The function `LACE` is still available for retrocompatibility.
#' 
#' @import logr
#' 
#' @export
LACEview <- function() {
    appDir <- system.file("shinyapp", package = "LACE")
    if (appDir == "") {
        stop("LACE: Could not find package directory.",
             "Try re-installing `LACE`.",
             call. = FALSE)
    }

    ## Did you remember to set:  options(browser = 'firefox')  ?
    
    .GlobalEnv$.my_actual_wd <- getwd()
    .GlobalEnv$.my_pkg_dir <- appDir
    .GlobalEnv$.my_tmp_file <- file.path(tempdir(), "test.log")
    .GlobalEnv$.my_lf <- logr::log_open(.GlobalEnv$.my_tmp_file)
    
    msg <- ""
    
    print(paste("[info] LACE: active folder =", .GlobalEnv$.my_actual_wd))
    print(paste("[info] LACE: log file =", .GlobalEnv$.my_tmp_file))
    
    answer <- "y"
    
    
    server_pp <- function(input, output) {    
        exec_bool <- reactiveVal(TRUE)
        myOS <- .Platform$OS.type
        
        myAnnovar_script1 <- "convert2annovar.pl"
        myAnnovar_script2 <- "annotate_variation.pl"
        
        if (myOS == "windows") {
            mySamtools <- "samtools.exe"
            myPerl <- "perl.exe"
        }
        else {
            mySamtools <- "samtools"
            myPerl <- "perl"
        }
        
        myAnnovar_script1_path <- Sys.which(myAnnovar_script1)
        myAnnovar_script2_path <- Sys.which(myAnnovar_script2)
        samtools_path <- Sys.which(mySamtools)
        perl_path <- Sys.which(myPerl)
        
        if (str_length(samtools_path) == 0) {
            exec_bool(FALSE)
            msg <- paste(msg, paste("[warning] LACE:", mySamtools, "not found."), sep = "\n")
            msg <- paste(msg, paste("[info] LACE:", mySamtools, "can be set using the UI."), sep = "\n")
        }
        
        if (str_length(perl_path) == 0) {
            exec_bool(FALSE)
            msg <- paste(msg, paste("[warning] LACE:", myPerl, "not found."), sep = "\n")
        }	
        
        if (str_length(myAnnovar_script1_path) == 0) {
            #exec_bool <- FALSE
            msg <- paste(msg, paste("[warning] LACE:", myAnnovar_script1, "not found."), sep = "\n")
            msg <- paste(msg, paste("[info] LACE:", myAnnovar_script1, "folder can be set using the UI."), sep = "\n")
        }
        
        if (str_length(myAnnovar_script2_path) == 0) {
            #exec_bool <- FALSE 
            msg <- paste(msg, paste("[warning] LACE:", myAnnovar_script2, "not found."), sep = "\n")
            msg <- paste(msg, paste("[info] LACE:", myAnnovar_script2, "folder can be set using the UI."), sep = "\n")
        }
        
        if (myOS == "windows") {
            exit_status <- system2("assoc", args = ".pl", 
                                   stdout = TRUE,
                                   stderr = TRUE,
                                   wait = TRUE)
            if (str_starts(exit_status, ".pl")) {
                exec_bool(FALSE)
                msg <- paste(msg, "[warning] LACE:",".pl file extension is not associated to perl.", sep = "\n")
            }
        }
        
        #answer <- "y"
        #answer <- 1
        #answer <- TRUE
        
        #if (!exec_bool) {
        #    print("Required software is not installed or cannot be found.")
        #    answer <- readline(prompt = "Do you wish to continue and start LACE UI? [Y,n]")
        #    #answer <- menu(c("Yes", "No"), title="Do you wish to continue and start LACE UI?")
        #}
        
        
        cat(msg)
        
        ml_txt <- str_replace_all(
          paste(msg, "\n", 
                "Required software is not installed or cannot be found.",
                "Do you wish to continue and start LACE UI?",
                sep = "\n"), 
          pattern = "\n", replacement = "<br>")
        
        ml_txt <- helpText(HTML(ml_txt))
          
        mod_dlg <- modalDialog(
          title = "LACE 2.0",
          ml_txt,
          easyClose = FALSE,
          footer = tagList(
            actionButton("No", "No"),
            actionButton("Yes", "Yes")
          ))
        
        showModal(mod_dlg)
        
        observeEvent(input$No, {
            answer <<- "n"
            stopApp()
        })
        
        observeEvent(input$Yes, {
            answer <<- "y"
            stopApp()
        })
    }        
        
    ui_pp <- basicPage(
        #print("test"),
        #actionButton("show", "Show modal dialog")
    )
    
    
    shiny::runApp(list(ui = ui_pp, server = server_pp))
    
    
    if (!(answer %in% c("y", "Y", "yes", "Yes", "ni", ""))) {
        return(NULL)
    }
    
    
    on.exit({
        setwd(.my_actual_wd)
        rm(list = c(".my_actual_wd"), pos = .GlobalEnv)
        logr::log_close()
        print("LACEview closed")
    })

    shiny::runApp(file.path(appDir, 'app.R'), display.mode = "normal", launch.browser = TRUE)
}


### end of file -- LACEinterface.R
