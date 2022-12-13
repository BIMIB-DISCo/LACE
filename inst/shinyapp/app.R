### app.R --
###
### Main LACEinterface file.
###
### See file LICENSE in the main folder for licensing and copyright
### information.


### reactlogShow(time = FALSE)

### Libraries needed by the app.

suppressMessages(library(biomaRt, exclude = c("show", "select")))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(sortable))
suppressMessages(library(shiny,
        exclude = c("runExample",
                    "dataTableOutput",
                    "renderDataTable",
                    "validate",
                    "show")))
suppressMessages(library(shinythemes))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
suppressMessages(library(shinyFiles))
suppressMessages(library(shinyjs, exclude = c("show")))
## library(shinyBS)
suppressMessages(library(jsonlite))
suppressMessages(library(configr))
## library(readr)
suppressMessages(library(DT))
suppressMessages(library(stringr))
suppressMessages(library(tools))
suppressMessages(library(fs))
suppressMessages(library(data.table, exclude = c("first", "last", "between")))
suppressMessages(library(htmltools))
suppressMessages(library(shinyBS))
suppressMessages(library(bsplus))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyvalidate))
suppressMessages(library(logr))
#library(widgetframe)

### Code
### ====

### getWidgetType --

getWidgetType <- function(widgetId) {
  paste0("elem = document.getElementById('", widgetId, "');
            var message;
            if (elem == null) {
              message = 'None'
            } else {
              // RStudio Viewer + IE workaround (dont have .include())
              if (elem.getAttribute('class').indexOf('js-range-slider') > -1) {
                message = 'slider'
              } else if (elem.getAttribute('class').indexOf('shinyFiles') > -1) {
                message = 'shinyFiles'
              } else if (elem.getAttribute('class').indexOf('shinyDirectories') > -1) {
                message = 'shinyDirectories'
              } else if (elem.nodeName == 'SELECT') {
                message = 'select'
              } else if (elem.getAttribute('class').indexOf('shiny-text-output') > -1) {
                message = 'shiny-text-output'
              } else if (elem.getAttribute('class').indexOf('selectized') > -1) {
                message = 'selectized'
              } else {
                message = elem.getAttribute('type');
              }
            }
          Shiny.onInputChange('inputType_", widgetId,"', message)
         "
  )
}


### jsCode --

jsCode <- "shinyBS.addTooltip = function(id, type, opts) {
  var $id = shinyBS.getTooltipTarget(id);
  var dopts = {html: true};
  opts = $.extend(opts, dopts);

  if (type == 'tooltip') {
    $id.tooltip('destroy');
    setTimeout(function() { $id.tooltip(opts); }, 200);
  } else if(type == 'popover') {
    $id.popover('destroy');
    setTimeout(function() { $id.popover(opts); }, 200);
  }
}"

log2_print <- function(x, msg = "") {
  if (str_length(msg)>0)
    log_print(msg, console = FALSE, blank_after = FALSE, hide_notes =TRUE)
  log_print(x, console = FALSE, blank_after = FALSE, hide_notes =FALSE)
}
#log2_print <- function(x, msg = "") {}

### compare_named_lists --
###
### Notes:
### MA: Seems a bit complicated.  What is this supposed to do?

compare_named_lists <- function(l1, l2) {
  if (is.list(l1) && is.list(l2)) { # if they are lists
    if (length(unique(names(l1))) == length(l1) &&
        length(unique(names(l2))) == length(l2) &&
        !("" %in% names(l1)) &&
        !("" %in% names(l2))
    ) { # if they are named
      st <- TRUE
      for (i in unique(c(names(l1), names(l2)))) {

        ## If the same name is present in both..
        ##
        ## Notes:
        ## MA: are we sure there is no 'intersection' operation on
        ## lists in R?

        if (i %in% names(l1) && i %in% names(l2)) {
          if (is.list(l1[[i]]))
            st <- st && compare_named_lists(l1[[i]], l2[[i]])
          else
            st <- st && identical(l1[[i]], l2[[i]])
        }
        else
          return(FALSE)
      }
      return(st)
    }
    return(FALSE)
  }
  else
    return(FALSE)
}


### Help Management
### ===============

source("info_help.R")

### make_shiny_help_popover --

make_shiny_help_popover <- function(for_object,
                                    pop_title,
                                    pop_text,
                                    with_icon = shiny_iconlink(),
                                    server = FALSE) {
  with_icon <- with_icon %>%
    bs_embed_popover(title = pop_title,
                     content = pop_text,
                     placement = "right",
                     trigger = "hover",
                     delay = list("show" = 200, "hide" = 3000),
                     html = TRUE
    )

  for_object <- for_object %>% shinyInput_label_embed(with_icon)
  if (server)
    for_object <-
    tagList(for_object,
            tags$script("$(function () {$('[data-toggle=\"popover\"]').popover()})")
    )
  return(for_object)
}


### make_title_help_popover

make_title_help_popover <- function(for_object,
                                    near_label,
                                    pop_title,
                                    pop_text,
                                    with_icon = shiny_iconlink(),
                                    server = FALSE) {
  with_icon <- with_icon %>%
    bs_embed_popover(title = pop_title,
                     content = pop_text,
                     placement = "right",
                     trigger = "hover",
                     delay= list("show"= 200, "hide"= 3000),
                     html = TRUE
    )
  with_icon <- htmltools::div(class = "pull-right", with_icon)

  tag <- htmltools::div(class = "form-group shiny-input-container",
                        near_label,
                        br(),
                        for_object)

  tag$children[[1]] <-
    tag$children[[1]] %>%
    htmltools::tagAppendChild(with_icon) %>%
    htmltools::tagAppendAttributes(style = "width:100%;")

  if (server)
    tag <-
    tagList(tag,
            tags$script("$(function () {$('[data-toggle=\"popover\"]').popover()})")
    )
  return(tag)
}


### inline --

inline = function (x) {
  tags$div(style="display:inline-block;", x)
}


### ui -- ####

ui <- fluidPage(
  ## singleton(tags$head(tags$script(src = "bs.js"))),

  useShinyjs(),
  withMathJax(),


  ## extendShinyjs(text = jsCode, functions = c("addTooltip")),
  ## extendShinyjs(text = jsCode, functions = c("bsPopover")),

  

  tags$head(tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
            tags$style(HTML(".bucket-list-container {min-height: 290px; max-height: 300px;}")),
            tags$style(HTML(".rank-list {min-height: 100px; max-height: 240px; overflow-y: scroll;}")),
            #tags$style(HTML(".shinyDirectories, .shinyFiles { width: 100%; direction: rtl; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;}")),
            tags$style(HTML(".shinyDirectories, .shinyFiles { width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;}")),
            tags$style(type="text/css", "body {padding-bottom: 70px;}"),
            tags$style(HTML(".content-wrapper, .right-side {background-color: #ffffff;}"))
  ),

  use_bs_popover(),
  
  dashboardPage(
    dashboardHeader(
      title = "LACE 2.0",
      tags$li(class = "dropdown",
              tags$a(uiOutput("pr_title_ui_h")
              )
      )
    ),

    dashboardSidebar(

      sidebarMenu( id = "sidemenu",
        hidden(menuItem("hidden_menu31", tabName = "hidden_menu31")),
        menuItem(
          "Application",
          tabName = "applicaion",
          icon = icon("braille"), #icon("window-maximize"),
          hidden(menuItem("hidden_menu3", tabName = "hidden_menu3")),
          menuSubItem("New Project", tabName = "new_proj", icon = icon("layer-group") ),
          menuSubItem("Quit", tabName = "quit_app", icon = icon("power-off") )#,
          #actionButton("quit_app", "Quit")
        ),
        hidden(menuItem("hidden_menu", tabName = "hidden_menu")),
        menuItem(
          "Projects",
          tabName = "Projects",
          icon = icon("cubes"),
          #expandedName = "ttt",
          menuItemOutput("recent_projects"),
          hidden(menuItem("hidden_menu2", tabName = "hidden_menu2")),
          # menuItem(
          #   "Recent Projects",
          #   tabName = "Recent Projects",
          #   icon = icon("th"),
          #   #expandedName = "projects",
          #   menuSubItem(
          #     "test22",
          #     tabName = ",media,disco,work_folder,Downloads,gitlace,LACEinterface,test2",
          #     #icon = icon("th"),
          #     #inline(actionButton("save_tab", "save")),
          #     #inline(actionButton("load_tab", "load"))
          #   ),
          #   menuSubItem(
          #     "test2",
          #     tabName = ",media,disco,work_folder,Downloads,gitlace,LACEinterface,test2",
          #     #icon = icon("th"),
          #     #inline(actionButton("save_tab", "save")),
          #     #inline(actionButton("load_tab", "load"))
          #   )#,
          #   #hidden(menuItem("hidden_menu4", tabName = "hidden_menu4"))
          # ),
          menuItem(
            "Demo Projects",
            tabName = "Demo Projects",
            icon = icon("database"),
            #expandedName = "demos",
            menuSubItem(
              "Melanoma dataset (Rambow)",
              tabName = "Melanoma dataset",
              #icon = icon("th"),
              #inline(actionButton("save_tab", "save")),
              #inline(actionButton("load_tab", "load"))
            ),
            menuSubItem(
              "Small dataset",
              tabName = "Small dataset",
              #icon = icon("th")#,
              #inline(actionButton("save_tab", "save")),
              #inline(actionButton("load_tab", "load"))
            )#,
            #hidden(menuItem("hidden_menu3", tabName = "hidden_menu3"))
          )#,
          #hidden(menuItem("hidden_menu1", tabName = "hidden_menu1"))
        ),
        menuItem(
          "Current Project",
          tabName = "dashboard",
          icon = icon("cube"),
          hidden(menuItem("hidden_menu2", tabName = "hidden_menu2")),
          #menuSubItem("Close Project", tabName = "close_proj", icon = icon("times-circle") ),
          #actionButton("close_proj", "Close Project"),
          menuItem(
            "Save and Load Current Tab",
            tabName = "dashboard_save",
            icon = icon("th"),
            menuSubItem("Save", tabName = "save_tab", icon = icon("download") ),
            menuSubItem("Load", tabName = "load_tab", icon = icon("upload") )#,
            #inline(actionButton("save_tab", "Save")),
            #inline(actionButton("load_tab", "Load"))
          )
        )#,
        #,
          # menuItem(
          #   "Save and Load All Tabs",
          #   tabName = "dashboard",
          #   icon = icon("th"),
          #   inline(actionButton("save_tab", "save")),
          #   inline(actionButton("load_tab", "load"))
          # ),
          # menuItem(
          #   "Reset Tabs",
          #   tabName = "dashboard",
          #   icon = icon("th"),
          #   inline(actionButton("save_tab", "Clean and restore "))
          # )
        
      ),
      collapsed = FALSE
    ),

    dashboardBody(
      tabItems(
        tabItem(tabName = "hidden_menu"),
        tabItem(tabName = "Small dataset"),
        tabItem(tabName = "Melanoma dataset"),
        tabItem(tabName = "test22"),
        tabItem(tabName = "test2")
      ),
      div(id = "computation_idCol_div",
          h3("Processing ... please wait")
      ),

      fluidRow(
        column(width = 12,
               tabsetPanel(id = "main_tabset",
                           type = "tabs",

                           ## Project tab
                           tabPanel(
                             "Project",
                             br(),
                             uiOutput("pr_title_ui"),
                             br(),
                             helpText(text[["pr_tab_help"]]),
                             br(),br(),


                             shinyDirButton('pr_folder',
                                            'Select folder',
                                            'Please select the project root folder', FALSE) %>%
                               make_title_help_popover (
                                 tags$b("Project root folder"),
                                 "Project folder",
                                 text[["pr_folder"]]
                               ),
                             br(),

                             textInput("pr_name",
                                       "Name",
                                       placeholder = "Your project name") %>%
                               make_shiny_help_popover(
                                 "Project name",
                                 text[["pr_name"]]
                               ),
                             br(),br(),

                             div(id = "pr_path_div",
                                 uiOutput("pr_path_ui")
                             ),

                             actionButton("pr_next", "Create project"),

                             br(),br(),

                           ),


                           ## Metadata tab
                           tabPanel(
                             "SC metadata",
                             br(),
                             tags$h3("Single cell metadata info"),
                             br(),
                             helpText(text[["m_tab_help"]]),
                             br(), br(),
                             shinyFilesButton('sc_metadata_file',
                                              'Select file',
                                              'Please select the experiment metadata file', FALSE) %>%
                               make_title_help_popover (
                                 tags$b("Metadata file"),
                                 "Metadata info file",
                                 text[["sc_metadata_file"]],
                                 server = TRUE
                               ),
                             br(),
                             br(),
                             div(id = "m_idCol_div",
                                 uiOutput("m_idCol")
                             ),

                             tableOutput("m_id_column_tab"),
                             br(),

                             div(id="m_timePointsCol_div",
                                 uiOutput("m_timePointsCol")
                             ),

                             div(id="m_timePoints_div",
                                 uiOutput("m_timePoints")
                             ),
                             br(), br(),

                           ),

                           ## Annotation tab
                           tabPanel(
                             "Annotations",
                             br(),
                             tags$h3("Gene variant annotations"),
                             br(),
                             helpText(text[["av_tab_help"]]),
                             br(), br(),

                             ## runjs(jsCode),
                             shinyDirButton('av_anovar_exec_dir',
                                            'Select folder',
                                            'Please select Annovar executable folder',
                                            FALSE) %>%
                               make_title_help_popover (
                                 tags$b("Annovar executable folder"),
                                 "Annovar",
                                 text[["av_anovar_exec_dir"]]
                               ),
                             br(),
                             br(),

                             shinyDirButton('av_anovar_db_dir',
                                            'Select folder',
                                            'Please select Annovar reference database folder',
                                            FALSE) %>%
                               make_title_help_popover (
                                 tags$b("Annovar reference database"),
                                 "Annovar reference database",
                                 text[["av_anovar_db_dir"]]
                               ),
                             br(),
                             br(),

                             shinyDirButton('av_vcf_in_dir',
                                            'Select folder',
                                            'Please select VCF folder',
                                            FALSE) %>%
                               make_title_help_popover (
                                 tags$b("Variant calling files folder"),
                                 "Variant calling files folder",
                                 text[["av_vcf_in_dir"]]
                               ),
                             br(),
                             br(),

                             ## tags$b("Output folder"),
                             ## br(),
                             ## shinyDirButton('av_out_dir', 'Select folder', 'Please select output folder', FALSE),
                             br(),
                             br(),

                             tags$b("Run"),
                             br(),
                             actionButton("av_exec",
                                          "Annotate variants"),

                             # tags$span(icon("info-circle"),
                             #           id = "icon_av_exec",
                             #           style = "color: blue;"),
                             # bsPopover("icon_av_exec",
                             #           "Load inputs",
                             #           text[["av_exec"]],
                             #           placement = "right",
                             #           trigger = "hover",
                             #           options = list("delay':  {show : 500, hide : 2000}, 'a" = "show")),
                             br(),br(),
                             ## tags$b("Result"),
                             ## br(),
                             ## verbatimTextOutput("av_anovar_exec_dir"),
                             ## verbatimTextOutput("av_anovar_db_dir"),
                             ## verbatimTextOutput("av_vcf_out_dir")
                           ),

                           ## Filters tab
                           tabPanel(
                             "Filters",
                             br(),
                             tags$h3("Quality filters"),
                             br(),
                             helpText(text[["thr_tab_help"]]),

                             br(), br(),
                             numericInput(
                               inputId = "thr_alleles_ratio",
                               label = "Alternate frequency",
                               min = 0,
                               max = 1,
                               step = 0.05,
                               value = 0.8
                             ) %>%
                               make_shiny_help_popover(
                                 "Cell alternate allele frequency",
                                 text[["thr_alleles_ratio"]]
                               ),

                             numericInput(
                               inputId = "thr_maf",
                               label = "MAF",
                               min = 0.01,
                               max = 0.5,
                               step = 0.05,
                               value = 0.01
                             ) %>%
                               make_shiny_help_popover(
                                 "Minor alleles frequency",
                                 text[["thr_maf"]]
                               ),

                             numericInput(
                               inputId = "thr_freq",
                               label = "Variant frequency",
                               min = 0.,
                               max = 1,
                               step = 0.1,
                               value = 0.5
                             ) %>%
                               make_shiny_help_popover(
                                 "Sample variant frequency",
                                 text[["thr_freq"]]
                               ),
                             br(),

                             div(id = "thr_bucket_var_list_div",
                                 uiOutput("thr_bucket_var_list")
                             ),
                             br(),br(),br(),br(),


                             tags$b("Run"),
                             br(),
                             actionButton("thr_exec", "Apply filters"),
                             br(),br(),
                             ## tags$b("Result"),
                             ## verbatimTextOutput("thr_filters"),
                             ## verbatimTextOutput('thr_accepted_var'),
                             ## verbatimTextOutput('thr_negleted_var')
                           ),

                           ## Depth tab
                           tabPanel(
                             "SC sampling depths",
                             br(),
                             tags$h3("Single cell sampling depth"),
                             br(),
                             helpText(text[["dp_tab_help"]]),
                             br(), br(),

                             shinyDirButton('dp_samtools_exec_dir',
                                            'Select folder',
                                            'Please select samtools executable folder',
                                            FALSE) %>%
                               make_title_help_popover (
                                 tags$b("Samtools executable folder"),
                                 "Samtools executable folder",
                                 text[["dp_samtools_exec_dir"]]
                               ),
                             br(), br(),

                             shinyDirButton('dp_bam_dir',
                                            'Select folder',
                                            'Please select folder containing the single cell BAM files',
                                            FALSE) %>%
                               make_title_help_popover (
                                 tags$b("BAMs folder"),
                                 "BAMs folder",
                                 text[["dp_bam_dir"]]
                               ),
                             br(), br(),

                             tags$b("Output folder"),
                             br(), br(),

                             tags$b("Run"),
                             br(),
                             actionButton("dp_exec", "Compute depth"),
                             br(), br(),
                             ## tags$b("Result"),
                             ## br(),
                             ## verbatimTextOutput("dp_par"),
                           ),

                           ## Variants tab
                           tabPanel(
                             "Variants",
                             br(),
                             tags$h3("Variant filters"),
                             br(),
                             helpText(text[["va_tab_help"]]),

                             br(), br(),

                             numericInput(
                               inputId = "va_depth_minimum",
                               label = "Minimum depth",
                               min = 1,
                               max = 10,
                               step = 1,
                               value = 3
                             ) %>%
                               make_shiny_help_popover(
                                 "Minimum depth",
                                 text[["va_depth_minimum"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "va_missing_values_max",
                               label = "Max missing value",
                               min = 0.0,
                               max = 1.0,
                               step = 0.1,
                               value = 0.4
                             ) %>%
                               make_shiny_help_popover(
                                 "Maximumax missing value",
                                 text[["va_missing_values_max"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "va_minumum_median_total", # minimum median depth for total reads
                               label = "Site minumum median depth:",
                               min = 0,
                               max = 50,
                               step = 1,
                               value = 10
                             ) %>%
                               make_shiny_help_popover(
                                 "Minumum median depth per site",
                                 text[["va_minumum_median_total"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "va_minumum_median_mutation", # minimum median depth for reads supporting mutations
                               label = "Mutation minimum median depth",
                               min = 0.0,
                               max = 50,
                               step = 1,
                               value = 4
                             ) %>%
                               make_shiny_help_popover(
                                 "Minimum median depth per mutation",
                                 text[["va_minumum_median_mutation"]]
                               ),
                             br(),

                             selectizeInput(
                               'va_verified_genes',
                               'Select variant genes',
                               NULL,
                               multiple = TRUE,
                               options = list (
                                 placeholder = 'Choose genes for inferential analysis',
                                 hideSelected = FALSE,
                                 plugins =
                                   list("drag_drop",
                                        "remove_button",
                                        "dropdown_header",
                                        "restore_on_backspace"),
                                 dropdown_header =
                                   list(title = 'a')
                               )
                             ) %>%
                               make_shiny_help_popover(
                                 "Variant gene selection",
                                 text[["va_verified_genes"]]
                               ),
                             br(),
                             br(),

                             DTOutput("va_out"),

                             br(), br(),
                             tags$b("Run"),
                             br(),
                             actionButton("va_exec", "Select variants"),
                             br(),
                             br(),
                             ## tags$b("Result"),
                             ## verbatimTextOutput("va_filters")
                           ),


                           ## Inference tab
                           tabPanel(
                             "Inference",
                             br(),
                             tags$h3("Longitudinal tree inference parameters"),
                             br(),
                             helpText(text[["inf_tab_help"]]),

                             br(), br(),
                             numericInput(
                               inputId = "inf_learning_rate",
                               label = "Learning rate",
                               min = 0.01,
                               max = 10,
                               step = 0.1,
                               value = 1
                             ) %>%
                               make_shiny_help_popover(
                                 "Learning rate",
                                 text[["inf_learning_rate"]]
                               ),
                             br(),

                             div(id = "inf_alpha_div",
                                 tagList(
                                   br(),
                                   span() %>%
                                     make_title_help_popover (
                                       tags$b("False positive rates"),
                                       "False positive rate",
                                       text[["inf_alpha"]]
                                     ),
                                   span("Insert a set of false positive rates, one \\(\\alpha\\) for each sampling time point. Fill the last row to add further sets of rates to be used in the inference."),
                                   DTOutput('inf_alpha', width = '100%'),
                                   br()
                                 )
                             ),
                             ## br(),

                             div(id = "inf_beta_div",
                                 tagList(
                                   br(),
                                   span() %>%
                                     make_title_help_popover (
                                       tags$b("False negative rates"),
                                       "False negative rate",
                                       text[["inf_beta"]]
                                     ),
                                   span("Insert a set of false negative rates, one \\(\\beta\\) for each sampling time point. Fill the last row to add further sets of rates to be used in the inference."),
                                   DTOutput('inf_beta', width = '100%'),
                                   br()
                                 )
                             ),
                             br(), br(),

                             numericInput(
                               inputId = "inf_num_iter",
                               label = "Number of iterations",
                               min = 100,
                               max = 4000,
                               step = 100,
                               value = 10000
                             ) %>%
                               make_shiny_help_popover(
                                 "Number of iterations",
                                 text[["inf_num_iter"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "inf_num_rs",
                               label = "Number of restarts",
                               min = 1,
                               max = 100,
                               step = 10,
                               value = 50
                             ) %>%
                               make_shiny_help_popover(
                                 "Number of restarts",
                                 text[["inf_num_rs"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "inf_n_try_bs",
                               label = "Early stopping",
                               min = 100,
                               max = 1000,
                               step = 100,
                               value = 500
                             ) %>%
                               make_shiny_help_popover(
                                 "Early stopping",
                                 text[["inf_n_try_bs"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "inf_num_processes",
                               label = "Number of parallel processes",
                               min = 1,
                               max = 100,
                               step = 5,
                               value = 10
                             ) %>%
                               make_shiny_help_popover(
                                 "Number of parallel processes",
                                 text[["inf_num_processes"]]
                               ),
                             br(),

                             numericInput(
                               inputId = "inf_seed",
                               label = "Seed",
                               min = 0,
                               max = NA,
                               step = 1000,
                               value = 1121
                             ) %>%
                               make_shiny_help_popover(
                                 "Seed",
                                 text[["inf_seed"]]
                               ),
                             br(),


                             checkboxInput("inf_random_tree",
                                           tags$b("Random tree intialization"),
                                           FALSE) %>%
                               make_shiny_help_popover(
                                 "Random tree intialization",
                                 text[["inf_random_tree"]]
                               ),

                             checkboxInput("inf_marginalize",
                                           tags$b("Marginalize"),
                                           FALSE) %>%
                               make_shiny_help_popover(
                                 "Marginalize",
                                 text[["inf_marginalize"]]
                               ),

                             checkboxInput("inf_keep_equivalent",
                                           tags$b("Keep equivalent"),
                                           FALSE) %>%
                               make_shiny_help_popover(
                                 "Keep equivalent",
                                 text[["inf_keep_equivalent"]]
                               ),

                             checkboxInput("inf_check_indistinguishable",
                                           tags$b("Check indistinguishable"),
                                           FALSE) %>%
                               make_shiny_help_popover(
                                 "Check indistinguishable",
                                 text[["inf_check_indistinguishable"]]
                               ),

                             checkboxInput("inf_error_move",
                                           tags$b("Error move"),
                                           FALSE) %>%
                               make_shiny_help_popover(
                                 "Error move",
                                 text[["inf_error_move"]]
                               ),

                             checkboxInput("inf_show",
                                           tags$b("Show results in interface")) %>%
                               make_shiny_help_popover(
                                 "Show results in interface",
                                 text[["inf_show"]]
                               ),

                             br(), br(),br(),
                             tags$b("Run"),
                             br(),
                             actionButton("inf_next", "Run LACE"),
                             br(), br(),
                             tags$b("Result"),
                             verbatimTextOutput("inf_par")
                           ),

                           ## results ####
                           tabPanel(
                             "Longitudinal display",
                             br(),

                             inline(actionLink("res_refresh", "Refresh page")),
                             inline(textOutput("res_lastrefresh")),
                             br(),
                             # #widgetframeOutput('LCT_FP', width = "1000px", height = "800px"),
                             # #LACE:::LACEOutput('LCT_FP'),#, width = 910, height = 800),
                             # {
                             #   #x <-LACE:::LACEOutput('LCT_FP')
                             #   #htmlwidgets::saveWidget(x,"LACE_result.html")
                             #   NULL
                             #   #x
                             # },
                             uiOutput("res"),
                             br(),br(),
                           )
               )
        )
      )
    )
  )
)


### server -- ####

server <- function(input, output, session) {

  setwd(.my_pkg_dir) # per non disperdersi

  inputs <- list()
  types_ <- reactiveVal()

  session$onSessionEnded(function() {
    stopApp()
  })

  ## Source?

  source("check_demo.R", local = TRUE)
  source("load_and_save.R", local = TRUE)
  source("project_mgr.R", local = TRUE)
  source("pipeline_io_ctrl.R")
  source('alpha_beta_table.R', local = TRUE)
  source("annotation.R")
  source("filters_computation.R", local = TRUE)
  source("depth_computation.R", local = TRUE)
  source("step3_explore_lace_input_data.R")
  source("make_lace_final_input.R")



  default_yaml <-
    list(".config_06_inf.yml" = list(),
         ".config_05_va.yml" = list(),
         ".config_04_dp.yml" = list(),
         ".config_03_thr.yml" = list(),
         ".config_02_av.yml" = list(),
         ".config_01_m.yml" = list()
    )

  os_hidden <- "."
  os_conf <- list()
  os_conf[["windows"]] <- "AppData"
  os_conf[["mac"]] <- "Library"
  os_conf[["unix"]] <- ".config"
  os_conf_subdir = "config"
  config_path = os_conf_subdir # to remove


  ## roots_dir=c(root = file.path('..'), "working_dir" = file.path('.'), getVolumes()())
  roots_dir = c("project dir" = file.path("."),
                ".." = file.path(".."),
                "../.." = file.path("..",".."),
                "../../.." = file.path("..","..",".."),
                getVolumes()())

  ## runjs(jsCode)
  ## runjs("console.log(shinyBS.addTooltip)")
  ## js$addTooltip("av_anovar_db_dir", "IL BOTTONE", "")
  ## addPopover(session, "av_anovar_db_dir", "IL BOTTONE2", "e il polpettone", placement = "right")

  observe({
    if (input$close > 0) stopApp()                             # stop shiny
  })

  m_loaded_input_ <- reactiveVal()
  av_loaded_input_ <- reactiveVal()
  thr_loaded_input_ <- reactiveVal()
  dp_loaded_input_ <- reactiveVal()
  va_loaded_input_ <- reactiveVal()
  inf_loaded_input_ <- reactiveVal()


  rs_ <- reactiveVal()
  inputs[["res_lastrefresh"]] <- reactiveVal("")
  stime <- reactiveVal(2)
  port <- reactiveVal()
  sopt <- reactiveVal(callr::r_session_options())


  av_grep_str= "^av_|^`av_|_av_"
  thr_grep_str= "^thr_|^`thr_|_thr_"
  dp_grep_str= "^dp_|^`dp_|_dp_"
  va_grep_str= "^va_|^`va_|_va_"
  inf_grep_str= "^inf_|^`inf_|_inf_"
  m_grep_str= "^m_|^`m_|_m_|sc"

  inf_uis = c("inf_num_iter",
              "inf_num_rs",
              "inf_n_try_bs",
              "inf_num_processes",
              "inf_seed",
              "inf_random_tree",
              "inf_marginalize",
              "inf_keep_equivalent",
              "inf_check_indistinguishable",
              "inf_show",
              "inf_learning_rate",
              "inf_error_move")

  va_uis= c('va_depth_minimum',
            'va_missing_values_max',
            'va_minumum_median_total',
            'va_minumum_median_mutation')
  thr_uis = c('thr_alleles_ratio',
              'thr_maf',
              'thr_freq',
              'thr_accepted_var',
              'thr_negleted_var')
  dp_uis= c('dp_samtools_exec_dir', 'dp_bam_dir')
  av_dir_uis = c('av_anovar_exec_dir',
                 'av_anovar_db_dir',
                 'av_vcf_in_dir')




  source("project_tab.R", local = TRUE)
  source("inference_tab.R", local = TRUE)


  setwd(.my_actual_wd)

  ### Dashboard reactiveVals ####
  inputs[["demo"]] <- reactiveVal()
  inputs[["reload_project"]] <- reactiveVal()
  inputs[["demo_remainder"]] <- reactiveVal()
  inputs[["long_job"]] <- reactiveVal(0)



  ### End dashboard reactiveVals ####

  ### Dashboard observers ####

  shinyjs::hide(id="computation_idCol_div")


  observeEvent(inputs[["reload_project"]](), {

    proj_dir <- inputs[["reload_project"]]()

    if (inputs[["reload_project"]]() != 1 && inputs[["reload_project"]]() != 0)
    {
      if (dir.exists(proj_dir)) {
        hide_tab()
        inputs[["reload_project"]](1)
        inputs[["pr_path"]](proj_dir)
      }
      else
        inputs[["reload_project"]](NULL)
    }
    #else
    #  inputs[["reload_project"]](NULL)
  })

  observeEvent( inputs[["demo"]](),{
    if(inputs[["demo"]]() == "Small_dataset" || inputs[["demo"]]() == "Rambow_dataset")
    {
      #browser()
      if(!check_demo()) {
        inputs[["demo"]](NULL)
        toggle_inputs(is.null(inputs[["demo"]]()))
        showNotification("Impossible to download demos.", duration = 10, type = "warning")
      }
      else
      {
        #browser()
        demo_dir<-file.path(.my_pkg_dir,inputs[["demo"]]())
  
        # create tmp folder
        tmp_dir <- tempdir()
        tmp_path <- file.path(tmp_dir,inputs[["demo"]]())
        dir.create(tmp_path, showWarnings = FALSE)
  
        inputs[["demo_remainder"]](tmp_path)
  
        # fill tmp folder
        if (dir.exists(tmp_path) && dir.exists(demo_dir))
        {
          #delay(100, {
            hide_tab()
          #})
  
          dir_copy(demo_dir, tmp_path, overwrite = TRUE)
  
          inputs[["demo"]](1) # temporary project ready
          inputs[["pr_path"]](tmp_path)
  
        }
        else
        {
          #browser()
          inputs[["demo"]](NULL)
          toggle_inputs(is.null(inputs[["demo"]]()))
        }
        # disable tab
        #shinyjs::addClass()
        #shinyjs::disable(selector = '.nav-tabs a[data-value="SC metadata"')
        #shinyjs::disable(selector = '.navbar-nav a[data-value="Annotations"')
        # load output
      }
    }
    #else
    #  inputs[["demo"]](NULL)

  }, ignoreInit = TRUE)


  # observeEvent(input[["sidebarItemExpanded"]], {
  #   print(input[["sidebarItemExpanded"]])
  #
  #   if(input$sidebarItemExpanded == "projects") {
  #
  #   }
  # })


  createmenuitem <- function(x, projs) {
    #browser()
    menuSubItem(
      text = names(projs)[x],
      tabName = str_replace_all(
        normalizePath(projs[[x]]),
        pattern = .Platform$file.sep,
        replacement = ","
      )
    )
  }

  observeEvent(input[["sidemenu"]], {
    #browser()
    projs <- update_proj_list(NULL)

    # createmenuitem <- function(x) {
    #   menuSubItem(
    #     text = names(projs)[x],
    #     tabName = str_replace_all(
    #       normalizePath(projs[[x]]),
    #       pattern = .Platform$file.sep,
    #       replacement = ","
    #     )
    #   )
    # }

    submenus <- lapply(seq_along(projs), FUN = function(x) {createmenuitem(x,projs)})
    recent_projects <- menuItem("Recent Projects", submenus,
               icon = icon("history"))

    output$recent_projects <- renderMenu({recent_projects})
    log2_print(recent_projects, msg = "LACEview: recent projects =")

  }, once = TRUE )

  observeEvent( input[["sidemenu"]],{
    log2_print(input[["sidemenu"]], msg = "LACEview:")
    #browser()
    if (input[["sidemenu"]] == "Small dataset") {
      inputs[["demo"]]("Small_dataset")
    }
    if (input[["sidemenu"]] == "Melanoma dataset") {
      inputs[["demo"]]("Rambow_dataset")
    }

    if (input[["sidemenu"]] != "Small dataset" && input[["sidemenu"]] != "Melanoma dataset") {
      proj_dir <- str_replace_all(input[["sidemenu"]], pattern = ",", replacement = .Platform$file.sep)

      inputs[["reload_project"]](proj_dir)
    }
    
    if (input[["sidemenu"]] == "quit_app")
      stopApp()
    if (input[["sidemenu"]] == "new_proj"){
      #inputs[["pr_path"]]("")
      print("NEW project!!")
      inputs[["pr_folder"]]("")
      inputs[["pr_folder_std"]](.my_actual_wd)
      inputs[["pr_name"]]("")
      inputs[["pr_name_std"]]("")
      
      inputs[["project_folder_std"]](.my_actual_wd)
      project_folder <- list( "path"="", "root"="")
      project_folder$path[[1]]<-""
      project_folder$path <-
        c(project_folder$path,
          as.list(path_split(path_rel(inputs[["project_folder_std"]](),
                                      start = roots_dir[[".."]]))[[1]]))
      project_folder$root=".."
      inputs[["project_folder"]](project_folder)
      
      updateActionButton(session,"pr_folder", label=inputs[["pr_folder_std"]]())
      updateTextInput(session,"pr_name", value=inputs[["pr_name_std"]]())
      #inputs[["pr_path"]](.my_actual_wd) #not necessary
    }
  }, ignoreInit = TRUE)

  # observe({
  #   if (input[["sidemenu"]] == "Small dataset") {
  #     inputs[["demo"]]("Small dataset")
  #   }
  #   if (input[["sidemenu"]] == "Melanoma dataset") {
  #     inputs[["demo"]]("Melanoma dataset")
  #   }
  # })

  observeEvent(input[["save_tab"]], {

    if(!inputs[["project_loaded"]]()) {
      showNotification("No porject created or loaded.", duration = 10, type = "warning")
      return(NULL)
    }

    config_path <- file.path(inputs[["project_folder_std"]](), os_conf_subdir)

    grep_str <- NULL
    if (input[["main_tabset"]] == "SC metadata") {
      grep_str <- m_grep_str
      config_file <- ".config_01_m.yml"
    } else if (input[["main_tabset"]] == "Annotations") {
      grep_str <- av_grep_str
      config_file <- ".config_02_av.yml"
    } else if (input[["main_tabset"]] == "Filters") {
      grep_str <- thr_grep_str
      config_file <- ".config_03_thr.yml"
    } else if (input[["main_tabset"]] == "SC sampling depths") {
      grep_str <- dp_grep_str
      config_file <- ".config_04_dp.yml"
    } else if (input[["main_tabset"]] == "Variants") {
      grep_str <- va_grep_str
      config_file <- ".config_05_va.yml"
    } else if (input[["main_tabset"]] == "Inference") {
      grep_str <- inf_grep_str
      config_file <- ".config_06_inf.yml"
    }

    if (!is.null(grep_str)) {
      doSave(grep_str, config_path, config_file)

      showNotification(paste(input[["main_tabset"]], "configuration tab saved"),
                       duration = 10)
    }
  })




  observeEvent(input[["load_tab"]], {

    if(!inputs[["project_loaded"]]()) {
      showNotification("No porject created or loaded.", duration = 10, type = "warning")
      return(NULL)
    }

    if(input[["main_tabset"]] == "Project")
      return(NULL)

    config_path <- file.path(inputs[["project_folder_std"]](), os_conf_subdir)

    if (input[["main_tabset"]] == "SC metadata") {
      m_doLoad_a(config_path)
      m_doLoad_b()

    } else if (input[["main_tabset"]] == "Annotations") {
      av_doLoad_a(config_path)
      uis <- av_dir_uis
      for (i in av_dir_uis) {
        av_doLoad_b(i)
      }
    } else if (input[["main_tabset"]] == "Filters") {
      thr_doLoad_a(config_path)
      uis <- thr_uis
      for(i in thr_uis) {
        thr_doLoad_b(i)
      }
    } else if (input[["main_tabset"]] == "SC sampling depths") {
      dp_doLoad_a(config_path)
      uis <- dp_uis
      for(i in dp_uis) {
        dp_doLoad_b(i)
      }

    } else if (input[["main_tabset"]] == "Variants") {
      va_doLoad_a(config_path) #
      uis <- va_uis
      for(i in va_uis) {
        va_doLoad_b(i)

      }
      va_doLoad_c()
    } else if (input[["main_tabset"]] == "Inference") {
      inf_doLoad_a(config_path)
      uis <- inf_uis
      for(i in uis) {
        inf_doLoad_b(i)
      }
      inf_doLoad_c()
    }

    showNotification(paste(input[["main_tabset"]], "configuration tab loaded"), duration = 10)
  })



  catch_ui_files_priority = 1

  observe({
    save_ui_state(m_grep_str, "config_01_m.yml")
    save_ui_state(av_grep_str, "config_02_av.yml")
    save_ui_state(thr_grep_str, "config_03_thr.yml")
    save_ui_state(dp_grep_str, "config_04_dp.yml")
    save_ui_state(va_grep_str, "config_05_va.yml")
    save_ui_state(inf_grep_str, "config_06_inf.yml")
  },
  priority = catch_ui_files_priority)

  ### End dashboard observers ####

  update_proj_list <- function (project_folder) {
    #browser()
    os_conf_tmp <- file.path(path_home(), os_conf[[.Platform$OS.type]], "LACE")
    if(dir.exists(os_conf_tmp)) {
      if(file.exists(file.path(os_conf_tmp, "projects_list.yml")))
        projects_list <- read.config(file = file.path(os_conf_tmp, "projects_list.yml"))
      else
        projects_list <- list()
    }
    else
      projects_list <- list()

    if(!is.list(projects_list))
      projects_list <- list()

    mask <- (projects_list %in% project_folder)

    if (length(which(mask))>0) {
      projects_list <- projects_list[-which(mask)]
    }

    if (!is.null(project_folder))
      projects_list <- c(list(project_folder), projects_list)

    names(projects_list) <- lapply(
      seq_along(projects_list),
      FUN = function(x) {
        basename(projects_list[[x]])
      })

    write.config(config.dat = projects_list,
                 file.path = file.path(os_conf_tmp, "projects_list.yml"),
                 write.type = "yaml")
    return(projects_list)
  }


  ### Dashboard outputs ####

  ### End dashboard outputs ####

  ### Variational reactiveVals ####


  va_uis <- c('va_depth_minimum',
              'va_missing_values_max',
              'va_minumum_median_total',
              'va_minumum_median_mutation')

  for (va_ui in va_uis)
    inputs[[va_ui]] <- reactiveVal()

  ## inputs[['va_data_in_dir']] <- reactive(inputs[['thr_out_dir']]())
  ## dir  <- out_subfolder_compute(inputs[['project_folder']](), 'filtered_var')
  ## inputs[['va_out_dir']] <- reactive(dir)
  inputs[['va_verified_genes']] <- reactiveVal()
  inputs[['va_list_genes']] <- reactiveVal()



  ## va_data_in_dir_ <- reactive(parseDirPath(roots=roots_dir, inputs[['va_data_in_dir']]()))
  va_depth_minimum_ <-
    reactive(inputs[['va_depth_minimum']]())
  va_missing_values_max_ <-
    reactive(inputs[['va_missing_values_max']]())
  va_minumum_median_total_ <-
    reactive(inputs[['va_minumum_median_total']]())
  va_minumum_median_mutation_ <-
    reactive(inputs[['va_minumum_median_mutation']]())
  va_out_dir_ <-
    reactive(parseDirPath(roots=roots_dir, inputs[['va_out_dir']]()))
  va_compute_output_ <- reactiveVal(NULL)

  files_ <- reactiveVal(NULL)
  list_genes_ <-
    reactive(inputs[['va_list_genes']]())
  verified_genes_ <-
    reactive(inputs[['va_verified_genes']]())

  va_rvs = reactiveValues(va_buttons = list(), va_observers = list())

  ### End variational reactiveVals ####


  ### Variational functions ####

  va_exec <- function () {

    if (length(va_out_dir_()) == 0)
      return()

    if (!is.integer(inputs[['thr_out_dir']]()) &&
        !is.integer(inputs[['dp_out_dir']]())) {
      if (dir.exists(thr_out_dir_()) &&
          dir.exists(dp_out_dir_())) {
        # NA_compute(va_depth_minimum_(),
        #            va_missing_values_max_(),
        #            thr_out_dir_(),
        #            dp_out_dir_(),
        #            va_out_dir_(),
        #            inputs[['m_time_points']]())
        files <- NA_compute2_load(thr_out_dir_(),
                                  dp_out_dir_(),
                                  va_out_dir_())
        files_(files)
      } else
        showNotification(paste("Annotated VCF folder does not exist"),
                         duration = 10,
                         type = "warning")
    } else
      showNotification(paste("Annotated VCF folder is not set"),
                       duration = 10,
                       type = "warning")
  }



  va_exec2 <- function() {
    req(inputs[['thr_out_dir']](), inputs[['dp_out_dir']]())
    if (!is.integer(inputs[['thr_out_dir']]()) &&
        !is.integer(inputs[['dp_out_dir']]()))
      if (dir.exists(thr_out_dir_()) &&
          dir.exists(dp_out_dir_()))

        if (any(!sapply(files_(), is.null))){ # files not found
          cells_aggregate_info <-
            NA_compute2(va_depth_minimum_(),
                        va_minumum_median_total_(),
                        va_minumum_median_mutation_(),
                        thr_out_dir_(),
                        dp_out_dir_(),
                        va_out_dir_(),
                        inputs[['m_time_points']](),
                        verified_genes_(),
                        files_())
          va_compute_output_(cells_aggregate_info)



        } else {
          if (!is.null(files_()))
            showNotification(paste("Files from previous steps not computed"),
                             duration = 10,
                             type = "warning")
        } else
          showNotification(paste("Sampling depth folder does not exist"),
                           duration = 10,
                           type = "warning")
    else
      showNotification(paste("Sampling depth folder is not set"),
                       duration = 10,
                       type = "warning")
  }

  ### End variational functions ####

  ### Variational observers ####

  observe({
    va_rvs$observers =
      lapply(va_uis,
             function(i) {
               observe({
                 req(inputs[[i]]())
               })
             }
      )
  },
  priority = -1)


  observeEvent(reactiveValuesToList(input), {
    outs <- outputOptions(output)
    lapply(names(outs),
           function(name) {
             outputOptions(output,
                           name,
                           suspendWhenHidden = FALSE)
           })
  },
  once = TRUE,
  priority = -1)


  observe({
    va_observers = lapply(va_uis,
                          function(i) {
                            observeEvent(input[[i]], {
                              ## req(input)
                              inputs[[i]](input[[i]])
                            })

                            output[[i]] <- renderText(inputs[[i]]())
                          })
  },
  priority = -1)



  observeEvent(inputs[['av_anovar_db_dir']](), {
    #browser()
    req(inputs[['av_anovar_db_dir']]())
    ## req(inputs[['thr_out_dir']](), inputs[['dp_out_dir']]())
    ## if (!is.integer(inputs[['thr_out_dir']]()) && !is.integer(inputs[['dp_out_dir']]())) {
    if (!is.integer(inputs[['av_anovar_db_dir']]())) {
      ## if (dir.exists(thr_out_dir_()) && dir.exists(dp_out_dir_())) {
      if (dir.exists(av_anovar_db_dir_())) {
        ## if (file.exists(file.path( thr_out_dir_(), "snpMut_filt_freq.rds"))){
        fa_file <-
          file_path_sans_ext(list.files(path = av_anovar_db_dir_(),
                                        pattern = ".*.fa")[1])
        ref_files <-
          list.files(path = av_anovar_db_dir_(),
                     pattern = ".*.[^f][^a]")
        ref_files2 <-
          file_path_sans_ext(ref_files)
        ref_files2 <-
          ref_files[str_detect(fa_file, ref_files2)]
        ref_files2 <-
          ref_files2[which.min(str_length(ref_files2))]
        ref_files2 <-
          file.path(av_anovar_db_dir_(), ref_files2)
        if (length(file.exists(ref_files2))>0) {
          if (file.exists(ref_files2)) {
            ref_info <-
              read.table(file = ref_files2,
                         header = FALSE,
                         sep = '\t',
                         stringsAsFactors = FALSE)
            list_gene_symbols <- ref_info[, 13] 
            inputs[['va_list_genes']](list_gene_symbols)
            updateSelectizeInput(session,
                                 'va_verified_genes',
                                 choices = inputs[['va_list_genes']](),
                                 server = TRUE)
            ## if (file.exists(file.path( av_anovar_db_dir_(), "snpMut_filt_freq.rds"))){
            ##  print('LOAD FILE2')
            ##  snpMut_filt_freq <- readRDS(file=paste0(file.path( thr_out_dir_(), "snpMut_filt_freq.rds")))
            ##  print(head(snpMut_filt_freq))
          } else {
            showNotification(paste("annovar reference file",
                                   ref_files2,
                                   "not found"),
                             duration = 10,
                             type = "warning")
          }
        }

      }
    }
  })


  observeEvent(input[['va_verified_genes']], {
    ## req(input)
    inputs[['va_verified_genes']](input[['va_verified_genes']])
  })

  
  
  
  va_iv <- InputValidator$new()
  va_iv$add_rule("va_depth_minimum", sv_between(1,100))
  va_iv$add_rule("va_missing_values_max", sv_between(0.,1.))
  va_iv$add_rule("va_minumum_median_total", sv_between(0.,50.))
  va_iv$add_rule("va_minumum_median_mutation", sv_between(0.,50.))
  va_iv$enable()

  observeEvent(input$va_exec,{
    req(va_iv$is_valid())
    va_exec()
  })


  observeEvent({ c(files_(),
                   va_depth_minimum_(),
                   va_missing_values_max_(),
                   va_minumum_median_total_(),
                   va_minumum_median_mutation_(), verified_genes_())
  }, { va_exec2() },
  ignoreNULL = FALSE,
  ignoreInit = TRUE)


  ### End Variational observers ####

  ### Variational outputs ####

  output$va_out <-
    DT::renderDT(va_compute_output_(), server = TRUE)
  output[['va_verified_genes']] <-
    renderText(inputs[['va_verified_genes']]())
  output[['va_list_genes']] <-
    renderPrint(inputs[['va_list_genes']]())
  output[['va_out_dir']] <-
    renderText(parseDirPath(roots = roots_dir,
                            inputs[['va_out_dir']]()))

  output[['va_filters']] <-
    renderPrint({
      list(va_data_in_dir = parseDirPath(roots=roots_dir,
                                         inputs[['thr_out_dir']]()),
           depth_minimum = inputs$va_depth_minimum(),
           missing_values_max = inputs$va_missing_values_max(),
           minumum_median_total = inputs$va_minumum_median_total(),
           minumum_median_mutation = inputs$va_minumum_median_mutation(),
           va_out_dir = parseDirPath(roots = roots_dir,
                                     inputs[['va_out_dir']]()),
           va_verified_genes = inputs[['va_verified_genes']]())
    })

  ### End variational outputs ####


  ### Depth reactivaVals ####


  dp_uis <- c('dp_samtools_exec_dir', 'dp_bam_dir')

  dp_rvs = reactiveValues(dp_buttons = list(), dp_observers = list())

  ###
  for (dir_ui in dp_uis) {
    # defaultPath <- ""
    # if(dir_ui == "dp_samtools_exec_dir")
    #   defaultPath <- path_rel(dirname(Sys.which("samtools")), start = "..")

    shinyDirChoose(input,
                   dir_ui,
                   roots = roots_dir,
                   filetypes = c('', 'txt'),
                   defaultPath = defaultPath,
                   defaultRoot = "..")
  }

  for (dp_ui in dp_uis)
    inputs[[dp_ui]] <- reactiveVal()

  inputs[['dp_mut_dir']] <-
    reactive(inputs[['thr_out_dir']]())
  ## inputs[['dp_out_dir']] <- reactiveVal()


  dp_samtools_exec_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['dp_samtools_exec_dir']]()))
  dp_samtools_ <-
    reactive(file.path(dp_samtools_exec_dir_(),
                       'samtools'))
  dp_bam_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['dp_bam_dir']]()))
  dp_out_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['dp_out_dir']]()))
  dp_mut_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['dp_mut_dir']]()))

  ### End depth reactivaVals ####

  ### Depth observers ####

  observe({
    dp_rvs$observers =
      lapply(dp_uis,
             function(i) {
               observe({
                 req(inputs[[i]]())
                 updateActionButton(session, i, label = set_dir_ui(i))
               })
             }
      )
  },
  priority = -1)


  observeEvent(reactiveValuesToList(input), {
    outs <- outputOptions(output)
    lapply(names(outs),
           function(name) {
             outputOptions(output,
                           name,
                           suspendWhenHidden = FALSE)
           })
  },
  once = TRUE,
  priority = -1)

  ## observeEvent(reactiveValuesToList(input),{
  ##   lapply(dp_uis, function(i) {
  ##     click(i)
  ##     hide(paste0(i,'-modal'))
  ##     delay(300,runjs(paste0("$('#",i,"-modal #sF-cancelButton').click()")))
  ##   })
  ## },once = T, priority = -1)


  observe({
    dp_observers = lapply(dp_uis,
                          function(i) {
                            observeEvent(input[[i]], {
                              req(input)
                              inputs[[i]](input[[i]])
                            })

                            output[[i]] <-
                              renderText(parseDirPath(roots = roots_dir,
                                                      inputs[[i]]()))
                          }
    )},
    priority = -1)


  observeEvent(dp_samtools_exec_dir_(), {
    if (!is.integer(input[['dp_samtools_exec_dir']])) {
      if (file.exists(dp_samtools_()))
        showNotification(paste("found samtools"),
                         duration = 10,
                         type = "message")
      else
        showNotification(paste("samtools", 'not present in',
                               dp_samtools_exec_dir_()),
                         duration = 10,
                         type = "warning")
    }
  })

  ### End depth observers ####

  ### Depth functions ####

  dp_exec <- function() {

    if (length(dp_out_dir_()) == 0)
      return()

    Opt = list() # MA:  Perche` maiuscolo il nome della variabile?
    Opt$StoredFile <- file.path(config_path, ".config_04_dp.yml")
    Opt$ActFile <- file.path(config_path, "config_04_dp.yml")
    Opt$ExtraFiles = NULL

    OutFiles <- c("final_data_depth.txt", "final_data_mut.txt")
    chk_files <- ManyInSomeOut(dp_bam_dir_(),
                               InFilesMask = ".*.bam$",
                               OutDir=dp_out_dir_(),
                               OutFiles = OutFiles,
                               Opt = Opt)

    InFilesToDo <- chk_files$InFilesToDo

    ## Use metadata if available
    if (!is.integer(inputs[['sc_metadata_file']]())) {
      if (file.exists(sc_metadata_label_())) {
        if (inputs[['m_id_column']]() %in% colnames(sc_metadata_()))
          InFilesToDo <-
            InFilesToDo[str_split_fixed(InFilesToDo,
                                        "\\.",
                                        n = 2)[, 1] %in%
                          as.character(sc_metadata_()[[inputs[['m_id_column']]()]])]
        else
          showNotification(paste("no valid metadata id column"),
                           duration = 10,
                           type = "warning")
      } else
        showNotification(paste("no valid metadata file"), duration = 10, type = "warning")
    } else
      showNotification(paste("no valid metadata file"), duration = 10, type = "warning")


    InFilesToDo <-
      file.path(dp_bam_dir_(), chk_files$InFilesToDo)
    OutFilesToRm <-
      file.path(dp_out_dir_(), chk_files$OutFilesToRm)

    if (!is.integer(inputs[['dp_samtools_exec_dir']]()) &
        !is.integer(inputs[['dp_bam_dir']]()) &
        !is.integer(inputs[['dp_mut_dir']]())) {
      if (dir.exists(dp_samtools_exec_dir_()) &
          file.exists(dp_samtools_()) &
          dir.exists(dp_bam_dir_()) & dir.exists(dp_mut_dir_())) {
        dp_compute(sc_metadata_(),
                   dp_bam_dir_(),
                   dp_samtools_(),
                   thr_out_dir_(),
                   dp_out_dir_(),
                   inputs[['m_id_column']](),
                   InFilesToDo,
                   OutFilesToRm)
        ## return(read_file(file.path(dp_out_dir_(), 'stdout.log')))
      } else
        showNotification(paste("one of the folders or executable does not exist"),
                         duration = 10,
                         type = "warning")
    } else
      showNotification(paste("one of the folders is not set"),
                       duration = 10,
                       type = "warning")
    return("possible errors")
  }

  ### End depth functions ####

  ### Depth outputs ####

  output$dp_out <- eventReactive(input$dp_exec,{
    dp_exec()
  })



  output[['dp_mut_dir']] <-
    renderText(parseDirPath(roots = roots_dir,
                            inputs[['dp_mut_dir']]()))

  output[['dp_par']] <-
    renderPrint({
      list(samtools =parseDirPath(roots = roots_dir,
                                  inputs$dp_samtools_exec_dir()),
           bam_dir = parseDirPath(roots = roots_dir,
                                  inputs$dp_bam_dir()),
           mut_dir = parseDirPath(roots = roots_dir,
                                  inputs$dp_mut_dir()),
           out_dir = parseDirPath(roots = roots_dir,
                                  inputs$dp_out_dir())
      )
    })

  ### End depth outputs ####




  ### Threshold reactiveVals #### 

  thr_uis = c('thr_alleles_ratio',
              'thr_maf',
              'thr_freq',
              'thr_accepted_var',
              'thr_negleted_var')

  ###
  thr_rvs = reactiveValues(thr_buttons = list(),
                           thr_observers = list())

  for (thr_ui in thr_uis)
    inputs[[thr_ui]] <- reactiveVal()



  thr_alleles_ratio_ <-
    reactive(inputs[['thr_alleles_ratio']]())
  thr_maf_ <-
    reactive(inputs[['thr_maf']]())
  thr_freq_ <-
    reactive(inputs[['thr_freq']]())
  thr_out_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['thr_out_dir']]()))
  thr_vcf_in_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['thr_vcf_in_dir']]()))


  exonic_list <- list("frameshift insertion",
                      "frameshift deletion",
                      "frameshift block substitution",
                      "stopgain",
                      "stoploss",
                      "nonframeshift insertion",
                      "nonframeshift deletion",
                      "nonframeshift block substitution",
                      "nonsynonymous SNV",
                      "synonymous SNV",
                      "unknown")

  ### End threshold reactiveVals ####


  ### Threshold functions ####

  
  
  thr_exec <- function() {
    if (length(thr_out_dir_()) == 0)
      return()

    Opt = list()
    Opt$StoredFile <- file.path(config_path, ".config_03_thr.yml")
    Opt$ActFile <- file.path(config_path, "config_03_thr.yml")
    Opt$ExtraFiles = NULL

    ## chk_files <- OneInOneOut( thr_vcf_in_dir_(), InFilesMask = ".*.exonic_variant_function$", thr_out_dir_(), OutExt = "anninput", Opt = Opt)

    OutFiles <- c("cells_aggregate_info.rds",
                  "scMutInfo.rds",
                  "SNPInfo.rds",
                  "snpMut_filt_freq.rds"
    )
    chk_files <- ManyInSomeOut(thr_vcf_in_dir_(),
                               InFilesMask =
                                 ".*.exonic_variant_function$",
                               OutDir=thr_out_dir_(),
                               OutFiles = OutFiles,
                               Opt = Opt)

    InFilesToDo <- chk_files$InFilesToDo

    ## use metadata if available
    if (!is.integer(inputs[['sc_metadata_file']]())) {
      if (file.exists(sc_metadata_label_())){
        if (inputs[['m_id_column']]() %in% colnames(sc_metadata_()))
          InFilesToDo <-
            InFilesToDo[str_split_fixed(InFilesToDo,
                                        "\\.",
                                        n = 2)[, 1] %in%
                          as.character(sc_metadata_()[[inputs[['m_id_column']]()]])]
        else
          showNotification(paste("no valid metadata id column"),
                           duration = 10,
                           type = "warning")
      } else
        showNotification(paste("no valid metadata file"),
                         duration = 10,
                         type = "warning")
    } else
      showNotification(paste("no valid metadata file"),
                       duration = 10,
                       type = "warning")

    InFilesToDo <-
      file.path(thr_vcf_in_dir_(),
                chk_files$InFilesToDo)
    OutFilesToRm <-
      file.path(thr_out_dir_(),
                chk_files$OutFilesToRm)


    if (!is.integer(inputs[['thr_vcf_in_dir']]())) {
      if (dir.exists(thr_vcf_in_dir_())) {


        ## filter1_compute(thr_vcf_in_dir_(), sc_metadata_(), thr_alleles_ratio_(), thr_maf_(), thr_freq_(), thr_out_dir_(), m_id_column_(), m_time_column_())
        filter1_compute(thr_vcf_in_dir_(),
                        sc_metadata_(),
                        thr_alleles_ratio_(),
                        thr_maf_(),
                        thr_freq_(),
                        thr_out_dir_(),
                        inputs[['m_id_column']](),
                        inputs[['m_time_column']](),
                        inputs[['thr_accepted_var']](),
                        InFilesToDo,
                        OutFilesToRm)


        filter2_compute(thr_vcf_in_dir_(),
                        sc_metadata_(),
                        thr_alleles_ratio_(),
                        thr_maf_(),
                        thr_freq_(),
                        thr_out_dir_(),
                        m_id_column_(),
                        m_time_column_(),
                        InFilesToDo,
                        OutFilesToRm)

        filter3_compute(thr_vcf_in_dir_(),
                        sc_metadata_(),
                        thr_alleles_ratio_(),
                        thr_maf_(),
                        thr_freq_(),
                        thr_out_dir_(),
                        m_id_column_(),
                        m_time_column_(),
                        inputs[['m_time_points']](),
                        InFilesToDo,
                        OutFilesToRm)

        return(read_file(file.path(thr_vcf_in_dir_(),
                                   'stdout.log')))
      } else
        showNotification(paste("Filtered VCF folder does not exist"),
                         duration = 10,
                         type = "warning")
    } else
      showNotification(paste("Filtered VCF folder is not set"),
                       duration = 10,
                       type = "warning")
    return("Possible errors") # MA: return or raise an error?
  }

  ### End threshold functions ####

  ### Threshold observers ####
  observe({
    thr_rvs$observers = lapply(thr_uis,
                               function(i) {
                                 observe({
                                   req(inputs[[i]]())
                                   ## updateActionButton(session, i, label = set_dir_ui(i))
                                   updateNumericInput(session, i, value = inputs[[i]]())
                                 })
                               }
    )
  },
  priority = -1)



  observeEvent(reactiveValuesToList(input), {
    outs <- outputOptions(output)
    lapply(names(outs),
           function(name) {
             outputOptions(output,
                           name,
                           suspendWhenHidden = FALSE)
           })
  },
  once = TRUE,
  priority = -1)


  ## # observeEvent(reactiveValuesToList(input),{
  ## #   lapply(thr_uis, function(i) {
  ## #     click(i)
  ## #     hide(paste0(i,'-modal'))
  ## #     delay(300,runjs(paste0("$('#",i,"-modal #sF-cancelButton').click()")))
  ## #   })
  ## # },once = T, priority = -1)

  ###



  ## #dir <- out_subfolder_compute(inputs[['project_folder']](),'filtered_vcf')
  ## #inputs[['thr_out_dir']] <- reactive(dir)



  observe({
    thr_observers = lapply(thr_uis,
                           function(i) {
                             observeEvent(input[[i]], {
                               req(input)
                               inputs[[i]](input[[i]])
                             })

                             output[[i]] <- renderPrint(inputs[[i]]())
                           }
    )
  },
  priority = -1)


  ## Exonic ui
  observeEvent(reactiveValuesToList(input), {
    inputs[['thr_accepted_var']](exonic_list)
    inputs[['thr_negleted_var']](NULL)

  },
  once = TRUE,
  priority = -1)

  output$thr_bucket_var_list <-
    renderUI({
      ## showNotification(paste("renderUI[['thr_bucket_var_list']]"), duration = NULL)

      bucket_list_basic <-
        bucket_list(
          header = "Choose the variant functions for inferential analysis",
          add_rank_list(
            text = "Considered exonic variants",
            labels = inputs[['thr_accepted_var']](),
            input_id = "thr_accepted_var",
            options = sortable_options(multiDrag = TRUE, disabled = !is.null(inputs[["demo"]]()))
          ),
          add_rank_list(
            text = "Neglected exonic variants",
            labels = inputs[['thr_negleted_var']](),
            input_id = "thr_negleted_var",
            options = sortable_options(multiDrag = TRUE, disabled = !is.null(inputs[["demo"]]()))
          ),
          group_name = "thr_bucket_var_list",
          orientation = "horizontal", 
          options = sortable_options(disabled = !is.null(inputs[["demo"]]()))
        )

      x <-
        tagList(
          span() %>%
            make_title_help_popover (
              tags$b("Exonic variant function annotations"),
              "List of exonic varian functions",
              text[["thr_bucket_var_list"]],
              server = TRUE
            ),
          bucket_list_basic
        )
      if (loading_()) {
        loaded_(TRUE)
      }
      x
    })

  thr_iv <- InputValidator$new()
  thr_iv$add_rule("thr_alleles_ratio", sv_between(0,1))
  thr_iv$add_rule("thr_maf", sv_between(0.,0.5))
  thr_iv$add_rule("thr_freq", sv_between(0.,1.))
  thr_iv$enable()
  ### Threshold observers ####

  ### Threshold outputs ####
  output[['thr_vcf_in_dir']] <-
    renderText(parseDirPath(roots = roots_dir,
                            inputs[['thr_vcf_in_dir']]()))
  output[['thr_out_dir']] <-
    renderText(parseDirPath(roots = roots_dir,
                            inputs[['thr_out_dir']]()))
  output[['thr_filters']] <-
    renderPrint({
      list(thr_vcf_in_dir = parseDirPath(roots = roots_dir,
                                         inputs[['thr_vcf_in_dir']]()),
           alleles_ratio = inputs$thr_alleles_ratio(),
           maf = inputs$thr_maf(),
           freq = inputs$thr_freq(),
           thr_out_dir = parseDirPath(roots = roots_dir,
                                      inputs[['thr_out_dir']]()) )
    })

  output$thr_out <- eventReactive(input$thr_exec, {
    req(thr_iv$is_valid())
    thr_exec()
  })

  ### End threshold outputs ####

  ### Annotation reactiveVals ####

  av_dir_uis= c('av_anovar_exec_dir',
                'av_anovar_db_dir',
                'av_vcf_in_dir')

  for (dir_ui in av_dir_uis)
    inputs[[dir_ui]] <- reactiveVal()

  av_rvs = reactiveValues(av_buttons = list(), av_observers = list())

  ## #dir <- out_subfolder_compute(inputs[['project_folder']](),'vcf_out')
  ## #inputs[['av_vcf_out_dir']] <- reactive(dir)

  av_anovar_exec_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['av_anovar_exec_dir']]()))
  av_anovar_convert_ <-
    reactive(file.path(av_anovar_exec_dir_(),
                       'convert2annovar.pl'))

  av_anovar_db_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['av_anovar_db_dir']]()))
  av_vcf_in_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['av_vcf_in_dir']]()))
  av_vcf_out_dir_ <-
    reactive(parseDirPath(roots = roots_dir,
                          inputs[['av_vcf_out_dir']]()))
  av_anovar_annot_ <-
    reactive(file.path(av_anovar_exec_dir_(),
                       'annotate_variation.pl'))

  ### End annotation reactiveVals ####


  ### Annotation functions ####

  set_dir_ui <- function(dir_id,
                         default_dir_label = 'Select folder',
                         input_var = inputs,
                         basedir = "..") { # replace with roots_dir
    this_label <- default_dir_label
    meta <- input_var[[dir_id]]()
    ## if (basedir %in% names(meta))
    if (!is.null(meta))
      if (!is.integer(meta))
        if (dir.exists(parseDirPath(roots = roots_dir, meta)))
          this_label <- parseDirPath(roots = roots_dir, meta)
    return(this_label)
  }


  out_subfolder_compute <- function(out_dir_inp, sub_dir) {
    if ('path' %in% names(out_dir_inp)) {
      tmp <- out_dir_inp[['path']]
      ## tmp <- tmp[-length(tmp)]
      out_dir_inp[['path']] <- c(tmp, sub_dir)
    }
    return (out_dir_inp)
  }

  av_exec <- function() {

    if(length(av_vcf_out_dir_())==0)
      return()

    log2_print('av_exec', msg = "LACEview:")
    log2_print(paste("av_vcf_out_dir_()", av_vcf_out_dir_()), msg = "LACEview:")

    Opt = list()
    Opt$StoredFile <- file.path(config_path,".config_02_av.yml")
    Opt$ActFile <- file.path(config_path,"config_02_av.yml")
    Opt$ExtraFiles = NULL

    chk_files <- OneInOneOut(av_vcf_in_dir_(),
                             InFilesMask = ".*filtered.vcf$",
                             av_vcf_out_dir_(),
                             OutExt =
                               "anninput.exonic_variant_function",
                             Opt = Opt)

    InFilesToDo <- chk_files$InFilesToDo

    ## Use metadata if available
    if (!is.integer(inputs[['sc_metadata_file']]())) {
      if (file.exists(sc_metadata_label_())){
        if (inputs[['m_id_column']]() %in% colnames(sc_metadata_()))
          InFilesToDo <-
            InFilesToDo[str_split_fixed(InFilesToDo,
                                        "\\.",
                                        n = 2)[, 1] %in%
                          as.character(sc_metadata_()[[inputs[['m_id_column']]()]])]
        else
          showNotification(paste("no valid metadata id column"),
                           duration = 10,
                           type = "warning")
      }
      else
        showNotification(paste("no valid metadata file"),
                         duration = 10,
                         type = "warning")
    }
    else
      showNotification(paste("no valid metadata file"),
                       duration = 10,
                       type = "warning")


    InFilesToDo <- file.path(av_vcf_in_dir_(),
                             chk_files$InFilesToDo)
    OutFilesToRm <- file.path(av_vcf_out_dir_(),
                              chk_files$OutFilesToRm)

    if (!is.integer(inputs[['av_anovar_exec_dir']]()) &
        !is.integer(inputs[['av_anovar_db_dir']]()) &
        !is.integer(inputs[['av_vcf_in_dir']]())
    ) {

      if (dir.exists(av_anovar_exec_dir_()) &
          file.exists(av_anovar_convert_()) &
          file.exists(av_anovar_annot_()) &
          dir.exists(av_anovar_db_dir_()) &
          dir.exists(av_vcf_in_dir_())
      ) {
        av_compute(av_anovar_convert_(),
                   av_anovar_annot_(),
                   av_anovar_db_dir_(),
                   av_vcf_in_dir_(),
                   av_vcf_out_dir_(),
                   InFilesToDo = InFilesToDo,
                   OutFilesToRm = OutFilesToRm)
        return(read_file(file.path(av_vcf_out_dir_(),
                                   'stdout.log')))
      } else
        showNotification(paste("one of the folders or executable does not exist"),
                         duration = 10,
                         type = "warning")
    } else
      showNotification(paste("one of the folders is not set"),
                       duration = 10,
                       type = "warning")
    return("possible errors") # MA:  CHECK THESE!!!! IS THE CHECK!!!
  }

  ### End annotation functions ####

  ### Annotation observers ####

  for (dir_ui in av_dir_uis) { #av_rvs$buttons???
    # defaultPath <- ""
    # if(dir_ui == "av_anovar_exec_dir")
    #   defaultPath <- path_rel(Sys.which("annotate_variation.pl"), start = "..")
    shinyDirChoose(input,
                   dir_ui,
                   roots = roots_dir,
                   defaultPath = defaultPath,
                   filetypes = c('', 'txt'),
                   defaultRoot = "..")
  }

  ## Update buttons
  observe({
    av_rvs$observers = lapply(av_dir_uis,
                              function(i) {
                                observe({
                                  req(inputs[[i]]())
                                  updateActionButton(session,
                                                     i,
                                                     label = set_dir_ui(i))
                                })
                              }
    )
    ## reactiveValuesToList(av_rvs)
  },
  priority = -1)


  ## Make ui updatable
  observeEvent(reactiveValuesToList(input), {
    outs <- outputOptions(output)
    lapply(names(outs),
           function(name) {
             outputOptions(output,
                           name,
                           suspendWhenHidden = FALSE)
           })
  },
  once = TRUE,
  priority = -1)

  ## observeEvent(reactiveValuesToList(input), {
  ##     lapply(av_dir_uis,
  ##            function(i) {
  ##                click(i)
  ##                hide(paste0(i, '-modal'))
  ##                delay(300,
  ##                      runjs(paste0("$('#",
  ##                                   i,
  ##                                   "-modal #sF-cancelButton').click()")))
  ##            })

  ## #delay(1000, runjs("$('.sF-modal modal-dialog modal-lg').keydown(new KeyboardEvent('keydown', {'keyCode': 40}))"))
  ## #delay(1000,"$('#av_anovar_exec_dir-modal').dispatchEvent(new KeyboardEvent('keydown', {'keyCode': 40}))")
  ## #delay(1000,runjs("handleArrowKey('down')"))
  ## },
  ## once = T,
  ## priority = -1)


  observe({
    av_observers = lapply(av_dir_uis,
                          function(i) {
                            observeEvent(input[[i]], {
                              req(input)
                              inputs[[i]](input[[i]])
                            })

                            output[[i]] <-
                              renderText(parseDirPath(roots = roots_dir,
                                                      inputs[[i]]()))
                          }
    )
    ## reactiveValuesToList(av_rvs)
  },
  priority = -1)



  ## observeEvent(inputs[['av_anovar_exec_dir']](),{
  observeEvent(av_anovar_exec_dir_(), {
    if (!is.integer(input[['av_anovar_exec_dir']])) {
      if (file.exists(av_anovar_convert_()))
        showNotification(paste("found convert2annovar.pl"),
                         duration = 10,
                         type = "message")
      else
        showNotification(paste("convert2annovar.pl",
                               'not present in',
                               av_anovar_exec_dir_()),
                         duration = 10,
                         type = "warning")
      if (file.exists(av_anovar_annot_()))
        showNotification(paste("found annotate_variation.pl"),
                         duration = 10,
                         type = "message")
      else
        showNotification(paste("annotate_variation.pl",'not present in',
                               av_anovar_exec_dir_()),
                         duration = 10,
                         type = "warning")
    }
  })

  output$av_out <- eventReactive(input$av_exec, {
    av_exec()
  },
  ignoreInit = TRUE)

  ### End annotation observers ####

  ### Annotation outputs ####

  output[['av_vcf_out_dir']] <-
    renderText(parseDirPath(roots = roots_dir,
                            inputs[['av_vcf_out_dir']]()))
  output$av_anovar_convert <-
    renderText(av_anovar_convert_())
  output$av_anovar_annot <-
    renderText(av_anovar_annot_())

  ### End annotation outputs ####




  ### Metadata reactiveVals ####


  loaded_input_ <- reactiveVal(list())
  loaded_ <- reactiveVal(FALSE)
  m_widgetId_ <- reactiveVal()


  loaded_input <- list()
  initialInputs <- list()
  initialInputs_copy <- list()
  the_files <- list()

  ### End metadata reactiveVals ####

  ### Metadata observers ####

  ## add types to ui #remove
  observeEvent(input, {
    initialInputs <<- reactiveValuesToList(input) # MA: WTF! Ma decidersi sugli assegnamenti????
    for (inp in names(initialInputs)) {
      ## print(inp)
      runjs(getWidgetType(inp))
      ## print(input$inputType)
    }
  },
  once = TRUE,
  priority = 5)


  ## Get types #remove
  observe({
    types <- list()
    for (inp in names(initialInputs))
      types[[inp]]<-input[[paste0('inputType_',inp)]]
    types_(types)
  },
  priority = 4)


  #####



  #####

  ## widgetId output
  observe({
    reactiveValuesToList(input)
    types<-types_()
    for (n in names(initialInputs_copy)) {
      if (n %in% names(types))
        if (types[[n]] == 'shinyFiles')
          if (n %in% names(the_files)) {
            ## initialInputs_copy[[n]] <<- the_files[[n]]
          }
    }
    m_widgetId_(initialInputs_copy)
  },
  priority = catch_ui_files_priority - 1)

  output$m_widgetId <- renderPrint({
    m_widgetId_()
  })


  shinyFileChoose(input,
                  'sc_metadata_file',
                  roots = roots_dir,
                  filetypes = c('rds',''),
                  defaultRoot = "..")


  ## Set_ui_files
  set_file_ui <- function(dir_id,
                          default_dir_label = 'Select file',
                          input_var = inputs,
                          basedir = "..") { #replace with roots_dir
    this_label <- default_dir_label
    meta <- input_var[[dir_id]]()
    ## if (basedir %in% names(meta))
    if (!is.null(meta))
      if (!is.integer(meta))
        if (file.exists(as.character(parseFilePaths(roots = roots_dir,
                                                    meta)[['datapath']])))
          this_label <- as.character(parseFilePaths(roots = roots_dir,
                                                    meta)[['datapath']])
    ## showNotification(this_label, duration = 10)
    return(this_label)
  }

  ## # output$m_sc_metadata_col <- reactive({
  ## #   if (length(sc_metadata_())>0) {
  ## #     sc_metadata_col <- colnames(sc_metadata_())
  ## #   } else {
  ## #     sc_metadata_col <- c("")
  ## #   }
  ## #   sc_metadata_col
  ## # })

  #####
  m_dir_uis = c('sc_metadata_file',
                'm_time_column',
                'm_time_points',
                'm_id_column')
  ## inputs <- list()
  for (dir_ui in m_dir_uis)
    inputs[[dir_ui]] <- reactiveVal()

  loading_ <- reactiveVal(FALSE)
  loaded_ <- reactiveVal(FALSE)


  #####
  sc_metadata_label_ <- reactiveVal("")
  sc_metadata_ <- reactiveVal()
  choices_ <- reactiveVal()

  observeEvent(input[['sc_metadata_file']],{
    if (! loading_()) {
      ## showNotification(paste("input[['sc_metadata_file']]","inputs[['sc_metadata_file']](input[['sc_metadata_file']])"), duration = NULL)

      inputs[['sc_metadata_file']](input[['sc_metadata_file']])

      ## print("set_file_ui('sc_metadata_file')")
      ## print(set_file_ui('sc_metadata_file'))

      sc_metadata_label_(set_file_ui('sc_metadata_file'))

      if (file.exists(sc_metadata_label_()) ) {
        if (file_ext(sc_metadata_label_()) %in% c("rds", "RDS") ) {
          cellInfo <- readRDS( sc_metadata_label_() )
          if (!is.data.frame(cellInfo)){
            showNotification("No metadata file",
                             type = "warning",
                             duration = 10)
            cellInfo <- data.frame()
          }
        } else
          cellInfo <- fread(sc_metadata_label_(), header=TRUE, data.table=FALSE)
      } else {
        showNotification("No metadata file",
                         type = "warning",
                         duration = 10)

        # hideTab(inputId = "main_tabset",
        #         target = "Annotations")
        # hideTab(inputId = "main_tabset",
        #         target = "Filters")
        # hideTab(inputId = "main_tabset",
        #         target = "SC sampling depths")
        # hideTab(inputId = "main_tabset",
        #         target = "Variants")
        # hideTab(inputId = "main_tabset",
        #         target = "Inference")
        # hideTab(inputId = "main_tabset",
        #         target = "Results")

        cellInfo <- data.frame()
      }
      sc_metadata_(cellInfo)

      if ( file.exists(sc_metadata_label_()) )
        choices_(colnames(sc_metadata_()))
      else
        choices_(c(""))

      id_col <- NULL
      time_col <- NULL
      if (length(choices_()) > 2) {
        id_col <- choices_()[1]
        time_col <- choices_()[2]
      }
      inputs[['m_id_column']](id_col)
      inputs[['m_time_column']](time_col)

      time_points <- NULL
      if (!is.null(time_col))
        time_points <- unique(as.character(sc_metadata_()[, time_col]))
      inputs[['m_time_points']](time_points)
    }
  })

  observeEvent(input[['m_id_column']], {
    if (! loading_()) {
      ## showNotification(paste("input[['m_time_column']]","inputs[['m_time_column']](input[['m_time_column']])"), duration = NULL)

      id_col <- input[['m_id_column']]
      inputs[['m_id_column']](id_col)

      if ((inputs[['m_time_column']]() == inputs[['m_id_column']]()) &&
          !("" == inputs[['m_id_column']]()))
        showNotification(paste("Same column for cell Id and time"),
                         type = "warning",
                         duration = 10)
      ## # id_points <- NULL
      ## # if ( id_col %in% colnames(sc_metadata_()) )
      ## #   id_points <- unique(as.character(sc_metadata_()[, id_col]))
      ## # inputs[['m_id_points']](id_points)
    }
  })

  observeEvent(input[['m_time_column']], {
    if (! loading_()) {
      time_col <- input[['m_time_column']]
      inputs[['m_time_column']](time_col)

      time_points <- NULL
      if (time_col %in% colnames(sc_metadata_()) )
        time_points <- unique(as.character(sc_metadata_()[, time_col]))
      inputs[['m_time_points']](time_points)
      if ((inputs[['m_time_column']]() == inputs[['m_id_column']]()) &&
          !(""==inputs[['m_time_column']]()))
        showNotification(paste("Same column for cell Id and time"),
                         type = "warning",
                         duration = 10)
    }
  })


  observeEvent(input[['m_time_points']], {
    if (! loading_()) {
      inputs[['m_time_points']](input[['m_time_points']])
    }
    if (loading_() & loaded_()) {
      loading_(FALSE)
      loaded_(FALSE)
    }
    if (file.exists(sc_metadata_label_()) )
      if (!is.null(input[['m_time_points']]))
        if (length(input[['m_time_points']]) > 1) {
          # showTab(inputId = "main_tabset",
          #         target = "Annotations")
          # showTab(inputId = "main_tabset",
          #         target = "Filters")
          # showTab(inputId = "main_tabset",
          #         target = "SC sampling depths")
          # showTab(inputId = "main_tabset",
          #         target = "Variants")
          # showTab(inputId = "main_tabset",
          #         target = "Inference")
        }
  })

  observeEvent(input[["pr_next"]],{ # The previous does not trigger the event for consecutive load
    if ( file.exists(sc_metadata_label_()) )
      if (!is.null(input[['m_time_points']]))
        if (length(input[['m_time_points']]) > 1) {
          # showTab(inputId = "main_tabset",
          #         target = "Annotations")
          # showTab(inputId = "main_tabset",
          #         target = "Filters")
          # showTab(inputId = "main_tabset",
          #         target = "SC sampling depths")
          # showTab(inputId = "main_tabset",
          #         target = "Variants")
          # showTab(inputId = "main_tabset",
          #         target = "Inference")
        }
    # delay(1000,
    #       updateTabsetPanel(session, "main_tabset", selected = "SC metadata")
    # )

  })


  observeEvent(inputs[['sc_metadata_file']](), {
    ## update the file select button name
    ## showNotification(paste("inputs[['sc_metadata_file']]","updateActionButton"), duration = NULL)
    updateActionButton(session,
                       'sc_metadata_file',
                       label = set_file_ui('sc_metadata_file'))
  })


  ### problema update ma necessario
  observeEvent(inputs[['m_time_column']](), {
    ## update time_col ui
    ## showNotification(paste("inputs[['m_time_column']]","updateSelectInput"), duration = NULL)
    updateSelectInput(session,
                      "m_time_column",
                      ## label = "Time column",
                      choices = choices_(),
                      selected = inputs[['m_time_column']]()
    )
  })

  observeEvent(inputs[['m_id_column']](),{
    ## Update time_col ui
    ## showNotification(paste("inputs[['m_time_column']]","updateSelectInput"), duration = NULL)
    updateSelectInput(session,
                      "m_id_column",
                      ## label = "Cell id column",
                      choices = choices_(),
                      selected = inputs[['m_id_column']]()
    )
  })

  ######

  observe({
    hide(id = "m_idCol_div")
    if (file.exists(sc_metadata_label_()) ) {
      shinyjs::show(id="m_idCol_div")
    }
  })

  observe({
    hide(id = "m_timePointsCol_div")
    if (file.exists(sc_metadata_label_()) ) {
      shinyjs::show(id="m_timePointsCol_div")
    }
  })

  ## Time_column ui
  output$m_idCol <-
    renderUI(selectInput("m_id_column", "Cell id column", c("")) %>%
               make_shiny_help_popover ("Ids column",
                                        text[["m_idCol"]],
                                        server = TRUE
               )
    )

  output$m_timePointsCol <-
    renderUI(selectInput("m_time_column", "Time column", c("")) %>%
               make_shiny_help_popover ("Sampling times column",
                                        text[["m_timePointsCol"]],
                                        server = TRUE
               )
    )


  ## Time_points ui

  output$m_timePoints <-
    renderUI({
      rank_list_basic <-
        rank_list(text = "Drag the time points in chronological order (before the smaller times)",
                  labels = inputs[['m_time_points']](),
                  input_id = "m_time_points", 
                  options = sortable_options(disabled = !is.null(inputs[["demo"]]()))
        )
      x <- rank_list_basic %>%
        make_title_help_popover (tags$b("Sampling points"),
                                 "Sampling time points",
                                 text[["m_timePoints"]],
                                 server = TRUE
        )

      if (loading_()) {
        loaded_(TRUE)
      }
      x
    })


  observe({
    hide(id = "m_timePoints_div")
    if (file.exists(sc_metadata_label_()) )
      shinyjs::show(id = "m_timePoints_div")
  })


  ## output[['sc_metadata_file']] <- renderText(parseFilePaths(roots = roots_dir, inputs[['sc_metadata_file']]()))
  output[['m_id_column_tab']] <-
    renderTable(head(sc_metadata_()[inputs[['m_id_column']]()]),
                bordered = TRUE,
                rownames = TRUE)
  output[['sc_metadata_file']] <-
    renderPrint(inputs[['sc_metadata_file']]())
  output[['m_time_column']] <-
    renderText(inputs[['m_time_column']]())
  output[['m_id_column']] <-
    renderText(inputs[['m_id_column']]())
  output[['m_time_points']] <-
    renderText(inputs[['m_time_points']]())
  ## #output[['m_id_column']] <- renderPrint(inputs[['m_id_column']]())

  ## #output$cellInfo <- renderTable({
  ## #  if ('root'%in%names(input$sc_metadata_file)){
  ## #    inFile <- parseFilePaths(roots = roots_dir[input$sc_metadata_file[['root']]], input$sc_metadata_file)
  ## #    if ( nrow(inFile)) {
  ## #      #cellInfo <- read.csv(as.character(inFile$datapath))
  ## #      cellInfo <- readRDS(as.character(inFile$datapath))
  ## #    }
  ## #  }
  ## #})

  outputOptions(output, "m_timePoints", suspendWhenHidden = FALSE)

  outs <- outputOptions(output)
  lapply(names(outs),
         function(name) {
           outputOptions(output,
                         name,
                         suspendWhenHidden = FALSE)
         })








  #rs_(callr::r_session$new(options = sopt, wait = T))
  #rs_()$supervise(T)

  # rp <- callr::r_bg(
  #   func = long_job,
  #   args = list("port"=port),
  #   supervise = TRUE
  #)

  observeEvent(input[["res_refresh"]], {
    #browser()
    returned_vals <- show_result(rs_())
    rs_(returned_vals$rs)
    inputs[["res_lastrefresh"]](returned_vals$message)
  })



  output[["res_lastrefresh"]] <- renderText(inputs[["res_lastrefresh"]]())


}

shinyApp(ui, server)
## return (shinyApp(ui, server))
## runApp(system.file("shiny", "tooltip_popover_modal", package = "bsplus"))

##}


### end of file -- app.R
