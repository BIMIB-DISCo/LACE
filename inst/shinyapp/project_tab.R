### project_tab.R

### Project reactiveVals ####

inputs[["default_config_dir_std"]] <-
  reactiveVal(create_default_config()) # each time check to create default conf
inputs[["config_dir_std"]] <- reactiveVal(os_conf_subdir)
inputs[["project_folder_std"]] <- reactiveVal()
inputs[["project_folder"]] <- reactiveVal()
inputs[["project_loaded"]] <- reactiveVal(FALSE)


inputs[["pr_folder"]] <- reactiveVal()
inputs[["pr_folder_std"]] <- reactiveVal(.my_actual_wd)
inputs[["pr_name"]] <- reactiveVal()
inputs[["pr_name_std"]] <- reactiveVal()
inputs[["pr_path"]] <- reactiveVal(.my_actual_wd)


inputs[['av_vcf_out_dir']] <- reactiveVal()
inputs[['thr_vcf_in_dir']] <- reactiveVal()
inputs[['thr_out_dir']] <- reactiveVal()
inputs[['dp_out_dir']] <- reactiveVal()
inputs[['va_out_dir']] <- reactiveVal()
inputs[['inf_out_dir']] <- reactiveVal()

### End project reactiveVals ####

### Project functions ####

toggle_inputs <- function(enable_inputs=TRUE) {
  input_list <- reactiveValuesToList(input)
  
  dp_input <- catch_ui_names(input_list, dp_grep_str)
  av_input <- catch_ui_names(input_list, av_grep_str)
  thr_input <- catch_ui_names(input_list, thr_grep_str)
  m_input <- catch_ui_names(input_list, m_grep_str)
  input_list <- c(m_input, av_input, thr_input, dp_input)
  #browser()
  print("enable_inputs")
  print(enable_inputs)
  # Toggle elements
  for(x in names(input_list))
    if(enable_inputs){
      shinyjs::enable(x)
    } else {
      shinyjs::disable(x) 
    }
}

disable_demo_tabs <- function() {
  
  enable_inputs <- FALSE
  
  #browser()
  
  input_list <- reactiveValuesToList(input)
  
  #if(only_buttons){
  #  buttons <- which(sapply(input_list,function(x) {any(grepl('Button',attr(x,"class")))}))
  #  input_list = input_list[buttons]
  #}
  
  dp_input <- catch_ui_names(input_list, dp_grep_str)
  av_input <- catch_ui_names(input_list, av_grep_str)
  thr_input <- catch_ui_names(input_list, thr_grep_str)
  m_input <- catch_ui_names(input_list, m_grep_str)
  
  #thr_grep_str= "^thr_|^`thr_|_thr_"
  #dp_grep_str= "^dp_|^`dp_|_dp_"
  #va_grep_str= "^va_|^`va_|_va_"
  #inf_grep_str= "^inf_|^`inf_|_inf_"
  #m_grep_str= "^m_|^`m_|_m_|sc"
  
  
  
  
  input_list <- c(m_input, av_input, thr_input, dp_input)
  
  #which(sapply(input_list,function(x) {any(grepl('Button',attr(x,"class")))}))
  
  # Toggle elements
  for(x in names(input_list))
    if(enable_inputs){
      shinyjs::enable(x)} else {
        shinyjs::disable(x) }
  
  
}

enable_demo_tabs <- function() {}
  
### End project functions ####

### Project observes ####

observeEvent(input[["pr_name"]], {
  pr_name <- input[["pr_name"]]
  pr_name_ <- inputs[["pr_name"]]
  pr_folder_std_ <- inputs[["pr_folder_std"]]
  pr_name_std_ <- inputs[["pr_name_std"]]
  pr_path_ <- inputs[["pr_path"]]

  pr_name_(pr_name)

  name <- str_replace_all(pr_name,
                          pattern = "[^A-Za-z0-9\\-+_]+",
                          replacement = "_")
  pr_name_std_(name)

  if (str_length(pr_name_std_()) > 0)
    pr_path_(file.path(pr_folder_std_(),
                       pr_name_std_(),
                       ""))
  else
    pr_path_(file.path(pr_folder_std_(), pr_name_std_()))
})


## Original.
## observeEvent(input[["pr_name"]], {
##     inputs[["pr_name"]](input[["pr_name"]])
##
##     name <- str_replace_all(input[["pr_name"]],
##                             pattern = "[^A-Za-z0-9\\-+_]+",
##                             replacement = "_")
##     inputs[["pr_name_std"]](name)
##
##     if (str_length(inputs[["pr_name_std"]]()) > 0)
##         inputs[["pr_path"]](file.path(inputs[["pr_folder_std"]](),
##                                       inputs[["pr_name_std"]](),
##                                       ""))
##         else # MA: Something is fishy here: incorrect indentation...
##             inputs[["pr_path"]](file.path(inputs[["pr_folder_std"]](),
##                                           inputs[["pr_name_std"]]()))
## })

observeEvent(input[["pr_folder"]],{
  inputs[["pr_folder"]](input[["pr_folder"]])
  inputs[["pr_folder_std"]](
    normalizePath(
      parseDirPath(roots = roots_dir,
                   inputs[["pr_folder"]]()),
      mustWork = FALSE
    )
  )

  if (str_length(inputs[["pr_name_std"]]()) > 0)
    inputs[["pr_path"]](file.path(inputs[["pr_folder_std"]](), inputs[["pr_name_std"]](), ""))
  else
    inputs[["pr_path"]](file.path(inputs[["pr_folder_std"]](), inputs[["pr_name_std"]]()))

  updateActionButton(session,
                     'pr_folder',
                     label = normalizePath(set_dir_ui("pr_folder"), mustWork = FALSE))
})

observeEvent(inputs[["pr_path"]](), {
  inputs[["project_loaded"]](FALSE)


  hide(id = "pr_path_div")
  if (!is.null(inputs[["pr_path"]]()))
    if (length(inputs[["pr_path"]]()) > 0) {
      shinyjs::show(id="pr_path_div")

      if (dir.exists(file.path(inputs[["pr_path"]](),os_conf_subdir))) {
        ## if (!load_project(inputs[["pr_path"]](), config_dir="config")) {
        ## warning()
        ## }
        ## else {
        path <- inputs[["pr_path"]]()
        inputs[["pr_folder_std"]](dirname(path))
        inputs[["pr_name"]](basename(path))
        inputs[["pr_name_std"]](basename(path))
        inputs[["project_folder_std"]](path)
        project_folder <- list( "path"="", "root"="")
        project_folder$path[[1]]<-""
        project_folder$path <-
          c(project_folder$path,
            as.list(path_split(path_rel(inputs[["project_folder_std"]](),
                                        start = roots_dir[[".."]]))[[1]]))
        project_folder$root=".."
        inputs[["project_folder"]](project_folder)

        dir <- out_subfolder_compute(inputs[['project_folder']](),'vcf_out')
        inputs[['av_vcf_out_dir']](dir)
        inputs[['thr_vcf_in_dir']](inputs[['av_vcf_out_dir']]())

        dir <- out_subfolder_compute(inputs[['project_folder']](),'filtered_vcf')
        inputs[['thr_out_dir']](dir)

        dir <- out_subfolder_compute(inputs[['project_folder']](),'sam_out')
        inputs[['dp_out_dir']](dir)

        dir  <- out_subfolder_compute(inputs[['project_folder']](), 'filtered_var')
        inputs[['va_out_dir']](dir)

        updateActionButton(session,"pr_folder", label=inputs[["pr_folder_std"]]())
        updateTextInput(session,"pr_name", value=inputs[["pr_name_std"]]())
        updateActionButton(session,"pr_next", label="Load project")
        ## }
      } else
        updateActionButton(session,"pr_next", label="Create project")
    }
  #browser()
  if (!is.null(inputs[["demo"]]())) {
    if (inputs[["demo"]]()>=1) {
      inputs[["demo"]](0)

      delay(1000, click("pr_next"))
      #click("pr_next")

      #hide_tab()
      delay(1500, {
        returned_vals <- show_result(rs_())
        rs_(returned_vals$rs)
      })
    }
  }

  #browser()
  if (!is.null(inputs[["reload_project"]]())) {
    #browser()
    if( inputs[["reload_project"]]()>=1) {
      inputs[["reload_project"]](0)

      delay(1000, click("pr_next"))
      #click("pr_next")

      #hide_tab()
      delay(1500, {
        returned_vals <- show_result(rs_())
        rs_(returned_vals$rs)
        #
      })

    }
  }

}, ignoreInit = TRUE)

observeEvent(inputs[["project_loaded"]](), {
  if (!is.null(inputs[["project_loaded"]]()))
    if (inputs[["project_loaded"]]()) {
      shinyjs::hide(id = "pr_next")
      
      print('disable tabs--------------------------------------------------------------------------')
      print(inputs[["demo"]]())
      toggle_inputs(is.null(inputs[["demo"]]()))
      #browser()

      a_sam <- Sys.which("samtools")
      famous_sam <- list()
      famous_sam$path <- c("",path_split(path_rel(dirname(a_sam[[1]]), start = roots_dir[[".."]]))[[1]])
      famous_sam$root <- roots_dir[[".."]]
      if(str_length(a_sam[[1]])>1)
        inputs[["dp_samtools_exec_dir"]](famous_sam)

      a_tool <- Sys.which("annotate_variation.pl")
      famous_tool <- list()
      famous_tool$path <- c("",path_split(path_rel(dirname(a_tool[[1]]), start = roots_dir[[".."]]))[[1]])
      famous_tool$root <- roots_dir[[".."]]
      if(str_length(a_tool[[1]])>1)
        inputs[["av_anovar_exec_dir"]](famous_tool)

      a_neckLACE <- Sys.which("perl")
      if(str_length(a_neckLACE[[1]])==0)
        showNotification("No perl found",
                         duration = 10,
                         type = "error")
    }
  else
  {
    #browser()
    shinyjs::show(id = "pr_next")
    
    if (!is.null(inputs[["demo_remainder"]]())) {
      if (dir.exists(inputs[["demo_remainder"]]())) {
        #dir_delete(inputs[["demo_remainder"]]())
      }
    }
  }
})

observeEvent(input[["pr_next"]], {
  #browser()
  warning_wd <- FALSE
  if(length(inputs[["pr_path"]]()) == 0)
    warning_wd <-TRUE
  else {
    if (dir.exists(inputs[["pr_path"]]())) {
      if (normalizePath(inputs[["pr_path"]]()) == .my_actual_wd) {
        ## pattume non aspetta per la risposta
        ## shinyalert(
        ##  paste("You experiment will be you current working folder,", getwd(), ". Create expriment?"), type = "warning",
        ##  immediate=F)
        ## warning_wd <- input$shinyalert
        print(warning_wd)
        warning_wd <- FALSE
      }
    } else
      warning_wd <- FALSE
  }
  if (!warning_wd) {
    dir.create(inputs[["pr_path"]](), showWarnings = FALSE)
    if (dir.exists(inputs[["pr_path"]]())) {
      inputs[["project_folder_std"]](inputs[["pr_path"]]())
      if (!dir.exists(file.path(inputs[["pr_path"]](),os_conf_subdir))) {
        if (!create_project(inputs[["project_folder_std"]](),
                            inputs[["default_config_dir_std"]]())) {
          showNotification(paste(paste("impossible to create project",
                                       inputs[["pr_path"]]())),
                           duration = 10,
                           type = "error")
        } else {
          inputs[["project_loaded"]](TRUE)
          #showTab(inputId = "main_tabset", target = "SC metadata")
        }
      } else {
        if (!load_project(inputs[["project_folder_std"]]())) {
          showNotification(paste(paste("impossible to load project",
                                       inputs[["pr_path"]]())),
                           duration = 10,
                           type = "error")
          showNotification(paste(paste("check config files")),
                           duration = 10,
                           type = "warning")
        } else {
          inputs[["project_loaded"]](TRUE)
          #showTab(inputId = "main_tabset", target = "SC metadata")
        }
      }


      #browser()
      projs <- update_proj_list(inputs[["pr_path"]]())

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

      submenus <- lapply(seq_along(projs),
                         FUN = function(x) {createmenuitem(x,projs)})

      recent_projects <- menuItem("Recent Projects", submenus, icon = icon("history"))

      output$recent_projects <- renderMenu({recent_projects})


      ## Change to project folder
      setwd(inputs[["project_folder_std"]]())

      project_folder <- list( "path"="", "root"="")
      project_folder$path[[1]]<-""
      project_folder$path <-
        c(project_folder$path,
          as.list(path_split(path_rel(inputs[["project_folder_std"]](),
                                      start = roots_dir[[".."]]))[[1]]))
      project_folder$root=".."
      inputs[["project_folder"]](project_folder)

      dir <- out_subfolder_compute(inputs[['project_folder']](), 'vcf_out')
      inputs[['av_vcf_out_dir']](dir)
      inputs[['thr_vcf_in_dir']](inputs[['av_vcf_out_dir']]())

      dir <- out_subfolder_compute(inputs[['project_folder']](), 'filtered_vcf')
      inputs[['thr_out_dir']](dir)

      dir <- out_subfolder_compute(inputs[['project_folder']](), 'sam_out')
      inputs[['dp_out_dir']](dir)

      dir  <- out_subfolder_compute(inputs[['project_folder']](), 'filtered_var')
      inputs[['va_out_dir']](dir)
    }
  }
  
  #browser()
  if (!is.null(inputs[["demo"]]())) {
    if (inputs[["demo"]]() == -1)
    {
      inputs[["demo"]](NULL)
    }
    else if (inputs[["demo"]]() == 0)
      inputs[["demo"]](-1)
  }
})

### End project observes ####


shinyDirChoose(input,
               "pr_folder",
               roots = roots_dir,
               filetypes = c('', 'txt'),
               defaultRoot = "..")

updateActionButton(session,
                   'pr_folder',
                   label = .my_actual_wd)


### Project outputs ####

output[["pr_path"]] <- renderText(inputs[["pr_path"]]())
output[["pr_title_ui"]] <- renderUI({
  tags$h3(paste("Project:", inputs[["pr_name"]]() ))
})

output[["pr_title_ui_h"]] <- renderUI({
  x <- ""
  if (str_length(inputs[["pr_name"]]()) > 0)
    x <- paste("Project: ", inputs[["pr_name"]]() )
  tags$b(x)
})

output[["pr_path_ui"]] <- renderUI({
  tagList(br(),
          tags$b("Project pathname"),
          verbatimTextOutput("pr_path"),
          br()
  )
})

### End project outputs ####


### end of file -- project_tab.R
