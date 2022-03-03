### load_and_save.R


## Save data

catch_ui_names <- function(inp, grep_str) {
  return(inp[grep(grep_str, names(inp))])
}

doSave <- function(grep_str, config_path, config_file) {
  initialInputs_copy_tmp <- list()
  for (i in names(inputs))
    initialInputs_copy_tmp[[i]] <- inputs[[i]]()

  initialInputs_copy <- catch_ui_names(initialInputs_copy_tmp, grep_str)
  for (n in names(initialInputs_copy) )
    initialInputs_copy <-
    initialInputs_copy[names(initialInputs_copy) %in%
                         paste0(n, "-modal") == FALSE]

  if (dir.exists(config_path))
    write.config(config.dat = initialInputs_copy,
                 file.path = file.path(config_path, config_file),
                 write.type = "yaml")
}



save_ui_state <- function(grep_str, config_file) {
  initialInputs_copy_tmp <- list()
  for (i in names(inputs))
    initialInputs_copy_tmp[[i]] <- inputs[[i]]()
  initialInputs_copy <-  catch_ui_names(initialInputs_copy_tmp, grep_str)
  for ( n in names(initialInputs_copy) )
    initialInputs_copy <-
    initialInputs_copy[names(initialInputs_copy) %in%
                         paste0(n, "-modal") == FALSE]

  if (dir.exists(config_path))
    write.config(config.dat = initialInputs_copy,
                 file.path =
                   file.path(config_path, config_file),
                 write.type = "yaml")
}


## Load data
inf_doLoad_a <- function(config_path) {
  inf_loaded_input_(read.config(file = file.path(config_path,".config_06_inf.yml")))
  loaded_input <- inf_loaded_input_()
  for (id in names(loaded_input)) {
    value <- loaded_input[[id]]
    session$sendInputMessage(id, list(value = value))
  }
}


inf_doLoad_b <- function(i) {
  req(input)
  loaded_input <- inf_loaded_input_()
  if( i %in% names(loaded_input)) {
    if(!is.null(loaded_input[[i]])) {
      #if(!is.integer(loaded_input[[i]])) {
      print(loaded_input[[i]])
      #if (dir.exists(parseDirPath(roots=roots_dir, loaded_input[[i]])))
      inputs[[i]](loaded_input[[i]])
      #}
    }
  }
  else
    inputs[[i]](input[[i]])
}


inf_doLoad_c <- function() {
  req(input)
  loaded_input <- inf_loaded_input_()
  for(i in c("inf_alpha", "inf_beta")) {
    if( i %in% names(loaded_input)) {
      if(!is.null(loaded_input[[i]])) {
        print(loaded_input[[i]])
        inputs[[i]](loaded_input[[i]])
        proxy<-dataTableProxy(i)
        replaceData(proxy, (do.call(cbind, inputs[[i]]())) %>% {as.data.frame((.)[,-1], row.names = (.)[,1])})
      }
    }
    else {
      inputs[[i]](session$userData[["ed_table_react_list"]][[i]]$x$data)
    }
  }
}


va_doLoad_a <- function(config_path) {
  va_loaded_input_(read.config(file = file.path(config_path,".config_05_va.yml")))
  loaded_input <- va_loaded_input_()
  for (id in names(loaded_input)) {
    value <- loaded_input[[id]]
    session$sendInputMessage(id, list(value = value))
  }
}



va_doLoad_b <- function(i) {
  req(input)
  loaded_input <- va_loaded_input_()
  if( i %in% names(loaded_input)) {
    if(!is.null(loaded_input[[i]])) {
      #if(!is.integer(loaded_input[[i]])) {
      print(loaded_input[[i]])
      #if (dir.exists(parseDirPath(roots=roots_dir, loaded_input[[i]])))
      inputs[[i]](loaded_input[[i]])
      #}
    }
  }
  else
    inputs[[i]](input[[i]])
}



va_doLoad_c <- function() {
  req(input)
  loaded_input <- va_loaded_input_()
  if( 'va_verified_genes' %in% names(loaded_input)) {
    if(!is.null(loaded_input[['va_verified_genes']])) {
      #if(!is.integer(loaded_input[[i]])) {
      print(loaded_input[['va_verified_genes']])
      #if (dir.exists(parseDirPath(roots=roots_dir, loaded_input[[i]])))
      inputs[['va_verified_genes']](loaded_input[['va_verified_genes']])
      inputs[['va_list_genes']](loaded_input[['va_list_genes']])

      updateSelectizeInput(session, 'va_verified_genes', selected = inputs[['va_verified_genes']](), choices = inputs[['va_list_genes']](), server = TRUE)
      #}
    }
  }
  else
    inputs[['va_verified_genes']](input[['va_verified_genes']])
}



dp_doLoad_a <- function(config_path) {
  dp_loaded_input_(read.config(file = file.path(config_path,".config_04_dp.yml")))
  loaded_input <- dp_loaded_input_()
  for (id in names(loaded_input)) {
    value <- loaded_input[[id]]
    session$sendInputMessage(id, list(value = value))
  }
}


dp_doLoad_b <- function(i) {
  req(input)
  loaded_input <- dp_loaded_input_()
  if( i %in% names(loaded_input)) {
    if(!is.null(loaded_input[[i]])) {
      if(!is.integer(loaded_input[[i]])) {
        if (dir.exists(parseDirPath(roots=roots_dir, loaded_input[[i]])))
          inputs[[i]](loaded_input[[i]])
      }
    }
  }
  else
    inputs[[i]](input[[i]])
}

thr_doLoad_a <- function(config_path) {
  thr_loaded_input_(read.config(file = file.path(config_path,".config_03_thr.yml")))
  loaded_input <- thr_loaded_input_()
  for (id in names(loaded_input)) {
    value <- loaded_input[[id]]
    session$sendInputMessage(id, list(value = value))
  }
}


thr_doLoad_b <- function(i) {
  req(input)
  loaded_input <- thr_loaded_input_()
  if( i %in% names(loaded_input)) {
    if(!is.null(loaded_input[[i]])) {
      #if(!is.integer(loaded_input[[i]])) {
      #print(loaded_input[[i]])
      #if (dir.exists(parseDirPath(roots=roots_dir, loaded_input[[i]])))
      inputs[[i]](loaded_input[[i]])
      #}
    }
  }
  else
    inputs[[i]](input[[i]])
}


av_doLoad_a <- function(config_path) {
  av_loaded_input_(read.config(file = file.path(config_path,".config_02_av.yml")))
  loaded_input <- av_loaded_input_()
  for (id in names(loaded_input)) {
    value <- loaded_input[[id]]
    session$sendInputMessage(id, list(value = value))
  }
}


av_doLoad_b <- function(i) {
  req(input)
  loaded_input <- av_loaded_input_()
  if( i %in% names(loaded_input)) {
    if(!is.null(loaded_input[[i]])) {
      if(!is.integer(loaded_input[[i]])) { #necessary for directories
        if (dir.exists(parseDirPath(roots=roots_dir, loaded_input[[i]])))
          inputs[[i]](loaded_input[[i]])
        else
          showNotification(paste(parseDirPath(roots=roots_dir, loaded_input[[i]]),'does not exist'), duration = 10, type = "warning")
      }
    }
  }
  else
    inputs[[i]](input[[i]])
}


m_doLoad_a <- function(config_path) {
  m_loaded_input_(read.config(file = file.path(config_path,".config_01_m.yml")))
  loaded_input <- m_loaded_input_()
  #print(loaded_input)
  # for (id in names(loaded_input)) {
  #  #for (id in names(initialInputs)) {
  #   value <- loaded_input[[id]]
  #   session$sendInputMessage(id, list(value = value))
  # }
}



m_doLoad_b <- function() {
  #req(input)
  loading_(T)
  inputs[['sc_metadata_file']](NULL)
  loaded_input <- isolate(m_loaded_input_())
  if (!is.null(loaded_input[['sc_metadata_file']])) {
    if (!is.integer(loaded_input[['sc_metadata_file']])) {
      ## print(loaded_input[['sc_metadata_file']])
      if (file.exists(as.character(parseFilePaths(roots = roots_dir,
                                                  loaded_input[['sc_metadata_file']])[['datapath']])))
        inputs[['sc_metadata_file']](loaded_input[['sc_metadata_file']])
    }
  }
  ## else
  ##  inputs[['sc_metadata_file']](isolate(input[['sc_metadata_file']]))

  ###
  sc_metadata_label_(set_file_ui('sc_metadata_file'))

  if (file.exists(sc_metadata_label_())) {
    if (file_ext(sc_metadata_label_())  %in% c("rds", "RDS") ) {
      cellInfo <- readRDS( sc_metadata_label_() )
      if (!is.data.frame(cellInfo)){
        showNotification("No metadata file", type = "warning", duration = 10)
        cellInfo <- data.frame()
      }
    }
    else
      cellInfo <- fread(sc_metadata_label_(),
                        header = TRUE,
                        data.table = FALSE)
  } else {
    showNotification("No metadata file", type = "warning", duration = 10)
    cellInfo <- data.frame()
  }
  sc_metadata_(cellInfo)

  ## if ( file.exists(sc_metadata_label_()) ) {
  choices_(colnames(sc_metadata_()))
  ## showNotification(paste("input[['sc_metadata_file']]","choises_)"), duration = NULL)
  ## }
  ## else
  ##  choices_(c(""))
  ###


  inputs[['m_id_column']](NULL)
  ## showNotification(paste(inputs[['m_time_column']]()), duration = 10)
  loaded_input <- isolate(m_loaded_input_())
  if (!is.null(loaded_input[['m_id_column']]))
    inputs[['m_id_column']](loaded_input[['m_id_column']])
  ## else
  ##  inputs[['m_time_column']](isolate(input[['m_time_column']]))

  inputs[['m_time_column']](NULL)
  ## showNotification(paste(inputs[['m_time_column']]()), duration = 10)
  loaded_input <- isolate(m_loaded_input_())
  if (!is.null(loaded_input[['m_time_column']]))
    inputs[['m_time_column']](loaded_input[['m_time_column']])
  ## else
  ##  inputs[['m_time_column']](isolate(input[['m_time_column']]))


  inputs[['m_time_points']](NULL)
  ## loaded_input <- isolate(m_loaded_input_())
  if (!is.null(loaded_input[['m_time_points']]))
    inputs[['m_time_points']](loaded_input[['m_time_points']])
  ## else
  ##  inputs[['m_time_points']](isolate(input[['m_time_points']]))
  ## showNotification(paste("loaded", "time points"), duration = NULL)
  ## showNotification(paste(inputs[['m_time_column']]()), duration = NULL)

  loading_(F)
}

## Load project .config.yml configs

inputs[["load_all_configs"]] <- reactiveVal(F)

load_all_configs <- function(project_folder,
                             default_config_dir = NULL,
                             config_dir = os_conf_subdir,
                             default = F,
                             recent = F) {


  inputs[["load_all_configs"]](F)

  ## Change to project folder
  setwd(inputs[["project_folder_std"]]())

  config_path <- file.path(inputs[["project_folder_std"]](), os_conf_subdir)

  m_doLoad_a(config_path)
  m_doLoad_b()

  av_doLoad_a(config_path)
  uis <- av_dir_uis
  for (i in av_dir_uis) {
    av_doLoad_b(i)
  }
  thr_doLoad_a(config_path)
  uis <- thr_uis
  for(i in thr_uis) {
    thr_doLoad_b(i)
  }
  dp_doLoad_a(config_path)
  uis <- dp_uis
  for(i in dp_uis) {
    dp_doLoad_b(i)
  }

  va_doLoad_a(config_path) #
  uis <- va_uis
  for(i in va_uis) {
    va_doLoad_b(i)

  }
  va_doLoad_c()

  inf_doLoad_a(config_path)
  uis <- inf_uis
  for(i in uis) {
    inf_doLoad_b(i)
  }
  for(i in c("inf_alpha", "inf_beta")) {
    inf_doLoad_b(i)
  }
  #inf_doLoad_c() # now is called below

  inputs[["load_all_configs"]](T)

  showNotification(paste("config files:", "loaded"),
                   duration = 10)

  return(inputs[["load_all_configs"]]())

}

observeEvent(inputs[["load_all_configs"]](),{
  if(inputs[["load_all_configs"]]()) {
    inf_doLoad_c()
    for(i in c("inf_alpha", "inf_beta")) {
      if( i %in% names(inf_loaded_input_())) {
        x <- (do.call(cbind, inputs[[i]]())) %>% {as.data.frame((.)[,-1], row.names = (.)[,1])}
        x <- data.frame(rownames(x), x, check.names=F)
        colnames(x)[1]=""
        session$userData[["ed_table_react_list_obj"]]$obj[[i]]$x$data <- x
        session$userData[["ed_table_react_list"]][[i]]$x$data <- x
        print(session$userData[["ed_table_react_list"]][[i]]$x$data)
      }
    }
  }
})

load_all_configs2 <- function(project_folder,
                              default_config_dir = NULL,
                              config_dir = os_conf_subdir,
                              default = F,
                              recent = F) {

  config_path <- NULL
  if (default)
    config_path <- default_config_dir
  else
    config_path <- file.path(project_folder, config_dir)

  file_default_config_list <- NULL
  if (!recent)
    pattern <- "^\\..*"
  else
    pattern <- ".*"

  if (dir.exists(config_path))
    file_config_list <- list.files(config_path,
                                   pattern = pattern,
                                   all.files = T,
                                   full.names=F,
                                   no..= T )

  file_config_list <- file.path(config_path, file_config_list)


  res <- T
  for (cf in file_config_list) {
    loaded_input <- read.config(file = cf)
    if (basename(cf) == ".config_01_m.yml") {
      ## load_meta(loaded_input)
      m_doLoad_a(config_path)
      m_doLoad_b()
    } else {
      if (is.list(loaded_input))
        for (id in names(loaded_input)) {
          value <- loaded_input[[id]]
          ## session$sendInputMessage(id, list(value = value))

          check <- T
          runjs(getWidgetType(id))

          ## print('----------->>')
          ## print(id)
          ## print((input[[paste0('inputType_',id)]]))

          if (!is.null(input[[paste0('inputType_',id)]])) {
            if (!input[[paste0('inputType_',id)]] %in% c("None")) {
              ## print('-------------')
              ## print(id)
              ## print(input[[paste0('inputType_',id)]])
              if (input[[paste0('inputType_',id)]] %in%
                  c("shinyDirectories", "shinyFiles"))
                check <- dir.exists(parseDirPath(roots=roots_dir, loaded_input[[id]]))
              else if (input[[paste0('inputType_',id)]] %in% c("select")) {
                check <- F
              }
              else if (input[[paste0('inputType_',id)]] %in% c("number")) {
                check <- T

              }
              else if (input[[paste0('inputType_',id)]] %in% c("shiny-text-output")) {
                check <- F

              }
              else if (input[[paste0('inputType_',id)]] %in% c("selectized")) {
                check <- T
              }

              if (id %in% c('thr_accepted_var', 'thr_negleted_var')) {
                check <- T
                value <- loaded_input[[id]]
                inputs[[id]](loaded_input[[id]])
                session$sendInputMessage(id, list(value = value))
              }

              if (id=="va_list_genes") {
                check <- F
                updateSelectizeInput(session,
                                     'va_verified_genes',
                                     choices =
                                       inputs[['va_list_genes']](),
                                     server = TRUE)
              }
              if (id=="va_verified_genes") {
                check <- F
                updateSelectizeInput(session,
                                     'va_verified_genes',
                                     selected =
                                       inputs[['va_verified_genes']](),
                                     server = TRUE)
              }

              if (check)
                inputs[[id]](loaded_input[[id]])
              else {
                if (input[[paste0('inputType_',id)]] %in%
                    c("shinyDirectories", "shinyFiles"))
                  showNotification(paste(parseDirPath(roots = roots_dir,
                                                      loaded_input[[id]]),
                                         'does not exist'),
                                   duration = 10,
                                   type = "warning")
                else
                  showNotification(paste(id, 'does not exist'),
                                   duration = 10,
                                   type = "warning")
              }
            } else {
              ## print('-------------')
              ## print(id)
              ## print('>>>>>')
            }
          }
        }
      else
        res <- F
    }
  }
  Load_done <- showNotification(paste("config files:", "loaded"),
                                duration = 10)
  return(res)
}


### end of file -- load_and_save.R
