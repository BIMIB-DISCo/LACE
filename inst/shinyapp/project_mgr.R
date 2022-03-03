### project_mgr.R
###

## Create the user os default config folder
create_default_config_dir_std <- function() {
  if (dir.exists(path_home()))
    if (dir.exists(file.path(path_home(), os_conf[[.Platform$OS.type]]) )){
      if (!dir.exists(file.path(path_home(), os_conf[[.Platform$OS.type]], "LACE")))
        dir.create(file.path(path_home(), os_conf[[.Platform$OS.type]], "LACE") )
      if (dir.exists(file.path(path_home(), os_conf[[.Platform$OS.type]], "LACE") ))
        return(file.path(path_home(), os_conf[[.Platform$OS.type]], "LACE"))
    }
  return(NULL)
  ## logout("impossible to create default configuration folder")
}

## write the .config.yml in the user os default config folder
write_default_yaml <- function(default_config_dir) {
  res <- T
  if (dir.exists(default_config_dir))
    for (y in names(default_yaml)) {
      if (!file.exists(file.path(default_config_dir, y))) {
        write.config(config.dat = default_yaml[[y]],
                     file.path =
                       file.path(default_config_dir, y),
                     write.type = "yaml")
        res <- res & file.exists(file.path(default_config_dir, y))
      }
    }
  else
    res <- F
  return(res)
}

## Call create_default_config_dir_std and write_default_yaml
create_default_config <- function() {
  default_config_dir_std <- create_default_config_dir_std()
  if (!is.null(default_config_dir_std)) {
    if (!write_default_yaml(default_config_dir_std)) {
      ## logout(paste("Not all config files created"))
    }
  } else {
    ## logout(paste("No default configuration folder", default_config_dir_std))
  }
  return(default_config_dir_std)
}


## Copy default yml in the current project config folder
copy_default_configs <- function(project_folder,
                                 default_config_dir,
                                 config_dir = os_conf_subdir,
                                 seletor = NULL) {
  if (dir.exists(default_config_dir)) {
    file_default_config_list <-
      list.files(default_config_dir, all.files = T, full.names=T, no..= T )
    if (dir.exists(project_folder))
      if (dir.exists(file.path(project_folder,config_dir))) {
        res <- file.copy(file_default_config_list,
                         file.path(project_folder,
                                   config_dir),
                         overwrite = F)
        if (all(res))
          return(T)
      }
  }
  return(F)
}

observeEvent(reactiveValuesToList(input), {
  outs <- outputOptions(output)
  lapply(names(outs), function(name) {
    outputOptions(output, name, suspendWhenHidden = FALSE)
  })
},
once = T,
priority = -2)



## Create project folder, copy default configs and load them
create_project <- function(project_folder,
                           default_config_dir,
                           config_dir=os_conf_subdir) {
  if (dir.exists(default_config_dir))
    if (dir.exists(project_folder))
      if (!dir.exists(file.path(project_folder, config_dir))) {
        dir.create(file.path(project_folder, config_dir))
        if (dir.exists(file.path(project_folder, config_dir))) {
          res_tmp <- copy_default_configs(project_folder,
                                      default_config_dir,
                                      config_dir) #overwrite=F
          res <- res_tmp
          res_tmp <- load_all_configs(project_folder,
                                        default_config_dir,
                                        config_dir,
                                        default = F,
                                        recent = F)
          res <- res & res_tmp
          return(res)
        }
      }
  return(F)
}


## Load project configs
load_project <- function(project_folder,
                         config_dir = os_conf_subdir) {
  if (dir.exists(project_folder))
    if (dir.exists(file.path(project_folder, config_dir))) {
      load_all_configs(project_folder, config_dir)
      return(T)
    }
  return(F)
}

## Make .config.yml in user default .config.yml
write_default_configs <-
  function(project_folder,
           default_config_dir,
           config_dir=os_conf_subdir) {
    if (dir.exists(default_config_dir))
      if (dir.exists(project_folder))
        if (dir.exists(file.path(project_folder, config_dir))) {
          file_config_list <-
            list.files(file.path(project_folder,
                                 config_dir),
                       pattern = "^\\..*",
                       full.names = F,
                       no..= T )
          file_config_list <- file.path(project_folder,
                                        file_config_list)
          return(file.copy(file_config_list,
                           default_config_dir))
        }
    return(F)
  }


## Make cofig.yml in project default .config.yml
write_configs <-
  function(project_folder,
           default_config_dir,
           config_dir = os_conf_subdir) {
    if (dir.exists(default_config_dir))
      if (dir.exists(project_folder))
        if (dir.exists(file.path(project_folder, config_dir))) {
          file_config_list <-
            list.files(file.path(project_folder,
                                 config_dir),
                       pattern = "^[^\\.].*",
                       full.names = F,
                       no..= T )
          #file_config_list <- substring(file_config_list,2)
          file_config_list <- paste0(".", file_config_list)
          file_config_list <- file.path(project_folder,file_config_list)
          return(file.copy(file_config_list, default_config_dir))
        }
    return(F)
  } # should exist tab specific write_default_config ?

### end of file -- project_mgr.R
