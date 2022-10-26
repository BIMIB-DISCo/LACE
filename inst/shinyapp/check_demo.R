is.zip <- function(filepath){
  result <- tryCatch({
    unzip(filepath, list = TRUE)
    return(TRUE)
  }, error = function(e){
    return(FALSE)
  })
  return(result)
}



download_demo <- function() {
  bStatus <- FALSE
  
  #check if demo already downloaded
  appDir <- system.file("shinyapp", package = "LACE")
  if(stringr::str_length(appDir)>0) {
    if(!dir.exists(file.path(appDir, "Rambow_dataset")) || !dir.exists(file.path(appDir, "Small_dataset"))) {
      bStatus <- FALSE
      
      tmp=tempdir()
      log2_print(tmp, msg = "download_demo: Rambow tmp folder =")
      list.files(tmp)
      
      #download repository
      url <- "https://github.com/gian-asco/LACE-DEMO/archive/refs/tags/1.0.0.zip"
      filen <- file.path(tmp,"LACE_demos.zip")
      tryCatch({
        if (curl::has_internet()) {
          #httr::GET(url, httr::write_disk(filen, overwrite=TRUE), httr::timeout(10))
          file_path <- curl::curl_fetch_disk(url, filen)
          log2_print(file_path, msg = "download_demo: downloading dataset url =")
        }
        #return(TRUE)
      }, error = function(e){
        log2_print("Download error", msg = "download_demo:")
        #return(FALSE)
      })
      
      
      #check file
      if(file.exists(filen)) 
        bStatus <- is.zip(filen)
      else
        bStatus <- FALSE
      
      if(bStatus)
      {
        unzip(filen, exdir=tmp)
        file.copy(from = file.path(tmp, "LACE-DEMO-1.0.0", "Rambow_dataset"), to = appDir, overwrite = TRUE, recursive = TRUE)
        file.copy(from = file.path(tmp, "LACE-DEMO-1.0.0", "Small_dataset"), to = appDir, overwrite = TRUE, recursive = TRUE)
      }
    }
    else
      bStatus <- TRUE
  }
  
  
  return(bStatus)
}

check_demo <- function() {
  bStatus <- FALSE
  appDir <- system.file("shinyapp", package = "LACE")
  if(stringr::str_length(appDir)>0) {
    if(!dir.exists(file.path(appDir, "Rambow_dataset")) || !dir.exists(file.path(appDir, "Small_dataset"))) {
      bStatus <- download_demo()
    }
    else {
      bStatus <- TRUE
      log2_print("demo already downloaded", msg = "check_demo:")
    }
  }
  
  return(bStatus) 
}
  

#if(!check_demo())
#  #print warning and do not proceede
#else
#  #proceed

