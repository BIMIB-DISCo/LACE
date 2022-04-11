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
      print(tmp)
      list.files(tmp)
      
      #download repository
      url <- "https://github.com/gian-asco/LACE-DEMO/archive/refs/tags/1.0.0.zip"
      filen <- file.path(tmp,"LACE_demos.zip")
      tryCatch({
        if (curl::has_internet()) {
          #httr::GET(url, httr::write_disk(filen, overwrite=TRUE), httr::timeout(10))
          file_path <- curl::curl_fetch_disk(url, filen)
          print(file_path)
        }
        #return(TRUE)
      }, error = function(e){
        print("Download error")
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
      print("demo already downloaded")
    }
  }
  
  return(bStatus) 
}
  

#if(!check_demo())
#  #print warning and do not proceede
#else
#  #proceed

