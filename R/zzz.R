.onLoad <- function(libname, pkgname){
  packageStartupMessage("Welcome to my package onload")
  appDir <- system.file("shinyapp", package = "LACE")
  if(stringr::str_length(appDir)>0) {
    if(!dir.exists(file.path(appDir, "Rambow_dataset")) || !dir.exists(file.path(appDir, "Small_dataset"))) {
      packageStartupMessage("Welcome to my package onattach")
      tmp=tempdir()
      list.files(tmp)
      
      url <- "https://github.com/BIMIB-DISCo/LACE/blob/devel_data/data/LACE_demos.zip?raw=true"
      filen=file.path(tmp,"LACE_demos1.zip")
      curl::curl_download(url, filen)
      
      url <- "https://github.com/BIMIB-DISCo/LACE/blob/devel_data/data/LACE_demos.z01?raw=true"
      filen=file.path(tmp,"LACE_demos1.z01")
      curl::curl_download(url, filen)
      
      url <- "https://github.com/BIMIB-DISCo/LACE/blob/devel_data/data/LACE_demos.z02?raw=true"
      filen=file.path(tmp,"LACE_demos1.z02")
      curl::curl_download(url, filen)
      
      filen=file.path(tmp,"LACE_demos1.zip")
      unsplit=file.path(tmp,"unsplit.zip")
      zip = Sys.which("zip")[[1]]
      system2(zip, args=c("-s 0", shQuote(filen), "-O", shQuote(unsplit)))
      
      #appDir <- system.file("shinyapp", package = "LACE")
      #if(stringr::str_length(appDir)>0) {
      #  #dir.create(appDir,showWarnings = F)
      unzip(unsplit, exdir=paste0(appDir))
      #}
    }
  }
}

.onAttach <- function(libname, pkgname) {
  
}











#zip2 <- function (zipfile, files, flags = "-r9X", extras = "", zip = Sys.getenv("R_ZIPCMD", "zip")) 
#{
#  if (missing(flags) && (!is.character(files) || !length(files))) 
#    stop("'files' must be a character vector specifying one or more filepaths")
#  if (!is.character(zip) || length(zip) != 1L || !nzchar(zip)) 
#    stop("argument 'zip' must be a non-empty character string")
#  args <- c(flags, shQuote(files), "-O", shQuote(path.expand(zipfile)))
#  print(args)
#  if (sum(nchar(c(args, Sys.getenv()))) + length(args) > 8000) {
#    args <- c(flags, shQuote(files), "-O", shQuote(path.expand(zipfile)))
#    input <- files
#  }
#  else input <- NULL
#  if (.Platform$OS.type == "windows") 
#    system2(zip, args, input = input, invisible = FALSE)
#  else invisible(system2(zip, args, input = input))
#}
        
#zip2("/media/disco/work_folder/Downloads/test_lace_files/cazzo/cazzo/unsplit.zip", files=c(paste0(tmp,"/LACE_demos1.zip")), flags = "-s 0", zip = Sys.which("zip")[[1]])
