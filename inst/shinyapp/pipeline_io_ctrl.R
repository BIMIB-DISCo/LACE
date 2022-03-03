# library(tools)
# library(dplyr)
# library(tidyr)
# library(configr)

ANDx <- function(x, y) {
  z <- x & y
  z[is.na(x) | is.na(y)] <- NA
  return(z)
}

ORx <- function(x, y) {
  z <- x | y
  z[is.na(x) | is.na(y)] <- NA
  return(z)
}

compare_named_lists <- function(l1,l2) {
  if(is.list(l1) && is.list(l2)) { #if they are lists
    if (
      length(unique(names(l1))) == length(l1) &&
      length(unique(names(l2))) == length(l2) && !("" %in% names(l1)) && !("" %in% names(l2))
    ) { #if they are named
      st<-TRUE
      for (i in unique(c(names(l1), names(l2)))) {
        if (i %in% names(l1) && i %in% names(l2)) { #if the same name is present in both
          #print('----')
          #print(l1[[i]])
          #print(l2[[i]])
          if (is.list(l1[[i]]))
            st <- st && compare_named_lists(l1[[i]],l2[[i]])
          else
            st <- st && identical(l1[[i]],l2[[i]])
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

OneInOneOut <- function(InDir, InFilesMask, OutDir, OutExt, Opt) {

	InFiles = list.files(path = InDir, pattern = InFilesMask, no.. = T)
	AllOutFiles <- list.files(path = OutDir, no.. = T)


	InFilesToDo <- c()
	OutFilesToRm <- c()
	OutFilesToRep <- c()
	if ( length(InFiles) >0 ) {
    ActFileContent <- read.config(file = Opt$ActFile)
    StoredFileContent <- read.config(file = Opt$StoredFile)

    OutFiles <- paste0(file_path_sans_ext(InFiles),'.',OutExt)
    print('out')
    print(OutFiles)
    print(file.exists(file.path(OutDir,OutFiles)))
    ResidualOutFiles <- AllOutFiles[!AllOutFiles %in% OutFiles]

    EqFiles <- compare_named_lists(ActFileContent, StoredFileContent)

    if ( !EqFiles ) { # if conf files are non equal
      file.copy(Opt$ActFile, Opt$StoredFile, overwrite = T)
      InFilesToDo <- InFiles
      OutFilesToRm <- ResidualOutFiles
      OutFilesToRep <- OutFiles
    }
		else {
		  #OptStoredFileDateMax = max(file.mtime( c(Opt$StoredFile, Opt$ExtraFiles) ))
			#OptStoredFileDateMin = min(file.mtime( c(Opt$StoredFile, Opt$ExtraFiles) ))
		  if (!is.null(Opt$ExtraFiles))
		    OptExtraDate <- file.mtime(Opt$ExtraFiles)
		  else
		    OptExtraDate <- NULL
			OptStoredFileDateMax = max(c( min(file.mtime( c(Opt$StoredFile, Opt$ActFile))), OptExtraDate ))
			OptStoredFileDateMin = min(c( max(file.mtime( c(Opt$StoredFile, Opt$ActFile))), OptExtraDate ))

			InFilesDate <- file.mtime(file.path(InDir,InFiles))
			RightInFilesTime <- OptStoredFileDateMin > InFilesDate
			OutFilesDate <- file.mtime(file.path(OutDir,OutFiles))
			RightOutFilesTime <- OptStoredFileDateMax < OutFilesDate

			ToDo=!replace_na( ANDx( RightOutFilesTime, RightInFilesTime), F)
			ToRep=!replace_na( ANDx( RightOutFilesTime, RightInFilesTime), T)

			#----------
			print('InFiles -> OptStored -> OutFiles')
			dout <- data.frame(
			  list(
			    "InFiles"=InFiles,
			    BooolIn=RightInFilesTime,
			    InDate=InFilesDate, Rel='<',
			    OptStoredFileDateMin = OptStoredFileDateMin, Rel='<',

			    OptStoredFileDateMax = OptStoredFileDateMax, Rel='<',
			    OutDate = OutFilesDate, Rel='<',
			    BoolOut=RightOutFilesTime,
			    "OutFiles"=OutFiles,

			    "ToDo"=ToDo,
			    "ToRep"=ToRep
			  )
			)
			print(head(dout))
			#----------

			InFilesToDo <- InFiles[ToDo]
			OutFilesToRm <- ResidualOutFiles
			OutFilesToRep <- OutFiles[ToRep]
		}
	}
	print(c('DO',InFilesToDo))
	print(c('REP',OutFilesToRep))
	print(c('RM',OutFilesToRm))

	return(list('InFilesToDo'=InFilesToDo, 'OutFilesToRm'=OutFilesToRm, 'OutFilesToRep'=OutFilesToRep))
}


# c1 <- read.config(file = "conf1")
# c2 <- read.config(file = "conf2")
# Opt=list()
# Opt$StoredFile <- "conf1"
# Opt$ActFile <- "conf2"
# Opt$ExtraFiles=NULL
#
# OneInOneOut( "./in/", InFilesMask = ".*", "./out/", OutExt = "out", Opt = Opt)

ManyInSomeOut <- function(InDir, InFilesMask, OutDir, OutFiles, Opt) {
  InFiles = list.files(path = InDir, pattern = InFilesMask, no.. = T)
  AllOutFiles <- list.files(path = OutDir, no.. = T)

  InFilesToDo <- c()
  OutFilesToRm <- c()
  OutFilesToRep <- c()
  if ( length(InFiles) >0 ) {
    ActFileContent <- read.config(file = Opt$ActFile)
    StoredFileContent <- read.config(file = Opt$StoredFile)

    OutFiles <- file.path(OutDir,OutFiles)

    print('out')
    print(OutFiles)
    print(file.exists(OutFiles))
    ResidualOutFiles <- AllOutFiles[!AllOutFiles %in% OutFiles]

    EqFiles <- compare_named_lists(ActFileContent, StoredFileContent)

    if ( !EqFiles ) { # if conf files are non equal
      file.copy(Opt$ActFile, Opt$StoredFile, overwrite = T)
      InFilesToDo <- InFiles
      OutFilesToRm <- ResidualOutFiles
      OutFilesToRep <- OutFiles
    }
    else {
      if (!is.null(Opt$ExtraFiles))
        OptExtraDate <- file.mtime(Opt$ExtraFiles)
      else
        OptExtraDate <- NULL
      OptStoredFileDateMax = max(c( min(file.mtime( c(Opt$StoredFile, Opt$ActFile))), OptExtraDate ))
      OptStoredFileDateMin = min(c( max(file.mtime( c(Opt$StoredFile, Opt$ActFile))), OptExtraDate ))
      InFilesDate <- file.mtime(file.path(InDir,InFiles))
      RightInFilesTime <- OptStoredFileDateMin > InFilesDate
      OutFilesDate <- file.mtime(OutFiles)
      RightOutFilesTime <- OptStoredFileDateMax < OutFilesDate

      ToDo=any(!replace_na( c(all(RightOutFilesTime), all(RightInFilesTime)), F))
      #ToRep=!replace_na( ANDx( RightOutFilesTime, RightInFilesTime), T)

      #----------
      # print('InFiles -> OptStored -> OutFiles')
      # dout <- data.frame(
      #   list(
      #     "InFiles"=InFiles,
      #     BooolIn=RightInFilesTime,
      #     InDate=InFilesDate, Rel='<',
      #     OptStoredFileDateMin = OptStoredFileDateMin, Rel='<',
      #
      #     OptStoredFileDateMax = OptStoredFileDateMax, Rel='<',
      #     OutDate = OutFilesDate, Rel='<',
      #     BoolOut=RightOutFilesTime,
      #     "OutFiles"=OutFiles,
      #
      #     "ToDo"=ToDo,
      #     "ToRep"=ToRep
      #   )
      # )
      # print(head(dout))
      #----------
      if (ToDo) {
        InFilesToDo <- InFiles
        OutFilesToRm <- ResidualOutFiles
        OutFilesToRep <- OutFiles
      }
    }
  }
  print(c('DO',InFilesToDo))
  print(c('REP',OutFilesToRep))
  print(c('RM',OutFilesToRm))

  return(list('InFilesToDo'=InFilesToDo, 'OutFilesToRm'=OutFilesToRm, 'OutFilesToRep'=OutFilesToRep))
}


SomeInSomeOut <- function(InFiles, OutFiles, Opt) {
  AllOutFiles <- list.files(path = OutDir, no.. = T)

  InFilesToDo <- c()
  OutFilesToRm <- c()
  if ( length(InFiles) >0 ) {
    ActFileContent <- read.config(file = Opt$ActFile)
    StoredFileContent <- read.config(file = Opt$StoredFile)

    ResidualOutFiles <- AllOutFiles[!AllOutFiles %in% OutFiles]

    EqFiles <- compare_named_lists(ActFileContent, StoredFileContent)

    if ( !EqFiles ) { # if conf files are non equal
      file.copy(Opt$ActFile, Opt$StoredFile)
      InFilesToDo <- InFiles
      OutFilesToRm <- c(OutFiles[file.exists(OutFiles)], ResidualOutFiles)
    }
    else {
      #OptStoredFileDateMax = max(file.mtime( c(Opt$StoredFile, Opt$ExtraFiles) ))
      #OptStoredFileDateMin = min(file.mtime( c(Opt$StoredFile, Opt$ExtraFiles) ))
      if (!is.null(Opt$ExtraFiles))
        OptExtraDate <- file.mtime(Opt$ExtraFiles)
      else
        OptExtraDate <- NULL
      OptStoredFileDateMax = max(c( min(file.mtime( c(Opt$StoredFile, Opt$ActFile))), OptExtraDate ))
      OptStoredFileDateMin = min(c( max(file.mtime( c(Opt$StoredFile, Opt$ActFile))), OptExtraDate ))
      InFilesDate <- file.mtime(file.path(InDir,InFiles))
      RightInFilesTime <- OptStoredFileDateMin > InFilesDate
      OutFilesDate <- file.mtime(OutFiles)
      RightOutFilesTime <- OptStoredFileDateMax < OutFilesDate

      ToDo=any(!replace_na( ANDx( RightOutFilesTime, RightInFilesTime), F))
      ToRm=!replace_na( ANDx( RightOutFilesTime, RightInFilesTime), T)

      #----------
      print('InFiles -> OptStored -> OutFiles')
      dout <- data.frame(
        list(
          "InFiles"=InFiles,
          BooolIn=RightInFilesTime,
          InDate=InFilesDate, Rel='<',
          OptStoredFileDateMin = OptStoredFileDateMin, Rel='<',

          OptStoredFileDateMax = OptStoredFileDateMax, Rel='<',
          OutDate = OutFilesDate, Rel='<',
          BoolOut=RightOutFilesTime,
          "OutFiles"=OutFiles,

          "ToDo"=ToDo,
          "ToRm"=ToRm
        )
      )
      print(head(dout))
      #----------

      InFilesToDo <- InFiles
      OutFilesToRm <- c(OutFiles[ToRm], ResidualOutFiles)
    }
  }
  print(paste('DO',InFilesToDo))
  print(paste('remove',OutFilesToRm))
  return(list(InFilesToDo,OutFilesToRm))
}
