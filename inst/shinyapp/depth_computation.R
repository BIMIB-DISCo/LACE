### depth_computation.R
###

  dp_compute <- function(cellInfo,
                         bam_dir,
                         samtools,
                         data_dir,
                         out_dir,
                         id_column,
                         InFilesToDo = NULL,
                         OutFilesToRm = NULL) {

    ## ################################################################
    ## GET DEPTH FOR EACH SELECTED MUTATION FROM BAMs #####
    ## ################################################################

    dir.create(out_dir, showWarnings = FALSE)
    if (!dir.exists(out_dir))
      showNotification(paste("Filtered data output folder,",
                             out_dir,
                             ", cannot be created"),
                       duration = 10,
                       type = "warning")
    log2_print(data_dir)
    # load data
    snpMut_filt_freq <-
      readRDS(file = paste0(file.path(data_dir, 'snpMut_filt_freq.rds')))

    listMut <-
      strsplit(snpMut_filt_freq$UIDsnp, "_")
    snpMut_filt_freq$Mut_pos <-
      unlist(lapply(listMut, function(m) paste0(m[2], "_", m[3])))

    listMutFormatted <-
      unlist(lapply(unique(strsplit(snpMut_filt_freq$Mut_pos, "_")),
                    function(m) paste0(m[1], ":", m[2], "-", m[2])))

    pattern = ".bam$"
    BAMlist_list <-
      dir(path = bam_dir, pattern = pattern, full.names = TRUE)

    log2_print(InFilesToDo)
    if (!is.null(InFilesToDo)) {
      log2_print('In')
      log2_print(BAMlist_list)
      BAMlist_list <- BAMlist_list[BAMlist_list %in% InFilesToDo]
      log2_print(BAMlist_list)
    }

    if (!is.null(OutFilesToRm)) {
      log2_print('Out')
      ## OutFilesToRm <- file.path(thr_out_dir, OutFilesToRm)
      log2_print(OutFilesToRm)
      ## file.remove(OutFilesToRm)
      log2_print(list.files(path = out_dir))
    }

    if (length(BAMlist_list)==0)
      return()

    BAMlist <- paste(BAMlist_list, collapse = " ")
    BAM = paste0(str_split_fixed(basename(BAMlist_list),
                                 "\\.",
                                 n = 2)[, 1],
                 collapse = "\t")
    header = paste0("CHR\tPOS\t", BAM)

    ## print('BAM')
    ## print(BAM)

    numCores <- detectCores()
    registerDoParallel(numCores)

    res <- foreach(i = 1:length(listMutFormatted), .combine=c) %dopar% {
      ## print(paste0(samtools, " depth -a -Q 20 -r ", listMutFormatted[i], " ", BAMlist))
      system(paste0(samtools,
                    " depth -a -Q 20 -r ",
                    listMutFormatted[i],
                    " ",
                    BAMlist),
             intern = TRUE,
             ignore.stdout = FALSE,
             ignore.stderr = TRUE)
    }
    ## print('res')
    ## print(res)
    ## BAM = gsub(x = paste(unlist(strsplit(x = gsub(x=BAMlist, pattern=bam_dir, ""), split=".bam")), collapse = "\t"), " ","")


    ##return()  #######

    ## ################################################################
    ## MAKE DEPTH MATRIX ####
    ## ################################################################

    depth_tbl <-
      read.table(text = paste0(header,
                               "\n",
                               paste(res, collapse = '\n')),
                 header = TRUE,
                 sep = '\t',
                 stringsAsFactors = FALSE)
    rownames(depth_tbl) <-
      paste(depth_tbl$CHR, depth_tbl$POS, sep = "_")
    depth_tbl <- depth_tbl[, -c(1, 2)]

    snpMut_filt_freq$Mut_pos <-
      paste(snpMut_filt_freq$Chr, snpMut_filt_freq$PosStart, sep = "_")

    mut_tbl <-
      as.data.frame.matrix(table(snpMut_filt_freq$Mut_pos,
                                 snpMut_filt_freq$scID))

    ## cellInfo <- readRDS("data_info.rds")

    ## pattern=".bbq_s4.bam$"

    ## this cycle does not do nothing
    for(i in 1:ncol(depth_tbl)) {
      run <- colnames(depth_tbl)[i]
      ## colnames(depth_tbl)[i] <- as.character(cellInfo$Run[cellInfo$Run == gsub(pattern,"",run)])
      colnames(depth_tbl)[i] <-
        as.character(cellInfo[[id_column]][cellInfo[[id_column]] == run])
    }

    depth_tbl <-
      depth_tbl[order(row.names(depth_tbl)),
                order(colnames(depth_tbl))]
    mut_tbl <-
      mut_tbl[order(row.names(mut_tbl)),
              order(colnames(mut_tbl))]

    ## write final results to text files
    write.table(x = depth_tbl,
                file = file.path(out_dir,
                                 "final_data_depth.txt"),
                sep = '\t',
                row.names = TRUE,
                col.names = TRUE)
    write.table(x = mut_tbl,
                file = file.path(out_dir, "final_data_mut.txt"),
                sep = '\t',
                row.names = TRUE,
                col.names = TRUE)
  }

### end of file -- depth_computation.R
