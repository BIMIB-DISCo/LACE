### filters_computation.R
###


  filter1_compute <- function(vcf_out_dir,
                              cellInfo,
                              thr_alleles_ratio,
                              thr_maf,
                              thr_freq,
                              thr_out_dir,
                              id_column,
                              time_column,
                              variant_functions,
                              InFilesToDo = NULL,
                              OutFilesToRm = NULL) {

    dir.create(thr_out_dir, showWarnings = FALSE)
    if (!dir.exists(thr_out_dir))
      showNotification(paste("Filtered data output folder,",
                             thr_out_dir,
                             ", cannot be created"),
                       duration = 10,
                       type = "warning")

    listSC <- dir(path = vcf_out_dir,
                  pattern = ".exonic_variant_function",
                  ignore.case = TRUE,
                  full.names = TRUE)


    # - log2_print(InFilesToDo)
    if (!is.null(InFilesToDo)) {
      # - log2_print('In')
      # - log2_print(listSC)
      listSC <- listSC[listSC %in% InFilesToDo]
      # - log2_print(listSC)
    }
    if (!is.null(OutFilesToRm)) {
      # - log2_print('Out')
      ## OutFilesToRm <- file.path(thr_out_dir, OutFilesToRm)
      # - log2_print(OutFilesToRm)
      ## file.remove(OutFilesToRm)
      # - log2_print(list.files(path = thr_out_dir))
    }

    if (length(listSC) == 0)
      return()

    ## Create data structure to save the results
    tbFinal <- setNames(data.frame(matrix(ncol = 12, nrow = 0)),
                        c("scID",
                          "Time",
                          "Gene",
                          "Chr",
                          "PosStart",
                          "PosEnd",
                          "REF",
                          "ALT",
                          "MutType",
                          "rs_ID",
                          "depth",
                          "Allele_Ratio"))
    cn <- colnames(tbFinal)

    ## Consider each cell
    cont <- 0
    for(sc in listSC) {
      ## Get current cell name
      run <- strsplit(x = basename(sc), split = "\\.")[[1]][1]

      ## Read vcf annotated by ANNOVAR for current cell
      tmpTbl <- read.table(file = sc,
                           header = FALSE,
                           sep = "\t",
                           stringsAsFactors = FALSE)
      tmpTbl$scID <- run

      ## Get information about time of experiment
      tmpTime <-
        as.character(cellInfo[[time_column]][cellInfo[[id_column]] == run])
      tmpTbl$Time <- tmpTime

      ## Get genes names
      tmpTbl$V3 <- sapply(as.character(tmpTbl$V3),
                          function(x){strsplit(x, ":")[[1]][1]})

      ## Get the remaining information
      OtherInfo <- tmpTbl[, "V18"]
      tmpTbl <-
        tmpTbl[,
               c("scID",
                 "Time",
                 "V3",
                 "V4",
                 "V5",
                 "V6",
                 "V7",
                 "V8",
                 "V2",
                 "V11")]
      colnames(tmpTbl) <-
        c("sc_ID",
          "Time",
          "Gene",
          "Chr",
          "PStart",
          "PEnd",
          "Ref",
          "Alt",
          "Mut_type",
          "RS_id")

      ## Extract quality and coverage information
      for(m in 1:length(OtherInfo)) {
        info <- unlist(strsplit(OtherInfo[m], ":"))

        if (length(info) < 5) {
          depth <- NA
          RA <- NA
        } else {
          RefAlt <- unlist(strsplit(info[2], ","))
          depth <- sum(as.numeric(RefAlt))
          if (depth == 0) {
            RA <- NA
          } else {
            RA <- as.numeric(RefAlt[2])
          }
        }
        tmpTbl$depth[m] <- depth
        tmpTbl$Allele_Ratio[m] <- RA

      }

      ## Save results for current cell
      colnames(tmpTbl) <- cn
      tbFinal <- rbind(tbFinal,tmpTbl)

      cont = cont + 1
      cat(paste0(cont,
                 " done | ",
                 round(cont / length(listSC),
                       digits = 2),
                 "%\r"))
    }

    tbFinal$UIDsnp <-
      paste0(tbFinal$Gene,
             "_",
             tbFinal$Chr,
             "_",
             tbFinal$PosStart,
             ifelse(tbFinal$rs_ID == '.',
                    "",
                    paste0("_",tbFinal$rs_ID)))
    saveRDS(tbFinal,
            file = paste0(file.path(thr_out_dir,
                                    'cells_aggregate_info.rds')))

    tbFinal <-
      readRDS(file = paste0(file.path(thr_out_dir,
                                      'cells_aggregate_info.rds')))

    ## # remove synonymous mutations and the ones marked as unknown
    ## #modify
    ## #tbFinal <- tbFinal[tbFinal$MutType!="synonymous SNV"& tbFinal$MutType != "unknown", ]
    #browser()
    tbFinal <-
      tbFinal[tbFinal$MutType %in% variant_functions, ]

    u_rsID <-
      as.character(unique(tbFinal$rs_ID[startsWith(as.character(tbFinal$rs_ID),
                                                   prefix="rs")]))

    showNotification(sc, duration = 10, type = "warning")
    ## showNotification(length(u_rsID), duration = 10, type = "warning")

    ensembl_snp = useMart("ENSEMBL_MART_SNP", dataset="hsapiens_snp")
    SNPInfo <- getBM(attributes=c("refsnp_id",
                                  "minor_allele",
                                  "minor_allele_freq",
                                  "polyphen_prediction",
                                  "sift_prediction",
                                  "clinical_significance"),
                     filters = "snp_filter",
                     values = u_rsID,
                     mart = ensembl_snp)

    saveRDS(SNPInfo, file = paste0(file.path(thr_out_dir, 'SNPInfo.rds')))

    scMutInfo <-
      merge.data.frame(tbFinal,
                       SNPInfo,
                       by.x = "rs_ID",
                       by.y = "refsnp_id",
                       all.x = TRUE,
                       all.y = TRUE)
    saveRDS(scMutInfo,
            file = paste0(file.path(thr_out_dir, 'scMutInfo.rds')))
  }


  filter2_compute <- function(vcf_out_dir,
                              cellInfo,
                              thr_alleles_ratio,
                              thr_maf,
                              thr_freq,
                              thr_out_dir,
                              id_column,
                              time_column,
                              InFilesToDo = NULL,
                              OutFilesToRm = NULL) {

    ## ###############################
    ## # START FILTERING STEP ####
    ## ##############################

    listSC <- dir(path = vcf_out_dir,
                  pattern = ".exonic_variant_function",
                  ignore.case = TRUE,
                  full.names = TRUE)

    # - log2_print(InFilesToDo)
    if (!is.null(InFilesToDo)) {
      # - log2_print('In')
      # - log2_print(listSC)
      listSC <- listSC[listSC %in% InFilesToDo]
      # - log2_print(listSC)
    }
    if (!is.null(OutFilesToRm)) {
      # - log2_print('Out')
      ## OutFilesToRm <- file.path(thr_out_dir, OutFilesToRm)
      # - log2_print(OutFilesToRm)
      ## file.remove(OutFilesToRm)
      # - log2_print(list.files(path = thr_out_dir))
    }

    if (length(listSC)==0)
      return()

    scMutInfo <-
      readRDS(file=paste0(file.path(thr_out_dir, 'scMutInfo.rds')))

    scMutInfo <-
      scMutInfo[((!is.na(scMutInfo$Allele_Ratio)) &
                   (scMutInfo$Allele_Ratio >= thr_alleles_ratio)),]

    mutWOrs <- scMutInfo[scMutInfo$rs_ID == ".", ]
    mutWrs <- scMutInfo[scMutInfo$rs_ID != ".", ]

    FilterMutations <- function(sc_r) {

      ## If not a snp, keep it
      if (sc_r["rs_ID"] == ".") {
        return(TRUE)
      }

      ## Filter using MAF
      if (sc_r["minor_allele"] == sc_r["ALT"] &&
          sc_r["minor_allele_freq"] > thr_maf) {
        return(FALSE)
      }
      else if (sc_r["minor_allele"] == sc_r["REF"] &&
               sc_r["minor_allele_freq"] < (1-thr_maf)) {
        return(FALSE)
      }

      return(TRUE)
    }

    mutWrs_filt <- mutWrs[apply(mutWrs,
                                1,
                                function(x) FilterMutations(x)), ]
    snpMut_filt <- rbind(mutWOrs, mutWrs_filt)
    saveRDS(snpMut_filt,
            file = paste0(file.path(thr_out_dir, 'snpMut_filt.rds')))
  }


  filter3_compute <- function(vcf_out_dir,
                              cellInfo,
                              thr_alleles_ratio,
                              thr_maf,
                              thr_freq,
                              thr_out_dir,
                              id_column,
                              time_column,
                              time_points,
                              InFilesToDo = NULL,
                              OutFilesToRm = NULL) {


    ###########################################################################################################
    ### MAKE CELLS/MUTATIONS MATRIX ####
    ###########################################################################################################

    listSC <- dir(path = vcf_out_dir,
                  pattern = ".exonic_variant_function",
                  ignore.case = TRUE,
                  full.names = TRUE)

    # - log2_print(InFilesToDo)
    if (!is.null(InFilesToDo)) {
      # - log2_print('In')
      # - log2_print(listSC)
      listSC <- listSC[listSC %in% InFilesToDo]
      # - log2_print(listSC)
    }

    if (!is.null(OutFilesToRm)) {
      # - log2_print('Out')
      ## OutFilesToRm <- file.path(thr_out_dir, OutFilesToRm)
      # - log2_print(OutFilesToRm)
      ## file.remove(OutFilesToRm)
      # - log2_print(list.files(path = thr_out_dir))
    }

    if (length(listSC) == 0)
      return()

    snpMut_filt <-
      readRDS(file = paste0(file.path(thr_out_dir,
                                      'snpMut_filt.rds')))

    ## # consider only cells passing quality checks
    ## #load(file.path('..','valid_clones_mapping.RData'))
    ## #snpMut_filt = snpMut_filt[which(snpMut_filt$scID%in%valid_clones_mapping$Run),]

    Mut_keep = c()
    tblscMut = list()
    for (tn in time_points) {
      tblscMut[[tn]] <-
        table(snpMut_filt$scID[snpMut_filt$Time == tn ],
              snpMut_filt$UIDsnp[snpMut_filt$Time == tn])
      ## Binarization
      tblscMut[[tn]][which(tblscMut[[tn]] > 0)] <- 1

      cs <- colSums(tblscMut[[tn]], na.rm = TRUE) / nrow(tblscMut[[tn]])
      Mut_keep = c(Mut_keep, colnames(tblscMut[[tn]])[cs > thr_freq])
    }

    snpMut_filt_freq <-
      snpMut_filt[snpMut_filt$UIDsnp %in% Mut_keep, ]

    saveRDS(snpMut_filt_freq,
            file = paste0(file.path(thr_out_dir,
                                    'snpMut_filt_freq.rds')))
  }

### end of file -- filters_computation.R
