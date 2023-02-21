#################################################
##########  NA2  ###########
#################################################
NA_compute2_load <- function(data_dir, depth_dir, out_dir) {

  browser()


  dir.create(out_dir, showWarnings = FALSE)
  if (!dir.exists(out_dir))
    showNotification(paste("Filtered data output folder,", out_dir, ", cannot be created"), duration = 10, type = "warning")

  # load data
  cells_aggregate_info <- NULL
  snpMut_filt_freq <- NULL
  depth <-NULL
  if (file.exists(file.path( data_dir, "cells_aggregate_info.rds")))
    cells_aggregate_info <- readRDS(file=paste0(file.path( data_dir, "cells_aggregate_info.rds")))
  if (file.exists(file.path( out_dir, "snpMut_filt_freq_reduced.rds")))
    snpMut_filt_freq <- readRDS(file=paste0(file.path( out_dir, "snpMut_filt_freq_reduced.rds")))
  if (file.exists(file.path( depth_dir, "final_data_depth.txt")))
    depth <- as.matrix(read.table( file.path(depth_dir, "final_data_depth.txt")))
  return( list(cells_aggregate_info=cells_aggregate_info, snpMut_filt_freq=snpMut_filt_freq, depth=depth))
}

NA_compute2 <- function(depth_minimum, minumum_median_total, minumum_median_mutation, data_dir, depth_dir, out_dir, time_points, verified_genes, non_NA_genes, files) {

  browser()

  cells_aggregate_info <- files$cells_aggregate_info
  snpMut_filt_freq <- files$snpMut_filt_freq
  depth <- files$depth

  # set working directory
  #baseDir = "/data_directory/BimiB/share/LACE/MELANOMA/"
  #setwd(baseDir)

  # load required libraries
  # library("TRONCO")


  # dir.create(out_dir, showWarnings = F)
  # if (!dir.exists(out_dir))
  #   showNotification(paste("Filtered data output folder,", out_dir, ", cannot be created"), duration = 10, type = "warning")


  # list of manually verified mutations
  #verified_genes = c("ARPC2","CCT8","COL1A2","CYCS","HNRNPC","PCBP1","PRAME","RPL5")

  #depth_minimum = 3 # minimum depth to set values to NA
  #minumum_median_total = 10 # minimum median depth for total reads
  #minumum_median_mutation = 4 # minimum median depth for reads supporting mutations

  # load data
  # cells_aggregate_info <- readRDS(file=paste0(file.path( data_dir, "cells_aggregate_info.rds")))
  # snpMut_filt_freq <- readRDS(file=paste0(file.path( data_dir, "snpMut_filt_freq.rds")))
  # depth = as.matrix(read.table( file.path(depth_dir, "final_data_depth.txt")))

  # select only verified mutations
  if (!is.null(non_NA_genes))
    snpMut_filt_freq = snpMut_filt_freq[
      which(snpMut_filt_freq$Gene %in% non_NA_genes),
      c("scID","Time","Gene","Chr","PosStart","PosEnd","REF",
        "ALT","MutType","depth","Allele_Ratio")
      ]
  

  snpMut_filt_freq = snpMut_filt_freq[order(snpMut_filt_freq[,3],snpMut_filt_freq[,4],snpMut_filt_freq[,5],snpMut_filt_freq[,6],snpMut_filt_freq[,7],snpMut_filt_freq[,8],snpMut_filt_freq[,9],snpMut_filt_freq[,1],snpMut_filt_freq[,2],snpMut_filt_freq[,10],snpMut_filt_freq[,11]),]

  # compute frequency of each mutation
  #distinct_mutations = unique(snpMut_filt_freq[,c("Gene","Chr","PosStart","PosEnd","REF","ALT","ALT","ALT","ALT","ALT","ALT","ALT")])
  distinct_mutations = unique(snpMut_filt_freq[,c("Gene","Chr","PosStart","PosEnd","REF","ALT")])
  
  if (nrow(distinct_mutations)==0){
    num_columns <- sapply(distinct_mutations, is.numeric)
    distinct_mutations[num_columns] <- lapply(distinct_mutations[num_columns], round, 3)
    if (file.exists(file.path( out_dir, "D.RData")))
      file.remove(file.path(out_dir,"D.RData"))
    return(list("distinct_mutations"=distinct_mutations, "g"=NULL))
  }
    
  
  #time_points=c("before treatment", "4d on treatment", "28d on treatment", "57d on treatment")
  #for (t in seq(1,length(time_points)) )
  #  distinct_mutations[[paste0('FreqT',t)]] = NA
  #distinct_mutations[['MedianDepth']] = NA
  #distinct_mutations[['MedianDepthMut']] = NA
  #colnames(distinct_mutations)[7] = "FreqT1"
  #colnames(distinct_mutations)[8] = "FreqT2"
  #colnames(distinct_mutations)[9] = "FreqT3"
  #colnames(distinct_mutations)[10] = "FreqT4"
  #colnames(distinct_mutations)[11] = "MedianDepth"
  #colnames(distinct_mutations)[12] = "MedianDepthMut"
  #cells_timepoints = unique(snpMut_filt_freq[,c("scID","Time")])
  #distinct_mutations$FreqT1 = NA
  #distinct_mutations$FreqT2 = NA
  #distinct_mutations$FreqT3 = NA
  #distinct_mutations$FreqT4 = NA
  #distinct_mutations$MedianDepth = NA
  #distinct_mutations$MedianDepthMut = NA

  #times = list()
  #for (t in time_points)
  #  times[[t]] = as.numeric(table(cells_timepoints$Time)[t])

  # browser()
  # 
  # for(i in 1:nrow(distinct_mutations)) {
  #   curr = snpMut_filt_freq[
  #     which(
  #       snpMut_filt_freq$Gene %in% distinct_mutations[i,"Gene"] &
  #         snpMut_filt_freq$Chr %in% distinct_mutations[i,"Chr"] & 
  #         snpMut_filt_freq$PosStart %in% distinct_mutations[i,"PosStart"] & 
  #         snpMut_filt_freq$PosEnd %in% distinct_mutations[i,"PosEnd"] & 
  #         snpMut_filt_freq$REF %in% distinct_mutations[i,"REF"] & 
  #         snpMut_filt_freq$ALT %in% distinct_mutations[i,"ALT"]),]
  #   
  #   for (t in seq(1,length(time_points)) )
  #     distinct_mutations[i,paste0('FreqT',t)] = as.numeric(table(curr$Time)[time_points[[t]]]) / times[[t]]
  # 
  #   #distinct_mutations[i,"FreqT1"] = as.numeric(table(curr$Time)["before treatment"]) / t1
  #   #distinct_mutations[i,"FreqT2"] = as.numeric(table(curr$Time)["4d on treatment"]) / t2
  #   #distinct_mutations[i,"FreqT3"] = as.numeric(table(curr$Time)["28d on treatment"]) / t3
  #   #distinct_mutations[i,"FreqT4"] = as.numeric(table(curr$Time)["57d on treatment"]) / t4
  #   #print('a')
  #   #print(curr$depth)
  #   #print(median(curr$depth))
  #   #print(median(curr$Allele_Ratio))
  #   distinct_mutations[i,"MedianDepth"] = as.numeric(median(curr$depth))
  #   distinct_mutations[i,"MedianDepthMut"] = as.numeric(median(curr$Allele_Ratio))
  # }
  
  distinct_mutations <- snpMut_filt_freq %>%
    group_by(Time) %>% 
    mutate(tot_per_time=n_distinct(scID)) %>% #cells per time point
    group_by(Gene,Chr,PosStart,PosEnd,REF,ALT,tot_per_time) %>% 
    add_count(Time) %>% #cells per mutations
    mutate(fr=n/tot_per_time) %>% 
    group_by(Gene,Chr,PosStart,PosEnd,REF,ALT) %>%
    mutate(MedianDepth=median(depth), MedianDepthMut=median(Allele_Ratio)) %>%
    ungroup() %>%
    select(-c(scID,MutType, Allele_Ratio, depth, n, tot_per_time)) %>% group_by_all() %>%
    mutate(Time= as.integer(factor(Time, levels = time_points, ordered = TRUE))) %>%
    distinct() %>%
    #summarise() %>%  #arrange(Time) %>%
    pivot_wider(names_from = Time, values_from = fr, names_prefix = "FreqT") 
  
  
  
#   distinct_mutations %>% add_count(Time, name= tot_per_time) %>%
#     arrange(name) %>%
#     group_by(Gene,Chr,PosStart,PosEnd,REF,ALT,Time) %>% 
#     pivot_wider(names_from = wool, values_from = Time, 
#                 
#                 mutate( )%>%
#                   
#                   group_by(Gene,Chr,PosStart,PosEnd,REF,ALT) %>%
#                   mutate(MedianDepth"] = median(depth)) %>%
# mutate(MedianDepth"] = median(Allele_Ratio))
#                 
  
  #distinct_mutations = distinct_mutations[-c(3,5,6,8),]
  # if (
  #   length(
  #     sort(
  #       unique(
  #         c(
  #           which(distinct_mutations$MedianDepth<minumum_median_total),
  #           which(distinct_mutations$MedianDepthMut<minumum_median_mutation)
  #           )
  #         )
  #       )
  #     )>0)
  #   distinct_mutations = distinct_mutations[
  #     -sort(
  #       unique(c(
  #         which(distinct_mutations$MedianDepth<minumum_median_total),
  #         which(distinct_mutations$MedianDepthMut<minumum_median_mutation)
  #         ))
  #       ),]
  
  distinct_mutations <- distinct_mutations %>% 
    filter(MedianDepth>=minumum_median_total & MedianDepthMut>=minumum_median_mutation) %>%
    ungroup()
  
  if (nrow(distinct_mutations)==0){
    num_columns <- sapply(distinct_mutations, is.numeric)
    distinct_mutations[num_columns] <- lapply(distinct_mutations[num_columns], round, 3)
    if (file.exists(file.path( out_dir, "D.RData")))
      file.remove(file.path(out_dir,"D.RData"))
    return(list("distinct_mutations"=distinct_mutations, "g"=NULL))
  }
  
  #rownames(distinct_mutations) = 1:nrow(distinct_mutations)
  
  valid_distinct_mutations = distinct_mutations
  if (!is.null(verified_genes))
    valid_distinct_mutations = subset(distinct_mutations,distinct_mutations$Gene %in% verified_genes) %>% ## mod
    filter(REF!="-") ## mod
  
  if (nrow(valid_distinct_mutations)==0){
    num_columns <- sapply(distinct_mutations, is.numeric)
    distinct_mutations[num_columns] <- lapply(distinct_mutations[num_columns], round, 3)
    if (file.exists(file.path( out_dir, "D.RData")))
      file.remove(file.path(out_dir,"D.RData"))
    return(list("distinct_mutations"=distinct_mutations, "g"=NULL))
  }
  
  
  valid_distinct_mutations_values = NULL
  for(i in 1:nrow(valid_distinct_mutations)) {
    valid_distinct_mutations_values = c(valid_distinct_mutations_values,paste0(valid_distinct_mutations[i,c("Chr","PosStart")],collapse="_"))
  }
  save(valid_distinct_mutations,file=file.path(out_dir,"valid_distinct_mutations.RData"))

  # make final mutations data structures
  #load("valid_clones_mapping.RData")
  #mutations = array(0,c(length(unique(valid_clones_mapping$Run)),6))
  #rownames(mutations) = sort(unique(valid_clones_mapping$Run))
  #colnames(mutations) = paste0(valid_distinct_mutations$Gene,"_",valid_distinct_mutations_values,"_",valid_distinct_mutations$REF,"_",valid_distinct_mutations$ALT)
  mutations = array(0,c(length(unique(colnames(depth))),nrow(valid_distinct_mutations)))
  rownames(mutations) = sort(unique(colnames(depth)))
  colnames(mutations) = paste0(valid_distinct_mutations$Gene,"_",valid_distinct_mutations_values,"_",valid_distinct_mutations$REF,"_",valid_distinct_mutations$ALT)


  
  #mutations = array(0,c(nrow(valid_distinct_mutations), length(unique(o_snpMut_filt_freq$scID))))
  #colnames(mutations) = sort(unique(o_snpMut_filt_freq$scID))
  #rownames(mutations) = valid_distinct_mutations_values
  
  #mutations = mutations[valid_distinct_mutations_values,]
  #browser()
  # set NA values
  depth = t(depth)
  #dim(depth)
  #depth = depth[colnames(mutations), rownames(mutations), drop=FALSE]
  depth = depth[sort(unique(snpMut_filt_freq$scID)), valid_distinct_mutations_values, drop=FALSE]
  
  
  mutations <- inner_join(cells_aggregate_info, valid_distinct_mutations) %>% 
    filter( scID %in% rownames({{depth}}))%>% 
    mutate(idx = paste(Gene,Chr,PosStart,REF,ALT,sep='_')) %>% 
    mutate(val=1) %>%
    distinct() %>%  
    #select(scID,idx,val) %>%
    pivot_wider(id_cols=scID, names_from = idx, values_from=val, values_fill=0) %>%
    full_join(data.frame(scID=rownames({{depth}})))
  
  mutations[is.na(mutations)] <- 0
  mutations=data.frame(mutations,row.names = 1)
  
  
  
  
  col_n <- 
    apply(str_split(colnames(mutations), pattern = "_", simplify = TRUE)[,2:3, drop=FALSE], 1, paste, collapse="_")
  
  depth = depth[rownames(mutations), col_n, drop=FALSE]
  colnames(depth) = colnames(mutations)
  mutations[which(depth<=depth_minimum,arr.ind=TRUE)] = NA # missing values rate equals to 359/2850, that is approx 12.6%
  
  
  cells_aggregate_info = cells_aggregate_info[which(cells_aggregate_info$scID%in%rownames(mutations)), , drop=FALSE]
  mycellsdata = list()
  for ( t in time_points )
    mycellsdata[[t]] = as.matrix.data.frame(mutations[sort(unique(cells_aggregate_info$scID[which(cells_aggregate_info$Time==t)])), , drop=FALSE], rownames.force = TRUE)
  D = mycellsdata
  #browser()
  save(D,file=file.path(out_dir,"D.RData"))

  
  ####
  #browser()
  g <- plot_D(D)
  
  
  
  ####
  
  
  # make Oncoprint
  n_rows=0
  for (t in seq(1, length(time_points)))
    n_rows=n_rows+nrow(D[[t]])
  clusters = array(NA,c(n_rows,1))

  init_i=1
  end_i=0
  r_names= NULL
  data = NULL
  for (t in seq(1, length(time_points))) {
    if(nrow(D[[t]])>0) {
      end_i <- end_i+nrow(D[[t]])
      r_names=c(r_names, rownames(D[[t]]))
      clusters[init_i:end_i,1] = time_points[[t]]
      init_i <- end_i+1
      data <- rbind(data, D[[t]])
    }
  }
  rownames(clusters) <- r_names
  #clusters[1:nrow(D[[1]]),1] = "T1_before_treatment"
  #clusters[(nrow(D[[1]])+1):(nrow(D[[1]])+nrow(D[[2]])),1] = "T2_4_days_treatment"
  #clusters[(nrow(D[[1]])+nrow(D[[2]])+1):(nrow(D[[1]])+nrow(D[[2]])+nrow(D[[3]])),1] = "T3_28_days_treatment"
  #clusters[(nrow(D[[1]])+nrow(D[[2]])+nrow(D[[3]])+1):(nrow(D[[1]])+nrow(D[[2]])+nrow(D[[3]])+nrow(D[[4]])),1] = "T4_57_days_treatment"
  #rownames(clusters) = c(rownames(D[[1]]),rownames(D[[2]]),rownames(D[[3]]),rownames(D[[4]]))
  #data = rbind(D[[1]],D[[2]],D[[3]],D[[4]])
  #data[which(is.na(data))] = 0
  #data = import.genotypes(data)
  # data = annotate.stages(data,clusters)
  ## if (ncol(data$genotypes)>1 || length(unique(data$genotypes[,1]))>1) #errore generico
  ##  oncoprint(data,excl.sort=FALSE,group.by.stage=TRUE)

  #distinct_mutations[['ResultantMeanDepth']]<-apply(depth,2,mean)
  #distinct_mutations[['ResultantVarDepth']]<-apply(depth,2,var)
  #browser()
  num_columns <- sapply(distinct_mutations, is.numeric)
  distinct_mutations[num_columns] <- lapply(distinct_mutations[num_columns], round, 3)
  return(list("distinct_mutations"=distinct_mutations, "g"=g))
}


plot_D <-function(D) {
  D <- D %>% 
    setNames({names(.) %>% 
        str_replace_all(pattern = "_", replacement = " ") %>% 
        data.frame("times"=.) %>% 
        mutate(
          times=ifelse(str_starts(times, pattern = "T[0-9] ", negate = TRUE), 
                       paste0('T', row_number(), " ",times), times)
        )}$times)
  
  #names(D) <- t_order$times
  
  
  D1 <- lapply(D, FUN=function(x){as_tibble(x,rownames = "cell")}) %>%
    bind_rows(.id = "time")  %>%
    mutate(time=forcats::fct_inorder(factor(time, ordered=TRUE))) %>%
    relocate(cell, .after = last_col()) %>% ##
    arrange(across(everything())) %>%
    mutate(ordered=factor(cell, levels = unique(cell, fromLast=TRUE), ordered = TRUE))
  
  
  #D1 <- D1 %>%
  #  mutate(ordered=cell) %>%
  #  arrange(across(everything())) %>%
  #  mutate(ordered=factor(ordered, levels = unique(ordered, fromLast=TRUE), ordered = TRUE))
  
  
  s <- D1 %>% group_by(time) %>% 
    summarise(across(where(is.numeric), ~sum(.,na.rm = TRUE))) %>% 
    pivot_longer(-1) %>% 
    pivot_wider(names_from = 1, values_from = value) #%>% 
    #arrange(across(everything())) not needed
  
  Dw <- D1 %>%
    pivot_longer( col = -c(time, cell,ordered), names_to = "mutation", values_to = "value") %>%
    mutate(value = factor(as.integer(value), levels=c(0,1,"NA"), exclude = NULL, ordered = T)) %>%
    mutate(mutation = factor(mutation, levels=s$name, ordered=TRUE))
  
  
  
  
  per_mut_site <- as.data.frame(Dw) %>% group_by(mutation)
  n_na <- (per_mut_site %>% mutate(cnt=sum(is.na(value))) )$cnt
  n_one <- (per_mut_site %>% mutate(cnt=sum(value==1, na.rm = TRUE)) )$cnt
  n_zero <- (per_mut_site %>% mutate(cnt=sum(value==0, na.rm = TRUE)) )$cnt
  str__na <- paste0("# \"NA\" = [", paste(range(n_na), collapse = ", "),"]")
  str__zero <- paste0("# \"0\" = [", paste(range(n_zero), collapse = ", "),"]")
  str__one <- paste0("# \"1\" = [", paste(range(n_one), collapse = ", "),"]")
  
  
  g <- ggplot(Dw, aes(mutation, y=ordered, fill= value)) +
    geom_tile()+
    
    scale_fill_manual(values = c("black", "white", "orange"), na.value="gray50",
                      aesthetics = c("colour", "fill")#, 
                      #labels = c("passing filter(s)", "filtered out", "NA")
    )+ #orange=2
    
    facet_grid(
      rows = vars(time),
      scales = "free",
      space = "free",
      labeller = label_wrap_gen(width=15)
    ) +
    scale_x_discrete(expand = c(0,0))+
    theme(axis.text.x = element_text(angle=45, hjust=1), ##it is shifted why?
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank()
    )+
    theme(
      strip.background = element_rect(
        color="black", fill="#FC8207", size=1.5, linetype="solid"
      ),
      panel.border = element_rect(color = "#FC8207", fill = NA, size = 2)
    ) +
    theme(legend.key=element_rect(colour="black")) +
    theme(panel.spacing = unit(0, "pt"))+
    labs(y = "cells")+
    labs(x = "selected mutations")+
    #labs(title = paste("Pre-inference input matrix {",str__na, ";", str__one, ";", str__zero,"}"))+
    labs(title = paste("Filtered binary data matrix"))+
    theme(text = element_text(size = 14))  
  
  return(g)
}