#################################################
##########  NA  ###########
#################################################
NA_compute <- function(depth_minimum, missing_values_max, data_dir, depth_dir, out_dir, time_points) {
  # set working directory

  #library(stringr)

  #depth_minimum = 3 # minimum depth to set values to NA
  #missing_values_max = 0.40 # maximum number of considered missing data per gene



  dir.create(out_dir, showWarnings = F)
  if (!dir.exists(out_dir))
    showNotification(paste("Filtered data output folder,", out_dir, ", cannot be created"), duration = 10, type = "warning")

  # load data
  snpMut_filt_freq <- readRDS(file=paste0(file.path(data_dir, "snpMut_filt_freq.rds")))
  mutations = as.matrix(read.table(file.path(depth_dir, "final_data_mut.txt")))
  depth = as.matrix(read.table(file.path(depth_dir, "final_data_depth.txt")))

  # set NA values
  check_ids <- snpMut_filt_freq$scID %in% colnames(mutations)
  error_ids <- sort(unique(snpMut_filt_freq$scID[!check_ids]))
  snpMut_filt_freq  <- snpMut_filt_freq[check_ids,]


  depth = depth[,colnames(mutations)]
  mutations[which(mutations>1,arr.ind=TRUE)] = 1
  mutations[which(depth<=depth_minimum,arr.ind=TRUE)] = NA

  mycellsdata = list()
  #time_points=c("before treatment", "4d on treatment", "28d on treatment", "57d on treatment")
  check_time_points <- time_points %in% unique(snpMut_filt_freq$Time)

  time_points <- time_points[check_time_points]


  for (i in time_points) {
    mycellsdata[[i]] = t(mutations[,sort(unique(snpMut_filt_freq$scID[which(snpMut_filt_freq$Time==i)]))])
  }

  # evaluate number of missing data per time point
  time_points_NA = list()
  for (t in time_points)
    time_points_NA[[t]] = NULL

  for(i in 1:ncol(mycellsdata[[1]]))
    for (t in time_points)
      time_points_NA[[t]] = c(time_points_NA[[t]],length(which(is.na(mycellsdata[[t]][,i])))/nrow(mycellsdata[[t]]))

  #valid_genes = sort(unique(colnames(mycellsdata[[1]])[which(all(time_points_NA<=missing_values_max))]))
  valid_genes = colSums(do.call(rbind, lapply(time_points_NA, function(x){x<=missing_values_max})))
  valid = NULL

  for(i in valid_genes) {
    valid = c(valid,unique(snpMut_filt_freq$Gene[grep(i,snpMut_filt_freq$UIDsnp)]))
  }
  valid = paste0(valid,"_",valid_genes)


  # get names of valid genes
  valid_genes_names = NULL
  #for(i in valid) {
  #  valid_genes_names = c(valid_genes_names,strsplit(i,split="_")[[1]][[1]])
  #}
  valid_genes_names <- str_split_fixed(valid[1:4],"_", n=2 )[,1]

  print(length(valid_genes_names))
  print(length(unique(valid_genes_names)))
  print(sort(valid_genes_names[which(duplicated(valid_genes_names))]))

  # make final list of candidate selected variants
  snpMut_filt_freq_reduced = unique(snpMut_filt_freq[which(snpMut_filt_freq$Gene%in%valid_genes_names),c("Gene","Chr","PosStart","PosEnd","REF","ALT")])
  snpMut_filt_freq_reduced = snpMut_filt_freq_reduced[order(snpMut_filt_freq_reduced[,1],snpMut_filt_freq_reduced[,2],snpMut_filt_freq_reduced[,3],snpMut_filt_freq_reduced[,4],snpMut_filt_freq_reduced[,5],snpMut_filt_freq_reduced[,6]),]

  write.table(snpMut_filt_freq_reduced,file=file.path(out_dir,"snpMut_filt_freq_reduced.txt"),append=FALSE,quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE)

}

