# Remove any indistinguishable event from input data
check.indistinguishable <- function( data ) {
    
    # build one data matrix from all time points
    full_data <- NULL
    for(i in 1:length(data)) {
        full_data <- rbind(full_data,data[[i]])
    }
    
    # check for indistinguishable events
    indistinguishable <- as.numeric(which(duplicated(t(full_data))))
    if(length(indistinguishable)>0) {
        # merge names of indistinguishable events
        valid_colnames <- colnames(full_data)[-indistinguishable]
        new_colnames <- valid_colnames
        invalid_colnames <- colnames(full_data)[indistinguishable]
        for(i in invalid_colnames) {
            for(j in valid_colnames) {
                if(all(is.na(full_data[,i])==is.na(full_data[,j]))) { # if NAs in i and j are the same
                    if(all(full_data[,i]==full_data[,j],na.rm=TRUE)) { # if not NAs entries in i and j are the same
                        new_colnames[which(valid_colnames==j)] <- paste0(new_colnames[which(valid_colnames==j)],"|",invalid_colnames[which(invalid_colnames==i)])
                        next
                    }
                }
            }
        }
        # remove indistinguishable events from data
        for(i in 1:length(data)) {
            data[[i]] <- data[[i]][,valid_colnames,drop=FALSE]
            colnames(data[[i]]) <- new_colnames
        }
    }
    
    return(data)
    
}

# Build adjacency matrix from B as returned from the function learn.longitudinal.phylogeny
as.adj.matrix <- function( B ) {
    
    adj_matrix <- array(0L,c((dim(B)[1]-1),(dim(B)[2]-1)))
    rownames(adj_matrix) <- colnames(B)[2:ncol(B)]
    colnames(adj_matrix) <- colnames(B)[2:ncol(B)]
    
    for(rP in 2:(nrow(B)-1)) {
        
        for(rC in ((rP+1):nrow(B))) {
            
            if(all(B[rP,1:rP]==B[rC,1:rP])&&(sum(B[rP,])==(sum(B[rC,])-1))) {
                
                adj_matrix[(rP-1),(rC-1)] <- 1
                
            }
            
        }
        
    }
    
    adj_matrix <- adj_matrix[as.character(sort(as.numeric(rownames(adj_matrix)))),]
    adj_matrix <- adj_matrix[,as.character(sort(as.numeric(colnames(adj_matrix))))]
    
    return(adj_matrix)
    
}

# Build adjacency matrix from B as returned from the function learn.longitudinal.phylogeny (unsorted)
as.adj.matrix.unsorted <- function( B, root = FALSE ) {
    
    if(root) {
        adj_matrix = array(0L,dim(B))
        rownames(adj_matrix) <- colnames(B)[(1:ncol(B))]
        colnames(adj_matrix) <- colnames(B)[(1:ncol(B))]
        for(rP in 1:(nrow(B))-1) {
            for(rC in (rP+1):nrow(B)) {
                if(all(B[rP,1:rP] == B[rC,1:rP]) && sum(B[rP,])==(sum(B[rC,])-1)) {
                    adj_matrix[(rP),(rC)] <- 1
                }   
            }
        }
    }
    else {
        adj_matrix = array(0L,(dim(B) - 1))
        rownames(adj_matrix) <- colnames(B)[(2:ncol(B))]
        colnames(adj_matrix) <- colnames(B)[(2:ncol(B))]
        for(rP in 2:(nrow(B))-1) {
            for(rC in (rP+1):nrow(B)) {
                if(all(B[rP,1:rP] == B[rC,1:rP]) && sum(B[rP,])==(sum(B[rC,])-1)) {
                    adj_matrix[(rP-1),(rC-1)] <- 1
                }   
            }
        }
    }
    
    return(adj_matrix)
    
}

# Build B from an adjacency matrix where we assume clones and mutations to be both ordered
as.B <- function( adj_matrix ) {
    
    n_clones <- dim(adj_matrix)[1]
    B <- diag(n_clones)
    
    for(k in 1:n_clones) {
        
        idx_child <- which(adj_matrix[k,]==1,arr.ind=TRUE)
        
        if(length(idx_child)==1) {
            
            B[idx_child,] <- B[k,] + B[idx_child,]
            
        } 
        else if(length(idx_child)>1) {
            
            B[idx_child,] <- sweep(B[idx_child,],2,B[k,],"+")
            
        }
        
    }
    rownames(B) <- 1:nrow(B)
    colnames(B) <- 1:ncol(B)
    B <- cbind(rep(1,nrow(B)),B)
    B <- rbind(c(1,rep(0,(ncol(B)-1))),B)
    rownames(B)[1] <- 'r'
    colnames(B)[1] <- 'r'
    
    return(B)
    
}

# check that point Q is not on segments A-B
checkQonAB <- function( Ax, Ay, Bx, By, Qx, Qy ) {

    if((Qx <= max(Ax,Bx)) && (Qx >= min(Ax, Bx)) && (Qy <= max(Ay, By)) && (Qy >= min(Ay, By))) {
        return(TRUE)
    }
    return(FALSE)

}

# check segments orientation
orientation <- function( Ax, Ay, Bx, By, Qx, Qy ) {

    res <- ((Qy - Ay) * (Bx - Qx)) - ((Qx - Ax)*(By - Qy))
    if(res > 0) {
        return(1)
    }
    else if(res < 0) {
        return(2)
    }
    else {
        return(0)
    }

}

# check if two segments interserct
checkSegmentIntersect <- function( ax1, ay1, ax2, ay2, bx1, by1, bx2, by2 ) {
    
    c1 = orientation(ax1,ay1, bx1,by1, ax2,ay2)
    c2 = orientation(ax1,ay1, bx2,by2, ax2,ay2)
    c3 = orientation(bx1,by1, ax1,ay1, bx2,by2)
    c4 = orientation(bx1,by1, ax2,ay2, bx2,by2)
    
    if((c1 != c2) && (c3 != c4)) {
        return(TRUE)
    }
    if((c1 == 0) && checkQonAB(ax1,ay1, ax2,ay2, bx1,by1)) {
        return(TRUE)
    }
    if((c2 == 0) && checkQonAB(ax1,ay1, ax2,ay2, bx2,by2)) {
        return(TRUE)
    }
    if((c3 == 0) && checkQonAB(bx1,by1, bx2,by2, ax1,ay1)) {
        return(TRUE)
    }
    if((c4 == 0) && checkQonAB(bx1,by1, bx2,by2, ax2,ay2)) {
        return(TRUE)
    }
    
    return(FALSE)
    
}

#' Compute mutation distance among variants from LACE corrected genotype and use it to perform hierarchical clustering.
#' @title compute.mutation.distance
#'
#' @examples
#' data(inference)
#' mutation_distance <- compute.mutation.distance(inference)
#'
#' @param inference Results of the inference by LACE.
#' @return A matrix mutation_distance with the mutation distance among variants computed from LACE corrected genotype and related hierarchical clustering.
#' @export compute.mutation.distance
#' @importFrom stats dist hclust
#'
compute.mutation.distance <- function( inference ) {
    
    mutation_distance <- dist(t(inference$corrected_genotype),method="euclidean")
    mutation_distance <- list(distance_matrix=as.matrix(mutation_distance),hierarchical_clustering=hclust(mutation_distance,method="complete"))
    return(mutation_distance)
    
}

#' Compute error rates for the considered variants comparing observed data to LACE corrected genotype.
#' @title compute.variants.error.rates
#'
#' @examples
#' data(longitudinal_sc_variants)
#' data(inference)
#' variants_error_rates <- compute.variants.error.rates(longitudinal_sc_variants,inference)
#'
#' @param D Mutation data from multiple experiments for a list of driver genes provided as a data matrix per time point. 
#' @param inference Results of the inference by LACE.
#' @return A matrix variants_error_rates with the estimated error rates for the considered variants.
#' @export compute.variants.error.rates
#'
compute.variants.error.rates <- function( D, inference ) {
    
    variants_error_rates <- array(NA,c(ncol(D[[1]]),2))
    rownames(variants_error_rates) <- colnames(D[[1]])
    colnames(variants_error_rates) <- c("Percentage_False_Positives","Percentage_False_Negatives")
    
    corrected_genotype <- inference$corrected_genotype
    observed_genotype <- NULL
    for(i in 1:length(D)) {
        observed_genotype <- rbind(observed_genotype,D[[i]])
    }
    
    for(i in rownames(variants_error_rates)) {
        fn <- length(which(observed_genotype[,i]==0&corrected_genotype[,i]==1))
        fp <- length(which(observed_genotype[,i]==1&corrected_genotype[,i]==0))
        variants_error_rates[i,"Percentage_False_Positives"] <- fp/nrow(observed_genotype)
        variants_error_rates[i,"Percentage_False_Negatives"] <- fn/nrow(observed_genotype)
    }
    
    return(variants_error_rates)
    
}

recursiveDescend <- function( descend ) {
    
    # Check next node,
    next_v <- which(descend$adjM[descend$row_v,] > 0)
    # If not than return
    if(length(next_v)==0) {
        descend$max_deep <- max(descend$l_path)
        return(descend)
    } else if(length(next_v) > 1){
        # brach!
        curr_level <- descend$l_path[descend$ind_lPath]
        for(v in next_v){
            descend$row_v = v
            descend$ind_lPath = descend$ind_lPath + 1
            descend$l_path[descend$ind_lPath] = curr_level + 1
            descend$mut_list <- c(descend$mut_list, as.numeric(v))
            descend <- recursiveDescend(descend = descend)
        }
    } else  {
        descend$l_path[descend$ind_lPath] = descend$l_path[descend$ind_lPath] + 1
        descend$row_v = next_v
        descend$mut_list <- c(descend$mut_list, as.numeric(next_v))
        descend <- recursiveDescend(descend = descend)
    }
    descend$max_deep <- max(descend$l_path)
    return(descend)

}


recursiveLongitudinalLayout <- function( idx_Vc, Xc, Yc, cl_df, adjMatrix_base, adjMatrix_overall, mut_TP, labels_show, label_offset) {
    
    
    cl_vertex_idx_Vc <- which(cl_df$cl_vertex$names == colnames(adjMatrix_overall)[idx_Vc])
    
    cl_df$cl_vertex$coord.x[cl_vertex_idx_Vc] <- Xc
    cl_df$cl_vertex$coord.y[cl_vertex_idx_Vc] <- Yc
    
    idx_next_v <- which(adjMatrix_overall[idx_Vc,] != 0)
    if(length(idx_next_v) == 0) {
        return(cl_df)
    } else if(length(idx_next_v) > 1){
        
        deep <- c()
        for(v in idx_next_v){
            descend = list(
                adjM = adjMatrix_overall,
                l_path = c(1),
                ind_lPath = 1,
                mut_list = c(v),
                row_v = v
            )
            deep_tmp <- recursiveDescend(descend)
            
            dif_clones <- unique(cl_df$cl_vertex$clone[cl_df$cl_vertex$names %in% colnames(adjMatrix_overall)[deep_tmp$mut_list]])
        }
        ord <- order(-cl_df$cl_vertex$TP[match(names(idx_next_v),cl_df$cl_vertex$names)], deep, decreasing = F)
        
        idx_next_v <- idx_next_v[ord]
        deep <- deep[ord]

        offset_X = 0
        for(i in 1:length(idx_next_v)){
            
            Vn_name <- colnames(adjMatrix_overall)[idx_next_v[i]]
            
            if(adjMatrix_overall[idx_Vc,idx_next_v[i]] == 2) {
                X = Xc
                Y = mut_TP[as.character((cl_df$cl_vertex$TP[cl_df$cl_vertex$names == Vn_name]-1))] + 1
                offset_X = offset_X + deep[i] - 1
            } else {
                
                
                
                if(cl_df$cl_vertex$TP[cl_vertex_idx_Vc] < cl_df$cl_vertex$TP[cl_df$cl_vertex$names == Vn_name]) {
                    Y = mut_TP[as.character((cl_df$cl_vertex$TP[cl_df$cl_vertex$names == Vn_name]-1))] + 1
                } else {
                    Y = Yc + 1
                }
                
                X = (Xc + offset_X + 1)

                offset_X = offset_X + deep[i]

                idx_row_edges = which(cl_df$cl_edges$from == cl_df$cl_vertex$names[cl_vertex_idx_Vc] & cl_df$cl_edges$to == Vn_name)
                mutation_name <- cl_df$cl_vertex$last_mutation[cl_df$cl_vertex$names == Vn_name]
                
                clone_fake_edge <- data.frame(from = cl_df$cl_edges$from[idx_row_edges],
                                              to = paste0('H_',mutation_name),
                                              type = "Parental",
                                              extincion = FALSE,
                                              label = "", 
                                              name = "",
                                              lty = 1)
                
                fake_mutation_edge <- data.frame(from = paste0('H_',mutation_name),
                                                 to = mutation_name,
                                                 type = "Parental",
                                                 extincion = FALSE,
                                                 label = "",
                                                 name = "",
                                                 lty = 1)
                
                mutation_clone_edge <- data.frame(from = mutation_name,
                                                  to = cl_df$cl_edges$to[idx_row_edges],
                                                  type = "Parental",
                                                  extincion = FALSE,
                                                  label = "",
                                                  name = "",
                                                  lty = 1)
                
                fake_vertex <- data.frame(names = paste0('H_',mutation_name), 
                                          branch_level = NA,
                                          branch = NA,
                                          label = "",
                                          last_mutation = "",
                                          TP = NA, 
                                          clone = cl_df$cl_vertex$clone[cl_vertex_idx_Vc],
                                          prevalance = NA,
                                          size = 0,
                                          size2 = 0,
                                          shape = "none",
                                          label.dist = 0,
                                          label.degree = 0,
                                          extincion = 0,
                                          coord.x = X,
                                          coord.y = Yc,
                                          color = NA)
                
                mutation_vertex <- data.frame(names = mutation_name, 
                                              branch_level = NA,
                                              branch = NA,
                                              label = ifelse(test = labels_show %in% c("both", "mutations"), yes = mutation_name, no = ""),
                                              last_mutation = "",
                                              TP = NA, 
                                              clone = cl_df$cl_vertex$clone[cl_vertex_idx_Vc],
                                              prevalance = NA,
                                              size = 6,
                                              size2 = 6,
                                              shape = "square",
                                              label.dist = label_offset,
                                              label.degree = 0,
                                              extincion = 0,
                                              coord.x = X,
                                              coord.y = (Yc + Y)*0.5,
                                              color = cl_df$cl_vertex$color[cl_df$cl_vertex$names == Vn_name])
                
                cl_df$cl_edges <- cl_df$cl_edges[-idx_row_edges,]
                cl_df$cl_edges <- rbind(cl_df$cl_edges, clone_fake_edge, fake_mutation_edge, mutation_clone_edge)
                
                cl_df$cl_vertex <- rbind(cl_df$cl_vertex, fake_vertex, mutation_vertex)
                
            }
            cl_df <- recursiveLongitudinalLayout(idx_next_v[i], X, Y, cl_df, adjMatrix_base, adjMatrix_overall, mut_TP, labels_show, label_offset)
        }
    } else {
        # NO BRANCH
        Vn_name <- colnames(adjMatrix_overall)[idx_next_v]
        
        if(adjMatrix_overall[idx_Vc,idx_next_v] == 2) {
            X = Xc
            Y = mut_TP[as.character((cl_df$cl_vertex$TP[cl_df$cl_vertex$names == Vn_name]-1))] + 1
        } else {
            
            if(cl_df$cl_vertex$TP[cl_vertex_idx_Vc] < cl_df$cl_vertex$TP[cl_df$cl_vertex$names == Vn_name]) {
                Y = mut_TP[as.character((cl_df$cl_vertex$TP[cl_df$cl_vertex$names == Vn_name]-1))] + 1
            } else {
                Y = Yc + 1
            }
            
            X = Xc + 1
            
            idx_row_edges = which(cl_df$cl_edges$from == cl_df$cl_vertex$names[cl_vertex_idx_Vc] & cl_df$cl_edges$to == Vn_name)
            mutation_name <- cl_df$cl_vertex$last_mutation[cl_df$cl_vertex$names == Vn_name]
            clone_fake_edge <- data.frame(from = cl_df$cl_edges$from[idx_row_edges],
                                          to = paste0('H_',mutation_name),
                                          type = "Parental",
                                          extincion = FALSE,
                                          label = "",
                                          name = "",
                                          lty = 1)
            
            fake_mutation_edge <- data.frame(from = paste0('H_',mutation_name),
                                             to = mutation_name,
                                             type = "Parental",
                                             extincion = FALSE,
                                             label = "",
                                             name = "",
                                             lty = 1)
            
            mutation_clone_edge <- data.frame(from = mutation_name,
                                              to = cl_df$cl_edges$to[idx_row_edges],
                                              type = "Parental",
                                              extincion = FALSE,
                                              label = "",
                                              name = "",
                                              lty = 1)
            
            fake_vertex <- data.frame(names = paste0('H_',mutation_name), 
                                      branch_level = NA,
                                      branch = NA,
                                      label = "",
                                      last_mutation = "",
                                      TP = NA,#cl_df$cl_vertex$TP[cl_vertex_idx_Vc],
                                      clone = cl_df$cl_vertex$clone[cl_vertex_idx_Vc],
                                      prevalance = NA,
                                      size = 0,
                                      size2 = 0,
                                      shape = "none",
                                      label.dist = 0,
                                      label.degree = 0,
                                      extincion = 0,
                                      coord.x = X,
                                      coord.y = Yc,
                                      color = NA)
            
            mutation_vertex <- data.frame(names = mutation_name, 
                                          branch_level = NA,
                                          branch = NA,
                                          label = ifelse(test = labels_show %in% c("both", "mutations"), yes = mutation_name, no = ""),
                                          last_mutation = "",
                                          TP = NA,#cl_df$cl_vertex$TP[cl_vertex_idx_Vc],
                                          clone = cl_df$cl_vertex$clone[cl_vertex_idx_Vc],
                                          prevalance = NA,
                                          size = 6,
                                          size2 = 6,
                                          shape = "square",
                                          label.dist = label_offset,
                                          label.degree = 0,
                                          extincion = 0,
                                          coord.x = X,
                                          coord.y = (Yc + Y)*0.5,
                                          color = cl_df$cl_vertex$color[cl_df$cl_vertex$names == Vn_name])
            
            cl_df$cl_edges <- cl_df$cl_edges[-idx_row_edges,]
            cl_df$cl_edges <- rbind(cl_df$cl_edges, clone_fake_edge, fake_mutation_edge, mutation_clone_edge)
            
            cl_df$cl_vertex <- rbind(cl_df$cl_vertex, fake_vertex, mutation_vertex)
            
        }
        cl_df <- recursiveLongitudinalLayout(idx_next_v, X, Y, cl_df, adjMatrix_base, adjMatrix_overall, mut_TP, labels_show, label_offset)
    }
    
    return(cl_df)
    
}
