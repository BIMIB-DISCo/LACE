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
    
    adj_matrix <- array(0,c((dim(B)[1]-1),(dim(B)[2]-1)))
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