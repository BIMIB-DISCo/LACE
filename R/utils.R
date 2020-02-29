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
