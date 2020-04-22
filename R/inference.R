# Main function for phylogeny reconstruction with longitudinal data
learn.longitudinal.phylogeny <- function( D, lik_w = rep(1/length(D),length(D)), alpha = 10^-3, beta = 10^-2, initialization = NULL, keep_equivalent = TRUE, num_rs = 50, num_iter = 10000, n_try_bs = 500, learning_rate = 1, marginalize = FALSE, seed = NULL, verbose = TRUE ) {
    
    # Set the seed
    set.seed(seed)

    # Do we have datasets with different error rates?
    different.error.rates <- FALSE
    if(length(alpha)>1) {
        different.error.rates <- TRUE
    }
    
    # Initialize global result variables (best solution among all restarts)
    B_global <- NULL
    C_global <- NULL
    lik_global <- NULL
    joint_lik_global <- NULL
    equivalent_solutions_global <- list()
    
    # First of all, we remove any NA value from data
    for(i in 1:length(D)) {
        if(any(is.na(D[[i]]))) {
            D[[i]][which(is.na(D[[i]]),arr.ind=TRUE)] <- -3
        }
        storage.mode(D[[i]]) <- "integer"
    }
    
    # Repeat for num_rs number of restarts
    for(i in 1:num_rs) {
        
        if(verbose) {
            cat(paste0("Performing restart number ",as.character(as.integer(i))," out of ",as.character(as.integer(num_rs))), "\n")
        }
        
        # Initialize B
        if(i==1) {
            
            if(is.null(initialization)) {
                B <- initialize.B(D,seed=round(runif(1)*10000))
            }
            else {
                B <- initialization
                storage.mode(B) <- "integer"
            }
            
        }
        else {
            
            # Random relabeling of all clones
            B <- B_global
            colnames(B) <- c('r',sample(1:(ncol(B)-1)))
            
        }
        
        res <- compute.C(B,D,lik_w,alpha,beta,different.error.rates,marginalize)
        C <- res$C
        lik <- res$lik
        joint_lik <- res$joint_lik
        
        # Initialize result variables (best solution for the current restart)
        B_best <- B
        C_best <- C
        lik_best <- lik
        joint_lik_best <- joint_lik
        count_lik_best_cons <- 0
        if(keep_equivalent) {
            equivalent_solutions <- list()
            equivalent_solutions[[1]] <- list(B=B_best,C=C_best,relative_likelihoods=lik_best,joint_likelihood=joint_lik_best)
        }
        
        # Repeat until num_iter number of iterations is reached
        for(j in 1:num_iter) {
            
            if(verbose && (j %% 100)==0) {
                cat(paste0("Performed iteration number ",as.character(as.integer(j))," out of ",as.character(as.integer(num_iter))," | Current best log likelihood ",joint_lik_best,"\n"))
            }
            
            # Try move on B
            B_tmp <- move.B(B,seed=round(runif(1)*10000))
            
            # Compute C at maximun likelihood given B_tmp and returns its likelihood
            res <- compute.C(B_tmp,D,lik_w,alpha,beta,different.error.rates,marginalize)
            C_tmp <- res$C
            lik_tmp <- res$lik
            joint_lik_tmp <- res$joint_lik
            
            # If likelihood at current step is better than best likelihood, replace best model with current
            if(joint_lik_tmp > joint_lik_best) {
                
                B_best <- B_tmp
                C_best <- C_tmp
                lik_best <- lik_tmp
                joint_lik_best <- joint_lik_tmp
                count_lik_best_cons <- 0
                B <- B_tmp
                C <- C_tmp
                lik <- lik_tmp
                joint_lik <- joint_lik_tmp
                if(keep_equivalent) {
                    equivalent_solutions <- list()
                    equivalent_solutions[[1]] <- list(B=B_best,C=C_best,relative_likelihoods=lik_best,joint_likelihood=joint_lik_best)
                }
                
            }
            else {

                if(joint_lik_tmp == joint_lik_best) {
                    if(keep_equivalent) {
                        equivalent_solutions[[(length(equivalent_solutions)+1)]] <- list(B=B_tmp,C=C_tmp,relative_likelihoods=lik_tmp,joint_likelihood=joint_lik_tmp)
                    }
                }
                
                count_lik_best_cons <- count_lik_best_cons + 1
                if(count_lik_best_cons > n_try_bs) {
                    
                    # Warning message
                    if(verbose) {
                        cat(paste0("Not improving likelihood of best solution after ",as.character(as.integer(n_try_bs)), " iterations. Skipping to next restart.\n"))
                    }
                    break
                    
                }
                
                # Take the current state with a probability proportional to the ratio of the two likelihoods
                rho <- min(exp(((joint_lik_tmp-joint_lik)/learning_rate)),1)
                if(runif(1)<=rho) {
                    
                    B <- B_tmp
                    C <- C_tmp
                    lik <- lik_tmp
                    joint_lik <- joint_lik_tmp
                    
                }
                
            }
            
        }
        
        if(is.null(joint_lik_global) || (joint_lik_best>joint_lik_global)) {
            
            B_global <- B_best
            C_global <- C_best
            lik_global <- lik_best
            joint_lik_global <- joint_lik_best
            if(keep_equivalent) {
                equivalent_solutions_global <- equivalent_solutions
            }
            
        }
        
    }
    
    return(list(B=B_global,C=C_global,lik=lik_global,joint_lik=joint_lik_global,equivalent_solutions=equivalent_solutions_global))
    
}

# Initialize B
initialize.B <- function( D, seed = NULL ) {
    
    # Define number of mutations
    m <- ncol(D[[1]])
    
    # Initialize B
    B <- array(0L,c((m+1),(m+1)))
    rownames(B) <- c('r',1:m)
    colnames(B) <- c('r',sample(1:m))
    diag(B) <- 1L
    B[,1] <- 1L
    
    # Add arcs with probability 50%
    p <- 0.50
    for(i in 2:(nrow(B)-1)) {
        
        if(runif(1)<p) {
            B[(i+1),] <- B[i,] + B[(i+1),]
        }
        
    }
    
    B[which(B>1)] <- 1L
    
    return(B)
    
}

# Performing either relabeling or edge changing moves on B
move.B <- function( B, seed = NULL ) {
    
    # Random probability of choosing a move
    p <- runif(1)
    
    # Perform pairwise relabeling with 55% probability
    if(p<0.55) {
        
        # Relabeling
        chosen <- sample(2:ncol(B),2,replace=FALSE)
        tmp <- colnames(B)[chosen[1]]
        colnames(B)[chosen[1]] <- colnames(B)[chosen[2]]
        colnames(B)[chosen[2]] <- tmp
        
    }
    # Perform structural moves with 40% probability
    else if(p>=0.55&&p<0.95) {
        
        # Change one arch
        is_not_valid <- TRUE
        while(is_not_valid) {
            ch_1 <- sample(3:nrow(B),1)
            ch_2 <- sample(1:(ch_1-1),1)
            
            # A pair of two nodes is valid if the nodes are not already directly connected
            if(!(all(B[ch_1,1:ch_2]==B[ch_2,1:ch_2])&&sum(B[ch_1,])==(sum(B[ch_2,])+1))) {
                
                is_not_valid <- FALSE
            }
        }
        
        # Performing move on ch_1
        ch_1_bkp <- B[ch_1,1:ch_1]
        B[ch_1,1:(ch_1-1)] <- c(1L,rep(0L,(ch_1-2)))
        B[ch_1,] <- B[ch_1,] + B[ch_2,]
        
        # Performing move on children of ch_1
        if(ch_1 != nrow(B)) {
            
            for(i in (ch_1+1):nrow(B)) {
                
                if(all(ch_1_bkp==B[i,1:ch_1])) {
                    
                    B[i,1:(ch_1-1)] <- c(1L,rep(0L,(ch_1-2)))
                    B[i,] <- B[i,] + B[ch_2,]
                    
                }
                
            }
            
        }
        
        B[which(B>1)] <- 1L
        
    }
    # Perform full relabeling with 5% probability
    else if(p>=0.95) {
        
        # Random relabeling of all clones
        colnames(B) <- c('r',sample(1:(ncol(B)-1)))
        
    }
    
    return(B)

}

# Compute attachments matrix C at maximum likelihood given B, D and P
compute.C <- function( B, D, lik_w = rep(1/length(D),length(D)), alpha = 10^-3, beta = 10^-2, different.error.rates = FALSE, marginalize = FALSE ) {
    
    # Initialize return variables
    C_res <- list()
    lik_matrix_res <- list()
    lik_res <- NULL
    joint_lik_res <- 0.0
    
    # Determine indeces to order D such that it matches B
    idx_srt <- as.integer(colnames(B)[2:ncol(B)])
    
    # For each time point
    curr_alpha <- alpha
    curr_beta <- beta
    for(i in 1:length(D)) {

        if(different.error.rates) {
            curr_alpha <- alpha[i]
            curr_beta <- beta[i]
        }
        
        # Initialize C for the current time point
        lik_time <- 0
        
        # Go through all the cells
        curr_cells_D <- D[[i]][,idx_srt,drop=FALSE]
        
        # Find assignment at maximum log likelihood for current cell
        lik_matrix <- array(0L,c(nrow(D[[i]]),(ncol(B)-1)))
        
        for(k in 2:nrow(B)) {
            
            curr_clone_C = matrix(rep(0L,(nrow(B)-1)),nrow=1)
            curr_clone_C[1,(k-1)] <- 1L
            
            # Save mapping between ordering in B and dataset D
            # Factorization D = C dot B. r_D_tilde represents a combination of mutations and clones
            r_D_tilde <- (curr_clone_C %*% B[-1,-1,drop=FALSE])*2
            
            sum_cell_clone <- as.matrix(sweep(curr_cells_D,MARGIN=2,r_D_tilde,"+"))
            lik_matrix[,(k-1)] <- (curr_beta^rowSums(sum_cell_clone==2)) * ((1-curr_beta)^rowSums(sum_cell_clone==0)) * ((curr_alpha)^rowSums(sum_cell_clone==1)) * ((1-curr_alpha)^rowSums(sum_cell_clone==3))
            
        }
        
        C_list_time <- rowMaxs(lik_matrix,value=FALSE)
        if(marginalize==TRUE) {
            lik_time <- sum(log(rowMeans(lik_matrix)))
        }
        else {
            lik_time <- sum(log(rowMaxs(lik_matrix,value=TRUE)))
        }
        
        storage.mode(C_list_time) <- "integer"
        C_res[[i]] <- C_list_time
        lik_matrix_res[[i]] <- lik_matrix
        lik_res <- c(lik_res,lik_time)
        joint_lik_res <- joint_lik_res + (lik_time * lik_w[i])
        
    }
    
    return(list(C=C_res,lik=lik_res,joint_lik=joint_lik_res))
    
}
