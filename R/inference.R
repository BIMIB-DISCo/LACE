# Main function for phylogeny reconstruction with longitudinal data
learn.longitudinal.phylogeny <- function( D, 
                                          lik_w = rep(1/length(D),length(D)), 
                                          alpha = 10^-3, 
                                          beta = 10^-2, 
                                          initialization = NULL, 
                                          keep_equivalent = FALSE, 
                                          num_rs = 50, 
                                          num_iter = 10000, 
                                          n_try_bs = 500, 
                                          learning_rate = 1, 
                                          marginalize = FALSE, 
                                          error_move = FALSE, 
                                          num_processes = 1, 
                                          seed = NULL, 
                                          verbose = TRUE, 
                                          log_file = "") {
    
    # Set the seed
    set.seed(seed)
    
    # Do we have datasets with different error rates?
    if(length(alpha)>1) {
        different.error.rates <- TRUE
    } else {
        different.error.rates <- FALSE
    }

    # First of all, we remove any NA value from data
    for(i in 1:length(D)) {
        if(any(is.na(D[[i]]))) {
            D[[i]][which(is.na(D[[i]]),arr.ind=TRUE)] <- -3
        }
        storage.mode(D[[i]]) <- "integer"
    }
    
    equivalent_solutions <- list()
    
    if(num_processes > 1) {
        
        parallel_cl <- makeCluster(num_processes,outfile=log_file)
        
        clusterExport(parallel_cl,varlist=c("D","lik_w","alpha","beta","initialization","different.error.rates","keep_equivalent","num_rs","num_iter","n_try_bs","learning_rate","marginalize","error_move","verbose","log_file"),envir=environment())
        clusterExport(parallel_cl,c("MCMC","initialize.B","move.B","relabeling", "prune.and.reattach", "compute.C"),envir=environment())
        clusterSetRNGStream(parallel_cl,iseed=round(runif(1)*100000))
        
        mcmc_res <- parLapply(parallel_cl,1:num_rs,function(x) { 
            
            MCMC(D = D, 
                 lik_w = lik_w,
                 alpha = alpha,
                 beta = beta,
                 different.error.rates = different.error.rates,
                 initialization = initialization,
                 keep_equivalent = keep_equivalent, 
                 num_rs = num_rs, 
                 num_iter = num_iter,
                 n_try_bs = n_try_bs,
                 learning_rate = learning_rate,
                 marginalize = marginalize,
                 error_move = error_move,
                 verbose = verbose,
                 rs_i = x,
                 log_file = log_file
            )
            
        })
        
        stopCluster(parallel_cl)
        
    } else {
        
        # Repeat for num_rs number of restarts
        mcmc_res <- list()
        
        for(i in 1:num_rs) {
            
            mcmc_res[[i]] <- MCMC(D = D, 
                                  lik_w = lik_w,
                                  alpha = alpha,
                                  beta = beta,
                                  different.error.rates = different.error.rates,
                                  initialization = initialization,
                                  keep_equivalent = keep_equivalent, 
                                  num_rs = num_rs,
                                  num_iter = num_iter,
                                  n_try_bs = n_try_bs,
                                  learning_rate = learning_rate,
                                  marginalize = marginalize,
                                  error_move = error_move,
                                  verbose = verbose,
                                  rs_i = i,
                                  log_file = log_file)
            
        }
    }

    if(num_rs > 1) {
        best_joint_lik <- c()
        
        for(i in  1:num_rs) {
            best_joint_lik <- c(best_joint_lik, mcmc_res[[i]]$joint_lik)
        }
        idx_bjl <- which(best_joint_lik == max(best_joint_lik))
        
        best_mcmc <- mcmc_res[[idx_bjl[1]]]
        for(i in 2:length(idx_bjl)) {
            best_mcmc$equivalent_solutions <- c(best_mcmc$equivalent_solutions,mcmc_res[[idx_bjl[i]]]$equivalent_solutions)
        }
        
    }
    else {
        best_mcmc <-  mcmc_res[[1]]
    }

    return(best_mcmc)

}

# perform MCMC
MCMC <- function(D, 
                 lik_w = rep(1/length(D),length(D)), 
                 alpha = 10^-3, 
                 beta = 10^-2, 
                 different.error.rates = FALSE,
                 initialization = NULL, 
                 keep_equivalent = FALSE, 
                 num_iter = 10000,
                 num_rs = 50, 
                 n_try_bs = 500, 
                 learning_rate = 1, 
                 marginalize = FALSE, 
                 error_move = FALSE, 
                 rs_i = NULL,
                 verbose = TRUE, 
                 log_file = "" ) {

    # initialize B
    if(is.null(initialization)) {
        B <- initialize.B(D)
        
    }
    else {
        B <- initialization
        storage.mode(B) <- "integer"
        if(rs_i > 1) {
            # Restars after the first one start from a different point if 
            # a initial tree was passed (it could be generated by TRAIT or by user)
            for(i in seq(1,ncol(B))) {
                B <- move.B(B=B,alpha=alpha,beta=beta,error_move=error_move)$B
            }
        }
    }

    res <- compute.C(B,D,lik_w,alpha,beta,different.error.rates,marginalize)
    C <- res$C
    lik <- res$lik
    joint_lik <- res$joint_lik

    # Initialize result variables (best solution for the current restart)
    B_best <- B
    C_best <- C
    alpha_best <- alpha
    beta_best <- beta
    lik_best <- lik
    joint_lik_best <- joint_lik
    count_lik_best_cons <- 0
    equivalent_solutions <- list()
    if(keep_equivalent) {
        equivalent_solutions[[1]] <- list(B=B_best,C=C_best,alpha=alpha_best,beta=beta_best,relative_likelihoods=lik_best,joint_likelihood=joint_lik_best)
    }
    
    # Repeat until num_iter number of iterations is reached
    j = 1
    while(j <= num_iter) {
        
        if(verbose && (j %% 500)==0) {
            
            cat("\r",
                "Current best lik. = ",format(joint_lik_best, digit = 2, nsmall = 2), 
                " | Restart # ",rs_i,"/",num_rs," | Iter # ",j, " | Likelihood not improved for ", count_lik_best_cons,"/",n_try_bs," iterations",
                "     ", 
                sep='', 
                file = log_file, 
                append = TRUE)
            
        }
        
        # Try move on B
        move_tmp <- move.B(B=B,alpha=alpha,beta=beta,error_move=error_move)
        B_tmp <- move_tmp$B
        alpha_tmp <- move_tmp$alpha
        beta_tmp <- move_tmp$beta
        
        # Compute C at maximun likelihood given B_tmp and returns its likelihood
        res <- compute.C(B_tmp,D,lik_w,alpha_tmp,beta_tmp,different.error.rates,marginalize)
        C_tmp <- res$C
        lik_tmp <- res$lik
        joint_lik_tmp <- res$joint_lik

        # If likelihood at current step is better than best likelihood, replace best model with current
        if(joint_lik_tmp > joint_lik_best) {
            
            B_best <- B_tmp
            C_best <- C_tmp
            alpha_best <- alpha_tmp
            beta_best <- beta_tmp
            lik_best <- lik_tmp
            joint_lik_best <- joint_lik_tmp
            count_lik_best_cons <- 0
            
            B <- B_tmp
            C <- C_tmp
            alpha <- alpha_tmp
            beta <- beta_tmp
            lik <- lik_tmp
            joint_lik <- joint_lik_tmp
            equivalent_solutions <- list()
            if(keep_equivalent) {
                equivalent_solutions[[1]] <- list(B=B_best,C=C_best,alpha=alpha_best,beta=beta_best,relative_likelihoods=lik_best,joint_likelihood=joint_lik_best)
            }
            
        }
        else {
            
            if(joint_lik_tmp == joint_lik_best) {
                if(keep_equivalent) {
                    equivalent_solutions[[(length(equivalent_solutions)+1)]] <- list(B=B_tmp,C=C_tmp,alpha=alpha_tmp,beta=beta_tmp,relative_likelihoods=lik_tmp,joint_likelihood=joint_lik_tmp)
                }
            }
            
            count_lik_best_cons <- count_lik_best_cons + 1
            if(count_lik_best_cons > n_try_bs) {
                
                # Skipping to the next restart
                if(verbose) {
                    cat("\r",
                        "Current best lik. = ",format(joint_lik_best, digit = 2, nsmall = 2), 
                        " | Restart # ",rs_i,"/",num_rs," | Iter # ",j, " | Likelihood not improved for ", (count_lik_best_cons-1),"/",n_try_bs," iterations",
                        "     ", 
                        "\n",
                        sep='')
                }
                break
                
            }
            
            # Take the current state with a probability proportional to the ratio of the two likelihoods
            rho <- min(exp(((joint_lik_tmp-joint_lik)/learning_rate)),1)
            if(runif(1)<=rho) {
                
                B <- B_tmp
                C <- C_tmp
                alpha <- alpha_tmp
                beta <- beta_tmp
                lik <- lik_tmp
                joint_lik <- joint_lik_tmp
                
            }
            
        }
        j = j + 1
    }
    
    return(list(B=B_best,C=C_best,alpha=alpha_best,beta=beta_best,lik=lik_best,joint_lik=joint_lik_best,equivalent_solutions=equivalent_solutions))
    
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
move.B <- function( B, alpha, beta, error_move = FALSE, seed = NULL ) {
    
    # Random probability of choosing a move
    p <- runif(1)
    
    # With no estimation of error rates within the MCMC
    if(error_move==FALSE) {
        # Perform pairwise relabeling with 55% probability
        if(p<0.55) {
            
            B <- relabeling(B=B)
            
        }
        # Perform structural moves with 40% probability
        else if(p>=0.55&&p<0.95) {
            
            B <- prune.and.reattach(B=B)
            
        }
        
        # Perform full relabeling with 5% probability
        else if(p>=0.95) {
            
            # Random relabeling of all clones
            colnames(B) <- c('r',sample(1:(ncol(B)-1)))
            
        }
    }
    # With estimation of error rates within the MCMC
    else {
        # Perform moves on error rates with 10% probability
        if(p<0.10) {
            
            for(err_val in 1:length(alpha)) {
                alpha[[err_val]] <- min(max(rnorm(1,mean=alpha[[err_val]],sd=(alpha[[err_val]]/3)),0.0001),0.9999)
                beta[[err_val]] <- min(max(rnorm(1,mean=beta[[err_val]],sd=(beta[[err_val]]/3)),0.0001),0.9999)
            }
            
        }
        # Perform pairwise relabeling with 50% probability
        else if(p>=0.10&&p<0.60) {
            
            # Relabeling
            B <- relabeling(B=B)
            
        }
        # Perform structural moves with 35% probability
        else if(p>=0.60&&p<0.95) {
            
            B <- prune.and.reattach(B=B)
            
        }
        # Perform full relabeling with 5% probability
        else if(p>=0.95) {
            
            # Random relabeling of all clones
            colnames(B) <- c('r',sample(1:(ncol(B)-1)))
            
        }
    }
    
    return(list(B=B,alpha=alpha,beta=beta))
    
}

# perform node relabeling
relabeling <- function(B) {
    
    # Relabeling
    chosen <- sample(2:ncol(B),2,replace=FALSE)
    tmp <- colnames(B)[chosen[1]]
    colnames(B)[chosen[1]] <- colnames(B)[chosen[2]]
    colnames(B)[chosen[2]] <- tmp
    return(B)
    
}

# perform prune and reattach
prune.and.reattach <- function(B) {
    
    # Change one arch
    is_not_valid <- TRUE
    while(is_not_valid) {
        
        # Select source node
        ch_1 <- sample(x=2:nrow(B),size=1)
        ch_1_gen <- B[ch_1,1:ch_1]
        remaing_node <- as.numeric(which(apply(B[,1:ch_1], c(1), FUN = function(x){!all(x == ch_1_gen)})))
        
        # Chose the target node from the nodes not included in the subtree where ch_1 is the root
        if(length(remaing_node) > 1) {
            
            ch_2 <- sample(x=remaing_node,size=1)
            
        } else if(length(remaing_node)==1) {
            
            ch_2 <- remaing_node
            
        } else {
            # if there aren't any nodes, select a different source
            next
        }

        # A pair of two nodes is valid if the nodes are not already directly connected
        if(!(all(B[ch_1,1:ch_2]==B[ch_2,1:ch_2])&sum(B[ch_1,])==(sum(B[ch_2,])+1))) {
            
            is_not_valid <- FALSE
        }
        
    }
    
    descendent_nodes <- setdiff(1:nrow(B), remaing_node)
    
    # Extract descendent node submatrix
    rem_B <- B[remaing_node,remaing_node,drop=FALSE]
    new_B <- matrix(data = 0L, nrow = nrow(rem_B), ncol = ncol(B))
    colnames(new_B) <- c(colnames(B)[remaing_node], colnames(B)[descendent_nodes])
    new_B[1:length(remaing_node),1:length(remaing_node)] <- rem_B
    gen_ch_2 <- new_B[which(colnames(new_B)==colnames(B)[ch_2]),1:length(remaing_node)]
    desc_B <- cbind(matrix(rep(gen_ch_2,each=length(descendent_nodes)),  nrow = length(descendent_nodes)), 
                    B[descendent_nodes,descendent_nodes,drop=FALSE])
    new_B <- rbind(new_B,desc_B)
    
    return(new_B)
    
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
        curr_cells_D <- cbind(rep(1,nrow(D[[i]][,idx_srt,drop=FALSE])),D[[i]][,idx_srt,drop=FALSE])
        
        # Find assignment at maximum log likelihood for current cell
        lik_matrix <- array(0L,c(nrow(D[[i]]),ncol(B)))
        
        for(k in 1:nrow(B)) {
            
            curr_clone_C = matrix(rep(0L,nrow(B)),nrow=1)
            curr_clone_C[1,k] <- 1L
            
            # Save mapping between ordering in B and dataset D
            # Factorization D = C dot B. r_D_tilde represents a combination of mutations and clones
            r_D_tilde <- (curr_clone_C %*% B)*2
            
            sum_cell_clone <- as.matrix(sweep(curr_cells_D,MARGIN=2,r_D_tilde,"+"))
            lik_matrix[,k] <- (curr_beta^Rfast::rowsums(sum_cell_clone==2)) * ((1-curr_beta)^Rfast::rowsums(sum_cell_clone==0)) * ((curr_alpha)^Rfast::rowsums(sum_cell_clone==1)) * ((1-curr_alpha)^Rfast::rowsums(sum_cell_clone==3))
            
        }
        
        C_list_time <- Rfast::rowMaxs(lik_matrix,value=FALSE)
        
        if(marginalize==TRUE) {
            lik_time <- sum(log(Rfast::rowsums(lik_matrix)))
        }
        else {
            lik_time <- sum(log(Rfast::rowMaxs(lik_matrix,value=TRUE)))
        }
        
        storage.mode(C_list_time) <- "integer"

        C_res[[i]] <- (C_list_time-1)
        lik_matrix_res[[i]] <- lik_matrix
        lik_res <- c(lik_res,lik_time)
        joint_lik_res <- joint_lik_res + (lik_time * lik_w[i])
        
    }
    
    return(list(C=C_res,lik=lik_res,joint_lik=joint_lik_res))
    
}
