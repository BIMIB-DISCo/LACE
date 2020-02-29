#' Perform inference of the maximum likelihood clonal tree from longitudinal data.
#' @title LACE
#'
#' @examples
#' data(data)
#' inference = LACE(D = data,
#'                  lik_w = c(0.2308772,0.2554386,0.2701754,0.2435088),
#'                  alpha = list(c(0.10,0.05,0.05,0.05)),
#'                  beta = list(c(0.10,0.05,0.05,0.05)),
#'                  keep_equivalent = FALSE,
#'                  num_rs = 5,
#'                  num_iter = 10,
#'                  n_try_bs = 5,
#'                  num_processes = NA,
#'                  seed = 12345,
#'                  verbose = FALSE)
#'
#' @param D Mutation data from multiple experiments for a list of driver genes.
#' @param lik_w Weight for each data point. If not provided, weights to correct for sample sizes are used.
#' @param alpha False positive error rate provided as list of elements; if a vector of alpha (and beta) is provided, the inference is performed for multiple values and the solution at 
#' maximum-likelihood is returned.
#' @param beta False negative error rate provided as list of elements; if a vector of beta (and alpha) is provided, the inference is performed for multiple values and the solution at 
#' maximum-likelihood is returned.
#' @param initialization Starting point of the mcmc; if not provided, a random starting point is used.
#' @param keep_equivalent Boolean. Shall I return results (B and C) at equivalent likelihood with the best returned solution?
#' @param num_rs Number of restarts during mcmc inference.
#' @param num_iter Maximum number of mcmc steps to be performed during the inference.
#' @param n_try_bs Number of steps without change in likelihood of best solution after which to stop the mcmc.
#' @param learning_rate Parameter to tune the probability of accepting solutions at lower values during mcmc. Value of learning_rate = 1 (default), set a 
#' probability proportional to the difference in likelihood; values of learning_rate greater than 1 inclease the chance of accepting solutions at lower likelihood 
#' during mcmc while values lower than 1 decrease such probability.
#' @param marginalize Boolean. Shall I marginalize C when computing likelihood?
#' @param num_processes Number of processes to be used during parallel execution. To execute in single process mode, 
#' this parameter needs to be set to either NA or NULL.
#' @param seed Seed for reproducibility.
#' @param verbose Boolean. Shall I print to screen information messages during the execution?
#' @return A list of 8 elements: B, C, clones_prevalence, relative_likelihoods, joint_likelihood, clones_summary and error_rates. Here, B returns the maximum likelihood longitudinal 
#' clonal tree, C the attachment of cells to clones and clones_prevalence clones' prevalence; relative_likelihoods and joint_likelihood are respectively the likelihood of 
#' the solutions at each individual time points and the joint likelihood; clones_summary provide a summary of association of mutations to clones. In equivalent_solutions, solutions (B and C) 
#' with likelihood equivalent to the best solution are returned. Finally error_rates provides the best values of alpha and beta among the considered ones. 
#' @param log_file log file where to print outputs when using parallel. If parallel execution is disabled, this parameter is ignored.
#' @export LACE
#' @import parallel
#' @import Rfast
#'
LACE <- function( D, lik_w = NULL, alpha = NULL, beta = NULL, initialization = NULL, keep_equivalent = TRUE, num_rs = 50, num_iter = 10000, n_try_bs = 500, learning_rate = 1, marginalize = FALSE, num_processes = Inf, seed = NULL, verbose = TRUE, log_file = "" ) {
    
    # Set the seed
    set.seed(seed)

    # Initialize weights to compute weighted joint likelihood
    if(is.null(lik_w)) {
        total_sample_size <- 0
        for(i in 1:length(D)) {
            total_sample_size <- total_sample_size + nrow(D[[i]])
        }
        for(i in 1:length(D)) {
            lik_w <- c(lik_w,(1-(nrow(D[[i]])/total_sample_size)))
        }
        lik_w <- lik_w / sum(lik_w)
    }

    # Initialize error rates alpha and beta
    if(is.null(alpha)) {
        alpha = list(rep(0.01,length(D)))
        beta = list(rep(0.05,length(D)))
    }
    
    if(verbose) {
        cat(paste0("Starting inference for a total of ",length(alpha)," difference values of alpha and beta.","\n"))
    }

    # Setting up parallel execution
    parallel <- NULL
    close_parallel <- FALSE
    if(is.null(parallel)&&length(alpha)>1) {
        if(is.na(num_processes) || is.null(num_processes) || num_processes == 1) {
            parallel <- NULL
        }
        else if(num_processes==Inf) {
            cores <- as.integer((detectCores()-1))
            if(cores < 2) {
                parallel <- NULL
            }
            else {
                num_processes <- min(num_processes,length(alpha))
                parallel <- makeCluster(num_processes,outfile=log_file)
                close_parallel <- TRUE
            }
        }
        else {
            num_processes <- min(num_processes,length(alpha))
            parallel <- makeCluster(num_processes,outfile=log_file)
            close_parallel <- TRUE
        }
        
        if(verbose && !is.null(parallel)) {
            cat("Executing",num_processes,"processes via parallel...","\n")
        }
    }

    # Now start the inference
    if(is.null(parallel)) {

        # Sequential computation
        inference <- list()

        for(i in 1:length(alpha)) {

            inference[[i]] <- learn.longitudinal.phylogeny( D = D, 
                                                            lik_w = lik_w, 
                                                            alpha = alpha[[i]], 
                                                            beta = beta[[i]], 
                                                            initialization = initialization, 
                                                            keep_equivalent = keep_equivalent, 
                                                            num_rs = num_rs, 
                                                            num_iter = num_iter, 
                                                            n_try_bs = n_try_bs, 
                                                            learning_rate = learning_rate, 
                                                            marginalize = marginalize, 
                                                            seed = round(runif(1)*10000), 
                                                            verbose = verbose)
            
        }

    }
    else {

        # Parallel computation
        res_clusterEvalQ <- clusterEvalQ(parallel,library("Rfast"))
        clusterExport(parallel,varlist=c("D","lik_w","alpha","beta","initialization","keep_equivalent","num_rs","num_iter","n_try_bs","learning_rate","marginalize","verbose"),envir=environment())
        clusterExport(parallel,c("learn.longitudinal.phylogeny","initialize.B","move.B","compute.C"),envir=environment())
        clusterSetRNGStream(parallel,iseed=round(runif(1)*100000))
        inference <- parLapply(parallel,1:length(alpha),function(x) {
            
            if(verbose) {
                cat('Performing inference for alpha =',paste0(alpha[[x]],collapse=" | "),'and beta =',paste0(beta[[x]],collapse=" | "),'\n')
            }
            
            inference <- learn.longitudinal.phylogeny( D = D, 
                                                       lik_w = lik_w, 
                                                       alpha = alpha[[x]], 
                                                       beta = beta[[x]], 
                                                       initialization = initialization, 
                                                       keep_equivalent = keep_equivalent, 
                                                       num_rs = num_rs, 
                                                       num_iter = num_iter, 
                                                       n_try_bs = n_try_bs, 
                                                       learning_rate = learning_rate, 
                                                       marginalize = marginalize, 
                                                       seed = round(runif(1)*10000), 
                                                       verbose = FALSE)

        })
        
    }
    
    # Close parallel
    if(close_parallel) {
        stopCluster(parallel)
    }

    # Return the solution at maximum likelihood among the inferrend ones
    lik <- NULL
    for(i in 1:length(inference)) {
        lik <- c(lik,inference[[i]][["joint_lik"]])
    }
    best <- which(lik==max(lik))[1]
    equivalent_solutions <- inference[[best]][["equivalent_solutions"]]
    error_rates <- list(alpha=alpha[[best]],beta=beta[[best]])

    # Renaming
    B <- inference[[best]][["B"]]
    rownames(B) <- c("Root",paste0("Clone_",rownames(B)[2:nrow(B)]))
    colnames(B) <- c("Root",colnames(D[[1]])[as.numeric(colnames(B)[2:ncol(B)])])
    C <- inference[[best]][["C"]]
    names(C) <- paste0("Experiment_",1:length(C))
    for(i in 1:length(C)) {
        tmp <- matrix(C[[i]],ncol=1)
        rownames(tmp) <- rownames(D[[i]])
        colnames(tmp) <- "Clone"
        C[[i]] <- tmp
    }
    relative_likelihoods <- inference[[best]][["lik"]]
    names(relative_likelihoods) <- paste0("Experiment_",1:length(relative_likelihoods))
    joint_likelihood <- inference[[best]][["joint_lik"]]

    # Compute clones' prevalence
    clones_prevalence <- array(NA,c((dim(B)[1]-1),(length(C)+1)))
    rownames(clones_prevalence) <- rownames(B)[2:nrow(B)]
    colnames(clones_prevalence) <- c(paste0("Experiment_",1:length(C)),"Total")
    total_number_cell <- length(unlist(C))
    for(i in 1:dim(clones_prevalence)[1]) {
        clone_number_cell <- 0
        for(j in 1:length(C)) {
            clone_number_cell <- clone_number_cell + length(which(C[[j]]==i))
            clones_prevalence[i,j] <- length(which(C[[j]]==i)) / dim(C[[j]])[1]
        }
        clones_prevalence[i,"Total"] <- clone_number_cell / total_number_cell
    }

    # Finally compute clones' summary
    clones_summary <- list()
    Bwor <- B[,-1]
    i = 2
    for(c in rownames(Bwor[-1,])) {
        mut_list <- colnames(Bwor)[Bwor[i,]==1]
        clones_summary[[c]] <- mut_list
        i = i + 1
    }

    return(list(B=B,C=C,clones_prevalence=clones_prevalence,relative_likelihoods=relative_likelihoods,joint_likelihood=joint_likelihood,clones_summary=clones_summary,equivalent_solutions=equivalent_solutions,error_rates=error_rates))

}
