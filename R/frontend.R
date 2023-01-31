#' Perform the inference of the maximum likelihood clonal tree from longitudinal data.
#' @title LACE
#'
#' @examples
#' data(longitudinal_sc_variants)
#' inference = LACE(D = longitudinal_sc_variants,
#'                  lik_w = c(0.2308772,0.2554386,0.2701754,0.2435088),
#'                  alpha = list(c(0.10,0.05,0.05,0.05)),
#'                  beta = list(c(0.10,0.05,0.05,0.05)),
#'                  keep_equivalent = TRUE,
#'                  num_rs = 5,
#'                  num_iter = 10,
#'                  n_try_bs = 5,
#'                  num_processes = NA,
#'                  seed = 12345,
#'                  verbose = FALSE,
#'                  show = FALSE)
#'
#' @param D Mutation data from multiple experiments for a list of driver genes. It can be either a list with a data matrix per time point or a SummarizedExperiment object.
#' In this latter, the object must contain two fields: assays and colData. Assays stores one unique data matrix pooling all single cells observed at each time point and colData stores a vector of labels reporting the time point when each single cell was sequenced.
#' Ordering of cells in assays field and colData field must be the same.
#' @param lik_w Weight for each data point. If not provided, weights to correct for sample sizes are used.
#' @param alpha False positive error rate provided as list of elements; if a vector of alpha (and beta) is provided, the inference is performed for multiple values and the solution at
#' maximum-likelihood is returned.
#' @param beta False negative error rate provided as list of elements; if a vector of beta (and alpha) is provided, the inference is performed for multiple values and the solution at
#' maximum-likelihood is returned.
#' @param initialization Binary matrix representing a perfect philogeny clonal tree; clones are rows and mutations are columns.
#' This parameter overrides "random_tree".
#' @param random_tree Boolean. Shall I start MCMC search from a random tree? If FALSE (default) and initialization is NULL, search
#' is started from a TRaIT tree (BMC Bioinformatics . 2019 Apr 25;20(1):210.  doi: 10.1186/s12859-019-2795-4).
#' @param keep_equivalent Boolean. Shall I return results (B and C) at equivalent likelihood with the best returned solution?
#' @param check_indistinguishable Boolean. Shall I remove any indistinguishable event from input data prior inference?
#' @param num_rs Number of restarts during mcmc inference.
#' @param num_iter Maximum number of mcmc steps to be performed during the inference.
#' @param n_try_bs Number of steps without change in likelihood of best solution after which to stop the mcmc.
#' @param learning_rate Parameter to tune the probability of accepting solutions at lower values during mcmc. Value of learning_rate = 1 (default), set a
#' probability proportional to the difference in likelihood; values of learning_rate greater than 1 inclease the chance of accepting solutions at lower likelihood
#' during mcmc while values lower than 1 decrease such probability.
#' @param marginalize Boolean. Shall I marginalize C when computing likelihood?
#' @param error_move Boolean. Shall I include estimation of error rates in the MCMC moves?
#' @param num_processes Number of processes to be used during parallel execution. To execute in single process mode,
#' this parameter needs to be set to either NA or NULL.
#' @param seed Seed for reproducibility.
#' @param verbose Boolean. Shall I print to screen information messages during the execution?
#' @param log_file log file where to print outputs when using parallel. If parallel execution is disabled, this parameter is ignored.
#' @param show Boolean. Show the interactive interface to explore the output.
#'
#' @return A list of 9 elements: B, C, clones_prevalence, relative_likelihoods, joint_likelihood, clones_summary and error_rates. Here, B returns the maximum likelihood longitudinal
#' clonal tree, C the attachment of cells to clones, corrected_genotypes the corrected genotypes and clones_prevalence clones' prevalence; relative_likelihoods and joint_likelihood are respectively
#' the likelihood of the solutions at each individual time points and the joint likelihood; clones_summary provide a summary of association of mutations to clones. In equivalent_solutions,
#' solutions (B and C) with likelihood equivalent to the best solution are returned. Finally error_rates provides the best values of alpha and beta among the considered ones.
#' @export LACE
#' @import parallel
#' @rawNamespace import(SummarizedExperiment, except = c("trim","shift", "show"))
# #' @import SummarizedExperiment
#' @import data.tree
#' @importFrom Rfast rowMaxs
#' @importFrom stats runif rnorm
#'
LACE <- function( D,
                  lik_w = NULL,
                  alpha = NULL,
                  beta = NULL,
                  initialization = NULL,
                  random_tree = FALSE,
                  keep_equivalent = TRUE,
                  check_indistinguishable = TRUE,
                  num_rs = 50,
                  num_iter = 10000,
                  n_try_bs = 500,
                  learning_rate = 1,
                  marginalize = FALSE,
                  error_move = FALSE,
                  num_processes = Inf,
                  seed = NULL,
                  verbose = TRUE,
                  log_file = "",
                  show = TRUE ) {

    # Set the seed
    set.seed(seed)

    # Handle SummarizedExperiment objects
    if(typeof(D)=="S4") {
        curr_data <- t(assays(D)[[1]])
        curr_experiment <- colData(D)[[1]]
        curr_experiment_unique <- unique(curr_experiment)
        curr_D <- list()
        for(i in 1:length(curr_experiment_unique)) {
            curr_D[[as.character(curr_experiment_unique[i])]] <- curr_data[which(curr_experiment==curr_experiment_unique[i]),,drop=FALSE]
        }
        D <- curr_D
    }

    # Set storage mode to integer
    for(i in 1:length(D)) {
        storage.mode(D[[i]]) <- "integer"
    }

    # Remove any indistinguishable event from input data prior inference
    if(check_indistinguishable) {
        D <- check.indistinguishable(D)
    }

    # If initiatilation is NULL and we do not require a random tree, we initializa with TRaIT tree
    if(is.null(initialization) && !random_tree) {

        # set initial tree from where to start MCMC search
        data <- Reduce(rbind,D)
        data[which(is.na(data))] <- 0
        marginal_probs <- matrix(colSums(data,na.rm=TRUE)/nrow(data),ncol=1)
        rownames(marginal_probs) <- colnames(data)
        colnames(marginal_probs) <- "Frequency"
        joint_probs <- array(NA,c(ncol(data),ncol(data)))
        rownames(joint_probs) <- colnames(data)
        colnames(joint_probs) <- colnames(data)
        for (i in seq_len(ncol(data))) {
            for (j in seq_len(ncol(data))) {
                val1 <- data[,i]
                val2 <- data[,j]
                joint_probs[i,j] <- (t(val1)%*%val2)
            }
        }
        joint_probs <- joint_probs/nrow(data)
        adjacency_matrix <- array(0,c(ncol(data),ncol(data)))
        rownames(adjacency_matrix) <- colnames(data)
        colnames(adjacency_matrix) <- colnames(data)
        pmi <- joint_probs
        for(i in seq_len(nrow(pmi))) {
            for(j in seq_len(ncol(pmi))) {
                pmi[i,j] <- log(joint_probs[i,j]/(marginal_probs[i,"Frequency"]*marginal_probs[j,"Frequency"]))
            }
        }
        ordering <- names(sort(marginal_probs[,"Frequency"],decreasing=TRUE))
        adjacency_matrix <- adjacency_matrix[ordering,ordering]
        adjacency_matrix[1,2] = 1
        if(nrow(adjacency_matrix)>2) {
            for(i in 3:nrow(adjacency_matrix)) {
                curr_c <- rownames(adjacency_matrix)[i]
                curr_candidate_p <- rownames(adjacency_matrix)[seq_len((i-1))]
                adjacency_matrix[names(which.max(pmi[curr_candidate_p,curr_c]))[1],curr_c] <- 1
            }
        }
        adjacency_matrix <- rbind(rep(0,nrow(adjacency_matrix)),adjacency_matrix)
        adjacency_matrix <- cbind(rep(0,nrow(adjacency_matrix)),adjacency_matrix)
        adjacency_matrix[1,2] = 1
        initialization <- as.B.trait(adj_matrix=adjacency_matrix,D=D[[1]])

    }

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
        cat(paste0("Starting inference for a total of ",length(alpha)," different values of alpha and beta.","\n"))
    }

    # Setting up parallel execution
    if(num_rs>1) {
        if(is.na(num_processes) || is.null(num_processes) || num_processes == 1) {
            num_processes = 1
        }
        else if(num_processes==Inf) {
            cores <- as.integer((detectCores()-1))
            if(cores < 2) {
                num_processes <- 1
            }
            else {
                num_processes <- min(cores,num_rs)
            }
        }
        else {
            num_processes <- min(num_processes,num_rs)
        }
        if(verbose && num_processes>1) {
            cat("Executing",num_processes,"processes via parallel...","\n")
        }
    }
    else {
        num_processes = 1
    }

    # Now start the inference

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
                                                        num_processes = num_processes,
                                                        error_move = error_move,
                                                        seed = round(runif(1)*10000),
                                                        verbose = verbose,
                                                        log_file = log_file)
    }

    # Return the solution at maximum likelihood among the inferrend ones
    lik <- NULL
    for(i in 1:length(inference)) {
        lik <- c(lik,inference[[i]][["joint_lik"]])
    }
    best <- which(lik==max(lik))[1]
    error_rates <- list(alpha=inference[[best]][["alpha"]],beta=inference[[best]][["beta"]])

    # compute corrected genotypes
    inference_B <- inference[[best]][["B"]]
    rownames(inference_B)[1] <- 0
    rownames(inference_B) <- (as.numeric(rownames(inference_B))+1)
    colnames(inference_B)[1] <- 0
    colnames(inference_B) <- (as.numeric(colnames(inference_B))+1)
    inference_attachments <- (unlist(inference[[best]][["C"]])+1)
    inference_attachments_ordered <- inference_attachments
    for(curr_C_index in 1:length(inference_attachments)) {
        inference_attachments_ordered[curr_C_index] <- as.integer(colnames(inference_B)[inference_attachments[curr_C_index]])
    }
    inference_attachments <- inference_attachments_ordered
    idx_B_curr <- sort.int(as.integer(colnames(inference_B)),index.return=TRUE)$ix
    inference_B <- inference_B[idx_B_curr,]
    inference_B <- inference_B[,idx_B_curr]
    inference_C <- array(0L,c(length(inference_attachments),ncol(inference_B)))
    for(curr_C_index in 1:nrow(inference_C)) {
        inference_C[curr_C_index,inference_attachments[curr_C_index]] <- 1L
    }
    corrected_genotypes <- inference_C %*% inference_B
    cells_names <- NULL
    for(cn in 1:length(D)) {
        cells_names <- c(cells_names,rownames(D[[cn]]))
    }
    rownames(corrected_genotypes) <- cells_names
    colnames(corrected_genotypes) <- c("Root",colnames(D[[1]]))
    corrected_genotypes <- corrected_genotypes[,-1,drop=FALSE]

    # Renaming
    B <- inference[[best]][["B"]]
    rownames(B) <- c("Root",paste0("Clone_",1:(nrow(B)-1)))
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

    # Include Root in clones_prevalence
    clones_prevalence <- rbind(rep(NA,ncol(clones_prevalence)),clones_prevalence)
    rownames(clones_prevalence)[1] <- "Root"
    clones_prevalence["Root",] <- (1-colSums(clones_prevalence,na.rm=TRUE))

    # Compute clones' summary
    clones_summary <- list()
    Bwor <- B[,-1]
    i = 2
    for(c in rownames(Bwor[-1,])) {
        mut_list <- colnames(Bwor)[Bwor[i,]==1]
        clones_summary[[c]] <- mut_list
        i = i + 1
    }

    # Finally, process equivalent solutions
    if(keep_equivalent == TRUE) {
        equivalent_solutions <- inference[[best]][["equivalent_solutions"]]

        eq_sol_df <- do.call(rbind,  lapply(equivalent_solutions, function(x) {
            adjM <- as.adj.matrix(x$B)
            return(as.vector(adjM))
        }))

        idx_uniq_sol <- which(!duplicated(eq_sol_df))

        equivalent_solutions <- equivalent_solutions[idx_uniq_sol]

        if(length(equivalent_solutions)==1) { # if we have only the best solution (no other equivalent solutions)
            equivalent_solutions <- list()
        } else {

            for(i in 1:length(equivalent_solutions)) {
                rownames(equivalent_solutions[[i]][["B"]]) <- c("Root",paste0("Clone_",rownames(equivalent_solutions[[i]][["B"]])[2:nrow(equivalent_solutions[[i]][["B"]])]))
                colnames(equivalent_solutions[[i]][["B"]]) <- c("Root",colnames(D[[1]])[as.numeric(colnames(equivalent_solutions[[i]][["B"]])[2:ncol(equivalent_solutions[[i]][["B"]])])])
            }
        }

    } else {

        equivalent_solutions <- NULL
    }

    longitudinal_tree <- NULL

    if(show==TRUE) {
        lace_interface(B, clones_prevalence, C, error_rates)
    }
    return(list(B=B,C=C,corrected_genotypes=corrected_genotypes,clones_prevalence=clones_prevalence,relative_likelihoods=relative_likelihoods,joint_likelihood=joint_likelihood,clones_summary=clones_summary,equivalent_solutions=equivalent_solutions,error_rates=error_rates, longitudinal_tree=longitudinal_tree))

}





#' Perform the inference of the maximum likelihood clonal tree from longitudinal data.
#' @title lacedata
#'
#' @examples
#' data(longitudinal_sc_variants)
#' lacedata(D = longitudinal_sc_variants,
#'          lik_w = c(0.2308772,0.2554386,0.2701754,0.2435088),
#'          alpha = list(c(0.10,0.05,0.05,0.05)),
#'          beta = list(c(0.10,0.05,0.05,0.05)),
#'          keep_equivalent = TRUE,
#'          num_rs = 5,
#'          num_iter = 10,
#'          n_try_bs = 5,
#'          num_processes = NA,
#'          seed = 12345,
#'          verbose = FALSE)
#'
#' @param D Mutation data from multiple experiments for a list of driver genes. It can be either a list with a data matrix per time point or a SummarizedExperiment object.
#' In this latter, the object must contain two fields: assays and colData. Assays stores one unique data matrix pooling all single cells observed at each time point and colData stores a vector of labels reporting the time point when each single cell was sequenced.
#' Ordering of cells in assays field and colData field must be the same.
#' @param lik_w Weight for each data point. If not provided, weights to correct for sample sizes are used.
#' @param alpha False positive error rate provided as list of elements; if a vector of alpha (and beta) is provided, the inference is performed for multiple values and the solution at
#' maximum-likelihood is returned.
#' @param beta False negative error rate provided as list of elements; if a vector of beta (and alpha) is provided, the inference is performed for multiple values and the solution at
#' maximum-likelihood is returned.
#' @param initialization Binary matrix representing a perfect philogeny clonal tree; clones are rows and mutations are columns.
#' This parameter overrides "random_tree".
#' @param random_tree Boolean. Shall I start MCMC search from a random tree? If FALSE (default) and initialization is NULL, search
#' is started from a TRaIT tree (BMC Bioinformatics . 2019 Apr 25;20(1):210.  doi: 10.1186/s12859-019-2795-4).
#' @param keep_equivalent Boolean. Shall I return results (B and C) at equivalent likelihood with the best returned solution?
#' @param check_indistinguishable Boolean. Shall I remove any indistinguishable event from input data prior inference?
#' @param num_rs Number of restarts during mcmc inference.
#' @param num_iter Maximum number of mcmc steps to be performed during the inference.
#' @param n_try_bs Number of steps without change in likelihood of best solution after which to stop the mcmc.
#' @param learning_rate Parameter to tune the probability of accepting solutions at lower values during mcmc. Value of learning_rate = 1 (default), set a
#' probability proportional to the difference in likelihood; values of learning_rate greater than 1 inclease the chance of accepting solutions at lower likelihood
#' during mcmc while values lower than 1 decrease such probability.
#' @param marginalize Boolean. Shall I marginalize C when computing likelihood?
#' @param error_move Boolean. Shall I include estimation of error rates in the MCMC moves?
#' @param num_processes Number of processes to be used during parallel execution. To execute in single process mode,
#' this parameter needs to be set to either NA or NULL.
#' @param seed Seed for reproducibility.
#' @param verbose Boolean. Shall I print to screen information messages during the execution?
#' @param log_file log file where to print outputs when using parallel. If parallel execution is disabled, this parameter is ignored.
#'
#' @return shiny interface
#' @export lacedata
#'
lacedata <- function( D,
                      lik_w = NULL,
                      alpha = NULL,
                      beta = NULL,
                      initialization = NULL,
                      random_tree = FALSE,
                      keep_equivalent = TRUE,
                      check_indistinguishable = TRUE,
                      num_rs = 50,
                      num_iter = 10000,
                      n_try_bs = 500,
                      learning_rate = 1,
                      marginalize = FALSE,
                      error_move = FALSE,
                      num_processes = Inf,
                      seed = NULL,
                      verbose = TRUE,
                      log_file = "")  {
  
  show <- TRUE
  
  inp <- list(D=D,
              lik_w = lik_w,
              alpha = alpha,
              beta = beta,
              initialization = initialization,
              random_tree = random_tree,
              keep_equivalent = keep_equivalent,
              check_indistinguishable = check_indistinguishable,
              num_rs = num_rs,
              num_iter = num_iter,
              n_try_bs = n_try_bs,
              learning_rate = learning_rate,
              marginalize = marginalize,
              error_move = error_move,
              num_processes = num_processes,
              seed = seed,
              verbose = verbose,
              log_file = log_file,
              show = show )
  
  inference_res=do.call('LACE',inp)
  
  B <- inference_res$B
  clones_prevalence <- inference_res$clones_prevalence
  C <- inference_res$C
  error_rates <- inference_res$error_rates
  
  if (!is.null(B) && !is.null(C) && !is.null(clones_prevalence) && !is.null(error_rates)) {
    x <- LACE:::lace_interface(
      B_mat = B,
      clones_prevalence = clones_prevalence,
      C_mat = C,
      error_rates = error_rates
    )
    x <- x[["html"]]
    return(x)
  }
  else
    return(NULL)
}