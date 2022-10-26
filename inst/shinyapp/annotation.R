### annotation.R
###
### Code for ANNOVAR annotations.
###
### See file LICENSE in the main folder for licensing and copyright
### information.
###

### Notes:
### MA: questo file assume UN*X.  Modificare.

av_compute <- function(anovar_convert,
                       anovar_annot,
                       anovar_db,
                       vcf_in_dir,
                       vcf_annot_dir,
                       pattern = '.*.filtered.vcf$',
                       v_ref = 'hg38',
                       InFilesToDo = NULL,
                       OutFilesToRm = NULL) {
    ## library("foreach")
    ## library("doParallel")
    
    numCores <- detectCores()
    registerDoParallel(ceiling(numCores / 2))
    
    dir.create(vcf_annot_dir, showWarnings = FALSE)
    stdout_log <- file.path(vcf_annot_dir, 'stdout.log') 
    stderr_log <- file.path(vcf_annot_dir, 'stderr.log') 
    log_bash <- paste0(' >> ', stdout_log, ' 2>> ', stderr_log) # UN*X! E Windows?
    stdout_log_tmp <- file.path("/tmp/av_stdout.log")           # UN*X! E Windows?
    stderr_log_tmp <- file.path("/tmp/av_stderr.log")           # UN*X! E Windows?
    
    cat("", file = stdout_log, append=FALSE) # comment to not erase log
    cat("", file = stderr_log, append=FALSE) # comment to not erase log
    
    
    vcf_filtered = list.files(path = vcf_in_dir, pattern = pattern)
    log2_print(InFilesToDo)
    if (!is.null(InFilesToDo)) {
        log2_print('In')
        log2_print(vcf_filtered)
        vcf_filtered <- vcf_filtered[vcf_filtered %in% basename(InFilesToDo)]
        log2_print(vcf_filtered)
    }
    if (!is.null(OutFilesToRm)) {
        log2_print('Out')
        ## OutFilesToRm <- file.path(vcf_annot_dir, OutFilesToRm)
        log2_print(OutFilesToRm)
        ## file.remove(OutFilesToRm)
        log2_print(list.files(path = vcf_annot_dir))
    }
    
    ## res <- foreach(f = vcf_filtered, .combine=c) %dopar% {
    for (f in vcf_filtered) {
        
        f_in = file.path(vcf_in_dir,f)
        f_out = sub('\\.vcf$', '', f)
        f_out = file.path(vcf_annot_dir,paste0(f_out, '.anninput'))
        
        ## cmd_exp=paste0(anovar_convert,' -format vcf4 ',f_in,' --outfile ',f_out, ' --includeinfo', log_bash)
        ## exit_status<- <- system(cmd_exp, intern = FALSE)
        cmd_exp = paste0('-format vcf4 ',
                         f_in,
                         ' --outfile ',
                         f_out,
                         ' --includeinfo')
        log2_print(cmd_exp)
        exit_status <-
            system2(anovar_convert,
                    args = cmd_exp,
                    stdout = stdout_log_tmp,
                    stderr = stderr_log_tmp,
                    wait = TRUE)
        now <- date()
        cat(paste0("\nExecuted: ",
                   now,
                   '\t',
                   anovar_convert,
                   "\n",
                   "Exit status: ",
                   exit_status,
                   "\n"),
            file = stdout_log,
            append = TRUE)
        file.append(stdout_log, stdout_log_tmp)
        cat(paste0("\nExecuted: ",
                   now,
                   '\t',
                   anovar_convert,
                   "\n",
                   "Exit status: ",
                   exit_status,
                   "\n"),
            file = stderr_log,
            append=TRUE)
        file.append(stderr_log, stderr_log_tmp)
    }
    
    ## res <- foreach(f = vcf_filtered, .combine=c) %dopar% {
    for (f in vcf_filtered) {
        f_out = sub('\\.vcf$', '', f)
        f_out = file.path(vcf_annot_dir, paste0(f_out, '.anninput'))
        f_in = f_out
        
        ## cmd_exp=paste0(anovar_annot, ' -out ', f_out, ' -exonicsplicing -build hg38 ', f_in, ' ',anovar_db, log_bash)
        ## exit_status<- <- system(cmd_exp, intern = FALSE)
        cmd_exp = paste0(' -out ', f_out,
                         ' -exonicsplicing -build ',
                         v_ref, ' ', f_in, ' ', anovar_db)
        log2_print(cmd_exp)
        exit_status <- system2(anovar_annot,
                               args = cmd_exp,
                               stdout = stdout_log_tmp,
                               stderr = stderr_log_tmp,
                               wait = TRUE)
        now <- date()
        cat(paste0("\nExecuted: ",
                   now,
                   '\t',
                   anovar_annot,
                   "\n",
                   "Exit status: ",
                   exit_status,
                   "\n"),
            file = stdout_log,
            append = TRUE)
        file.append(stdout_log, stdout_log_tmp)
        
        cat(paste0("\nExecuted: ", now, '\t', anovar_annot, "\n",
                   "Exit status: ",exit_status, "\n"),
            file = stderr_log,
            append = TRUE)
        file.append(stderr_log, stderr_log_tmp)
    }
}


### end of file -- annotation.R
