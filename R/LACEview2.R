## version 1118

### LACEview2.R
###
### Add lace_interface function

#' @title LACE Interface
#' @description This function generates a longitudinal clonal tree and a
#' graphic interface to explore the data using as input the clonal tree
#' formatted in the same way as the one produced by LACE during the imputation
#' steps
#'
#' @param B_mat (Required). B is the clonal tree matrix where columns are the
#' clonal mutations and and rows are the clones. The clonal tree matrix should
#' contain a column and a row named "Root" representing the root of the tree and
#' the wild type, respectively.
#' B is a binary matrix where 1 are the mutations associated to the clones.
#' The wild type column has all ones
#'
#' @param clones_prevalence (Required) The clonal prevalence matrix
#'
#' @param C_mat (Required) The corrected clonal attachment
#'
#' @param error_rates (Required) The false positive alpha and false negative beta
#' error rates used to infer the clonal tree
#'
#' @param info (Optional). HTML formatted text with information regarding
#' the experiments
#'
#' @param width (optional) Size of the window interafce
#'
#' @param height (optional) Size of the window interafce
#'
#' @param elementId (optional) Element id
#'
#' @return  An implementation of the htmlwidgets
#'
#' @importFrom  Matrix rankMatrix
#' @importFrom jsonlite toJSON
#' @importFrom grDevices colorRampPalette
#' @importFrom purrr is_null
#' @importFrom tidyr gather
#' @importFrom purrr detect_index
#' @import stringr
#' @import stringi
#' @import RColorBrewer
#'
#'
#'
# #' @export


lace_interface <- function (B_mat,
                            clones_prevalence,
                            C_mat,
                            error_rates,
                            width = NULL,
                            height = NULL,
                            elementId = NULL,
                            info = '') {

    ## library(purrr, quietly = T)
    ## library(stringr, quietly = T)
    ## library(stringi, quietly = T)
    ## library(Matrix, quietly = T)

    if (!exists('width')) {width=NULL}
    if (!exists('height')) {height=NULL}
    if (!exists('elementId')) {elementId=NULL}
    if (!exists('info')) {info=''}
    
    if (is_null(height)) {
        height=900
        height_fishplot=300
    } else if (height<800) {
        height_fishplot=300
    } else {
      height_fishplot=300
    }
  
    if (is_null(width)) {
        width=900
        width_fishplot=width/2
    } else if (width<900) {
        width_fishplot=width/2
    } else {
      width_fishplot=width/2
    }
    if (is_null(info)) {
        info = 'No dataset or experimental info available'
    }


    B = B_mat
    B


    make_binary<-function(B) {
        if (!is.numeric(B)){
          # - log2_print('B is not a valid matrix')
            return(NULL)
        }

        is_binary=sum(B==1)+sum(B==0)==ncol(B)*nrow(B)
        if (!is_binary) {
          # - log2_print('B is not binary')
            B[B==1]=1
            B[B==0]=0
        }
        else {
          # - log2_print('B is already binary')
        }
        return(B)
    }


    find_roots <- function(B) {
                                        #find roots
        set_of_roots = rownames(B)
        set_of_roots
        for (i in set_of_roots) {
            tmp_set_of_roots=setdiff(set_of_roots,i)
            for (j in tmp_set_of_roots) {
                proj=sum(B[i,]*B[j,])
                if (sum(B[i,])==proj) {
                    set_of_roots=setdiff(set_of_roots,j)
                    # - log2_print(paste(i,'is parent of', j))
                                        #print(set_of_roots)
                }
                if (sum(B[j,])==proj) {
                    set_of_roots=setdiff(set_of_roots,i)
                    # - log2_print(paste(j,'is parent of', i))
                                        #print(set_of_roots)
                }
            }
        }
        set_of_roots
        return(set_of_roots)
    }

    forest_check <- function(B) {
                                        #check if it is a forest
        check_forest=matrix(data = 0, nrow=nrow(B),ncol=1)
        check_forest
        check_forest_control=matrix(data = 1, nrow=nrow(B),ncol=1)
        check_forest_control[1,1]=0

        for(i in 1:nrow(B)) {
            check_forest_tmp_=0
            for(j in i:nrow(B)) {
                check_forest_tmp=(sum(abs(B[j,]-B[i,]))==1)
                check_forest[j,1]=check_forest[j,1]+check_forest_tmp
                                        #check_forest_tmp_ = check_forest_tmp_+ check_forest_tmp
                # - log2_print(paste(rownames(B)[i], rownames(B)[j], check_forest[j,1]))
            }

                                        #check_forest=rbind(check_forest, check_forest_tmp_)

        }
        # - log2_print(check_forest)
        # - log2_print(check_forest_control)
        # - log2_print(all(check_forest==check_forest_control))
        is_forest=all(check_forest==check_forest_control)
        if (is_forest) {
          # - log2_print("B represents a forest")
        } else {
          # - log2_print("B is not a forest or a tree")
        }
        return(is_forest)
    }

    add_unique_root <- function(B, root_name='Root') {
                                        #add unique root if necessary

        B_rank=rankMatrix(B)[1]

        set_of_roots = find_roots(B)

                                        #check if it has one root
        num_roots=length(set_of_roots)
        if (num_roots==1) {
          # - log2_print("nodes in B are sprouting from a single node")
        }
        else {
          # - log2_print("there are multiple roots")
        }

        if (B_rank==nrow(B) || B_rank==ncol(B)) {
          # - log2_print('B is a full-rank matrix.')
            if (forest_check(B)) {
              # - log2_print('B has a forest like structure')

                if (nrow(B)>ncol(B)) {
                  # - log2_print('B is not a square matrix. B has not enough mutations.')
                                        #print('B is not a full rank matrix. B has not enough mutations.')
                    if (nrow(B)==ncol(B)+1) {
                                        #check if there is a root
                        if (num_roots!=1  && root_col_name!=root_name) {
                            root_col_name=root_name
                            B = cbind(xxx=1,B)
                            colnames(B)[colnames(B) == 'xxx'] <- root_name
                        }
                    }
                } else if (nrow(B)<ncol(B)) {
                  # - log2_print('B is not a square matrix. B has not enough clones.')
                    if (nrow(B)+1==ncol(B)) {
                        lower_tri_column_index=sort(colSums(B),decreasing = TRUE)
                                        #number of clones with such mutation
                        clones_per_mutation=table(lower_tri_column_index)
                        max_clones_per_mutation_idx=(
                            max(as.integer(names(clones_per_mutation))))
                        max_clones_per_mutation_idx

                        if (max_clones_per_mutation_idx<B_rank && root_row_name!=root_name) {
                            root_row_name='Root'
                            B = rbind(xxx=0,B)
                            rownames(B)[rownames(B) == 'xxx'] <- root_name
                            B[1,1]=1
                        }
                    }
                }
            }
        }

        B_rank=rankMatrix(B)[1]
        B_rank
        if (!(B_rank==ncol(B) && B_rank==nrow(B))) {
          # - log2_print('Adding a root does not resolve the problem, B is till not a full rank matrix')
        }

        return(B)
    }

    make_lowtri<-function(B) {
                                        #make B lower triangular
        B_tmp=matrix(nrow=0, ncol=ncol(B))
        colnames(B_tmp)=colnames(B)
        B_tmp
        B2=B #B_row_reduced
        B3=B2 #B_row_reduced_col_ordered
        B4=B3 #B_row_col_reduced_col_ordered
        set_of_roots = rownames(B2)

        for(i in set_of_roots) {
            i=nrow(B)-length(set_of_roots)+1
            i
            first_r=names(which.min(rowSums(B4)))
            first_r

            B_tmp=rbind(B_tmp, B3[first_r, ,drop=FALSE])
            B_tmp
            if (i==nrow(B)){
                break
            }

            if (i>1) {
                one_col=B_tmp[first_r,-1:-i+1]==1
                zero_col=B_tmp[first_r,-1:-i+1]==0
            } else {
                one_col=B_tmp[first_r,]==1
                zero_col=B_tmp[first_r,]==0
            }
            one_col=names(one_col[one_col])
            one_col
            zero_col=names(zero_col[zero_col])
            zero_col
            B2=B2[rownames(B2) !=first_r, , drop=FALSE]
            B2
            B3=cbind(B2[,colnames(B_tmp[,1:i-1,drop=FALSE]),drop=FALSE],
                     B2[,one_col,drop=FALSE],B2[,zero_col,drop=FALSE])
            B3
            B4=cbind(B2[,one_col,drop=FALSE],B2[,zero_col,drop=FALSE])
            B4
                                        #B2=B2[,-1]
            B_tmp=cbind(B_tmp[,1:i-1,drop=FALSE], B_tmp[,one_col,drop=FALSE],
                        B_tmp[,zero_col,drop=FALSE])
            B_tmp
            set_of_roots=setdiff(set_of_roots,first_r)
        }

        B=B_tmp
        return(B)
    }

#####
    B=make_binary(B)
                                        #B_rank=rankMatrix(B)[1]
                                        #B_rank

                                        #is_forest = forest_check(B)
                                        #is_forest

    lower_tri_column_index=sort(colSums(B),decreasing = TRUE)
    root_col_name=names(which.max(lower_tri_column_index))
    root_row_name=names(which.min(sort(rowSums(B),decreasing = TRUE)))

    lower_tri_column_index
    root_col_name
    root_row_name

    B<-add_unique_root(B)
    B<-make_lowtri(B)

    lower_tri_column_index=sort(colSums(B),decreasing = TRUE)
    root_col_name=names(which.max(lower_tri_column_index))
    root_row_name=names(which.min(sort(rowSums(B),decreasing = TRUE)))

    lower_tri_column_index
    root_col_name
    root_row_name



                                        #check all
    is_binary = sum(B==1)+sum(B==0)==ncol(B)*nrow(B)
    is_square_mat = ncol(B)==nrow(B)
    is_full_rank = rankMatrix(B)[1]==nrow(B)
    is_forest = forest_check(B)
    has_single_root = length(find_roots(B))==1
    if (is_binary) {log2_print('B is a binary matrix')}
    if (is_square_mat) {log2_print('B is a square matrix')}
    if (is_full_rank) {log2_print('B is a full rank matrix')}
    if (is_forest) {log2_print('B is a forest ')}
    #browser()
    if (has_single_root) {log2_print('B has single root')}
    if (is_binary && is_square_mat && is_full_rank && is_forest && has_single_root) {
      # - log2_print('all checks done!')
      # - log2_print('continue...')
    }


                                        #this if else is not necessary in this form. Instead, it should set root names
    if ("Root" %in% rownames(clones_prevalence)){
        prevalence=clones_prevalence
    } else {
        prevalence=rbind(Root=0,clones_prevalence)
    }
    prevalence=prevalence[rownames(B),]

    clone_labels=as.list(str_split(colnames(B), pattern='_', n=2, simplify = TRUE)[,1])
    ##clone_labels=str_split(colnames(B), pattern='_', n=2, simplify = T)[,1]
    names(clone_labels)<-rownames(B)
    clone_labels

                                        #library("RColorBrewer")
    colours = as.list(brewer.pal(n = nrow(prevalence), name = "Paired"))
    colours =  as.list(colorRampPalette(brewer.pal(12, "Paired"))(nrow(prevalence)))
    names(colours)<-rownames(B)
    colours






                                        #create adjacency matrix
    adj_matrix = array(0L, c((dim(B)[1]), (dim(B)[2])))
    rownames(adj_matrix) <- colnames(B)[(1:ncol(B))]
    colnames(adj_matrix) <- colnames(B)[(1:ncol(B))]
    for (rP in seq(1, nrow(B), by=1) ) {
        for (rC in seq((rP + 1), nrow(B), length = max(0,nrow(B)-(rP + 1)+1))) {
            if (all(B[rP, 1:rP] == B[rC, 1:rP]) && sum(B[rP, ]) == (sum(B[rC, ]) - 1)) {
                adj_matrix[(rP ), (rC )] <- 1
            }
        }
    }

    # - log2_print('clonal tree:')
    # - log2_print(adj_matrix)

    adj_to_B <- function( adj_matrix, clone_labels ) {
        B=matrix(0, nrow=nrow(adj_matrix), ncol=ncol(adj_matrix))
        colnames(B)=colnames(adj_matrix)
        rownames(B)=rownames(adj_matrix)
        B[1,1]=1
        for (s in rownames(adj_matrix)) {
            for (t in colnames(adj_matrix)) {
                if (adj_matrix[s,t]==1) {
                    B[t,]=B[s,]
                    B[t,t]=1
                }
            }
        }
        rownames(B)=names(clone_labels)
        return(B)
    }
    # - log2_print(adj_to_B(adj_matrix, clone_labels))


                                        #adjMatrix_base <- adj_matrix

####

                                        #it resolves all the incongruences resulting from not considering the time at all
    chrono_occurences <- function(adj_matrix, prevalence) {
                                        #create first time mutations appearance based on clonal prevalence only
        times= apply((prevalence)>0, 1, function(x) detect_index(x, ~ .x == TRUE) )
                                        #times=times+1 #test
        times[1]=1 #setting root

        times_pre<-times

        adj_matrix_t_pre<-adj_matrix
        colnames(adj_matrix_t_pre) <-paste0(rownames(prevalence), '_t', times)
        rownames(adj_matrix_t_pre) <-paste0(rownames(prevalence), '_t', times)


                                        #create first time mutations appearance based on chronological order
        sources=apply( t(adj_matrix)>0, 1, function(x) detect_index(x, ~ .x == TRUE) )
        sources[1]=1
        for(target in 1: length(rownames(prevalence))) {
            source=sources[target]
            if (times[target] < times[source]) {
              # - log2_print(times[target],times[source])
                times[source]=times[target]
            }
        }

        adj_matrix_t<-adj_matrix
        colnames(adj_matrix_t) <-paste0(rownames(prevalence), '_t', times)
        rownames(adj_matrix_t) <-paste0(rownames(prevalence), '_t', times)



        if (all(times==times_pre)) {
          # - log2_print('No incongruences found due to the chronological order')

          # - log2_print('first time mutation occurrences based the chronological order of samples:')
          # - log2_print(times)
          # - log2_print('adjacent matrix with first time occurrences:')
          # - log2_print(adj_matrix_t)

        } else {
          # - log2_print('The longitudinal clonal tree has been corrected')

          # - log2_print('pre-computation of first time mutation occurrences based only on prevalence values different from zero:')
          # - log2_print(times_pre)
          # - log2_print('adjacent matrix with pre-computed first time occurrences:')
          # - log2_print(adj_matrix_t_pre)

          # - log2_print('first time mutation occurrences based the chronological order of samples:')
          # - log2_print(times)
          # - log2_print('adjacent matrix with first time occurrences:')
          # - log2_print(adj_matrix_t)
        }

        return(times)
    }

    times <- chrono_occurences(adj_matrix,prevalence)

    adj_matrix_t<-adj_matrix
    colnames(adj_matrix_t) <-paste0(rownames(prevalence), '_t', times)
    rownames(adj_matrix_t) <-paste0(rownames(prevalence), '_t', times)




    ## adding the other elements of the direct product
    times_n= 1:(length(colnames(prevalence))-1)
    times_n
    for (t in times_n) {
        for (col in 1:ncol(adj_matrix)) {
            col_ = paste0(rownames(prevalence)[col],'_t',t)

            if (!(col_ %in% colnames(adj_matrix_t))) {
                adj_matrix_t=cbind(adj_matrix_t, xxx=0)
                colnames(adj_matrix_t)[colnames(adj_matrix_t) == 'xxx'] <- col_
            }
        }
        for (row in 1: nrow(adj_matrix)) {
            row_ = paste0(rownames(prevalence)[row],'_t',t)

            if (!(row_ %in% rownames(adj_matrix_t))) {
                adj_matrix_t=rbind(adj_matrix_t, xxx=0)
                rownames(adj_matrix_t)[rownames(adj_matrix_t) == 'xxx'] <- row_
            }
        }
    }

    ## Root as wild type should be considered present in all the next time steps:
    ## hence an accessory self link is used
    adj_matrix_t['Root_t1','Root_t1']=1

    ## setting links of clones in following times after first occurrence
  for (t_1 in times_n[1:(length(times_n)-1)]) {
    # - log2_print(c('time=',t_1))
    for (source in 1:nrow(adj_matrix)) {
      for (target in 1:ncol(adj_matrix)) {
        for (t_2 in times_n[t_1:(length(times_n)-1)]) { # r fa cacare
          #source mut_time
          source_t1 = paste0(rownames(prevalence)[source],'_t',t_1)
          #target mut_time
          target_t2 = paste0(rownames(prevalence)[target],'_t',t_2)
          #print(c(source_t1, target_t2, adj_matrix_t[source_t1,target_t2]))
          #source mut_time
          source_t2 = paste0(rownames(prevalence)[target],'_t',t_2)
          #target mut_time+1
          target_t2_next=paste0(rownames(prevalence)[target],'_t',t_2+1)

          if (adj_matrix_t[source_t1,target_t2]>=1 && adj_matrix_t[source_t1,target_t2]<=3) {

            if (target==source && prevalence[target,t_2]==0){
              value=3 #for dashed in case undetected clones

              # - log2_print(paste0(source_t1, "->", target_t2,"=", adj_matrix_t[source_t1,target_t2], ", prev_",target_t2, "=", prevalence[target,t_2], ": ", source_t1, "->", target_t2, "=", value))
              adj_matrix_t[source_t1, target_t2]<-value # we set the link before t2
              # - log2_print(paste0(source_t1, "->", target_t2,"=", adj_matrix_t[source_t1,target_t2], ", prev_",target_t2, "=", prevalence[target,t_2], ": ", source_t2, "->", target_t2_next, "=", value))
              adj_matrix_t[source_t2, target_t2_next]<-value # we set the link after t2

            }
            else {
              value=2 #for time persistence

              # - log2_print(paste0("1st ",source_t1, "->", target_t2,"=", adj_matrix_t[source_t1,target_t2], ", prev_",target_t2, "=", prevalence[target,t_2], ": ", source_t2, "->", target_t2_next, "=", value))
              adj_matrix_t[source_t2,target_t2_next]<-value
            }

            if (target==source && t_2+1==length(times_n) && prevalence[target,t_2+1]==0){
              value=3 #for dashed in case undetected clones
              # - log2_print(paste0(source_t1, "->", target_t2,"=", adj_matrix_t[source_t1,target_t2], ", prev_",target_t2, "=", prevalence[target,t_2], ": ", source_t2, "->", target_t2_next, "=", value))
              adj_matrix_t[source_t2, target_t2_next]<-value # we set the link after t2
            }
          }
          #if (target==source && t_1+1==t_2 && prevalence[target,t_2]==0) {
            #target_t1 = paste0(rownames(prevalence)[target],'_t',t_1) #t1=t2-1
            # = paste0(rownames(prevalence)[source],'_t',t_2)
            #target_t2 = paste0(rownames(prevalence)[target],'_t',t_2)
            #target mut_time+1
            #target_t2_next=paste0(rownames(prevalence)[target],'_t',t_2+1)

            #value=3 #for dashed in case undetected clones

            # print(c(source_t1, target_t2, source_t2, target_t2_next, value))
            # print(c(source_t1, target_t2, source_t1, target_t2, value))
            # adj_matrix_t[source_t2, target_t2_next]<-value # we set the link after t2
            # adj_matrix_t[source_t1, target_t2]<-value # we set the link before t2
          #}
        }
      }
    }
  }

                                        #dead breaches
                                        #in time reversal
                                        #prevalence=prevalence
    for (t_1 in times_n[(length(times_n)):1]) { # instead of 2 as smaller point
      # - log2_print(c('time=',t_1))
        for (source in 1:nrow(adj_matrix)) {
            target=source # correlation only at distance 0

            source_t1_prev =paste0(rownames(prevalence)[source],'_t',t_1-1)
            target_t1 = paste0(rownames(prevalence)[target],'_t',t_1)

            source_t1 =paste0(rownames(prevalence)[source],'_t',t_1)
            target_t1_next = paste0(rownames(prevalence)[target],'_t',t_1+1)

            if (t_1==times_n[(length(times_n))]  && prevalence[target,t_1]==0 ){ #&& prevalence[source,t_1-1]==0) {
                value=0 #for dead breaches
                # - log2_print(c(source_t1, target_t1, source_t1_prev, target_t1, value))
                adj_matrix_t[source_t1_prev,target_t1]<-value # we set the link after t2

                                        #prevalence[prevalence[,t_1]==0,t_1]=-1
                prevalence[target,t_1]=-1

            } else if (prevalence[source,t_1]==0  && adj_matrix_t[source_t1,target_t1_next]==0) {
                                        #target mut_time+1
                value=0 #for dead breaches
                # - log2_print(c('removed', target_t1))
                # - log2_print(c(source_t1, target_t1, source_t1, target_t1_next, value))
                adj_matrix_t[source_t1,target_t1_next]<-value # we set the link after t2

                if (t_1!=1){
                  # - log2_print(c(source_t1, target_t1, source_t1_prev, target_t1, value))
                    adj_matrix_t[source_t1_prev,target_t1]<-value # we set the link after t2
                }
                                        #prevalence[prevalence[,t_1]==0,t_1]=-1
                if (!(stri_detect_fixed(target_t1,"Root") && t_1==1)){
                    prevalence[target,t_1]=-1
                }
            }
        }
    }
    ## removal of the accessory self link
    adj_matrix_t['Root_t1','Root_t1']=0


    colnames(prevalence)

    elements = list()
    elements$nodes = list()
    elements$edges = list()

    for (row in 1:nrow(adj_matrix_t)) {
        for (col in 1:ncol(adj_matrix_t)) {
            if (adj_matrix_t[row, col] == 1) {
                data = list()
                data$data = list()

                clone_=stri_reverse(str_split(
                    stri_reverse(colnames(adj_matrix_t)[col]),pattern='_', n=2)[[1]][2])
                clone_idx=match(clone_,rownames(B))
                # - log2_print(c('idx',clone_))

                data$data$source = sprintf("%s", rownames(adj_matrix_t)[row])
                data$data$target = sprintf("%s", colnames(adj_matrix_t)[col])
                data$data$name = sprintf("%s", clone_labels[clone_][[1]])

                                        #data$data$color = colours[clone_idx]
                data$data$color = colours[clone_][[1]]

                data$data$id = sprintf("%s_%s", data$data$source, data$data$target)
                data$data$linestyle = "solid"
                elements$edges = c(elements$edges, list(data))
            }
            if (adj_matrix_t[row, col] == -200) {  #if (adj_matrix_t[row, col] == 2) {
                data = list()
                data$data = list()

                clone_=stri_reverse(str_split(stri_reverse(
                    colnames(adj_matrix_t)[col]),pattern='_', n=2)[[1]][2])
                                        #color_idx=match(clone_,rownames(B))

                data$data$source = sprintf("%s", rownames(adj_matrix_t)[row])
                data$data$target = sprintf("%s", colnames(adj_matrix_t)[col])
                data$data$name = sprintf("")

                data$data$color = colours[clone_][[1]]

                data$data$id = sprintf("%s_%s", data$data$source, data$data$target)
                data$data$linestyle = "solid" # only if next clonal_prev==0 else solid
                elements$edges = c(elements$edges, list(data))
            }
            if (adj_matrix_t[row, col] >= 2) {  #if (adj_matrix_t[row, col] == 3) {
                data = list()
                data$data = list()

                clone_=stri_reverse(str_split(stri_reverse(
                    colnames(adj_matrix_t)[col]),pattern='_', n=2)[[1]][2])
                                        #color_idx=match(clone_,rownames(B))

                data$data$source = sprintf("%s", rownames(adj_matrix_t)[row])
                data$data$target = sprintf("%s", colnames(adj_matrix_t)[col])
                data$data$name = sprintf("")

                data$data$color = colours[clone_][[1]]

                data$data$id = sprintf("%s_%s", data$data$source, data$data$target)
                data$data$linestyle = "dashed" # only if next clonal_prev==0 else solid
                elements$edges = c(elements$edges, list(data))
            }
        }
    }

                                        #nodes
    for (col in 1:(ncol(prevalence) - 1)) {
        data = list()
        data$data = list()
        data$data$id = sprintf("T%s", col)
        data$data$name = sprintf("T%s", col) #experiment2...
        data$data$color = "#FFFFFF"
        data$data$size = sprintf("%spx", 50)
        data$data$prev = sprintf("%s [%s]", data$data$name, round(prevalence[1, col], 2))
        parent = data$data$name
        elements$nodes = c(elements$nodes, list(data))
        for (row in 1:nrow(prevalence)) {
            if (prevalence[row,col]!=-1) {
                data = list()
                data$data = list()
                data$data$name = rownames(prevalence)[row]
                data$data$color = colours[row][[1]]
                data$data$id = sprintf("%s_t%s", rownames(prevalence)[row], col)

                data$data$size = sprintf("%spx", 5 + as.integer(prevalence[row, col] * 150))
                data$data$prev = sprintf("%s", round(prevalence[row, col], 2))
                data$data$parent = parent
                elements$nodes = c(elements$nodes, list(data))
            }
        }
    }


                                        #-------------------
                                        #library(tidyr)

    if ("Root" %in% rownames(clones_prevalence)){
        prevalence=clones_prevalence
    } else {
        prevalence=rbind(Root=0,clones_prevalence)
    }
    prevalence=prevalence[rownames(B),]

    fishplot_clonal_prev=as.data.frame(prevalence)
    fishplot_clonal_prev = fishplot_clonal_prev[,!(names(fishplot_clonal_prev) %in% c("Total"))]
    fishplot_clonal_prev=subset(fishplot_clonal_prev)
    colnames(fishplot_clonal_prev)=c(paste0('T',times_n))
    fishplot_clonal_prev['clone_id']=rownames(fishplot_clonal_prev)
    fishplot_clonal_prev


    fishplot_clonal_prev=gather(fishplot_clonal_prev,
                                'timepoint','clonal_prev', -'clone_id' )
    fishplot_clonal_prev=fishplot_clonal_prev[c('timepoint',
                                                'clone_id','clonal_prev')]
    fishplot_clonal_prev['clonal_prev']=fishplot_clonal_prev['clonal_prev']+0.0000001

    fishplot_clonal_prev


    fishplot_tree_edges=as.data.frame(adj_matrix)
                                        #fishplot_tree_edges['source']=rownames(fishplot_tree_edges)
    fishplot_tree_edges['source']=rownames(prevalence)
    colnames(fishplot_tree_edges)=c(fishplot_tree_edges[['source']],'source')
    fishplot_tree_edges=gather(fishplot_tree_edges, 'target','value', -source)
    fishplot_tree_edges=fishplot_tree_edges[fishplot_tree_edges['value']==1,][c('source', 'target')]

    fishplot_tree_edges

    fishplot_colour=t(as.data.frame(colours))
    fishplot_colour=data.frame(clone_id=rownames(fishplot_colour), colour=fishplot_colour[,1])


    # - log2_print(getwd())
                                        #source(timescape.R)
                                        #source(paste0(basedir,"/peter/LACEView/R/timescape.R"))
    x=timescape(clonal_prev = fishplot_clonal_prev,
                tree_edges = fishplot_tree_edges,  height=height_fishplot,
                width=width_fishplot, alpha=0, genotype_position="centre",
                clone_colours = fishplot_colour)

    tabula=data.frame(T=c(paste0('T',times_n)), alpha=error_rates$alpha,
                      beta=error_rates$beta,
                      m_tilde=length(rownames(prevalence))-1,
                      n=unlist(lapply(C_mat,length)))
    rownames(tabula)=c(paste0('T',times_n))
    tabula
    headings=c("<p style=' font-size: 14px'>T</p>",
               "<p style=' font-size: 14px'>alpha</p>
               <p style='font-weight:normal; margin-top: -20px; margin-bottom: -20px; font-size: 12px'>est.</p>",
               "<p style=' font-size: 14px'>beta</p>
               <p style='font-weight:normal; margin-top: -20px; margin-bottom: -20px; font-size: 12px'>est.</p>",
               paste("<p style=' font-size: 14px'>m_tilde</p>",
               #<p style='font-weight:normal; margin-top: -20px; margin-bottom: -20px; font-size: 12px'>n candidate drivers</p>",
               "<p style='font-weight:normal; margin-top: -20px; margin-bottom: -20px; font-size: 12px'>n drivers</p>"),
               "<p style=' font-size: 14px'>n</p>
               <p style='font-weight:normal; margin-top: -20px; margin-bottom: -20px; font-size: 12px'>single cells</p>"
               )
    tab={}
    tab$headings=headings
    tab$data=t(t(tabula))

                                        #library(jsonlite)
    toJSON(tab)
                                        #-------------------



    clonalprev_df <- prevalence
    clonalprev_df = clonalprev_df[,!(colnames(clonalprev_df) %in% c("Total"))]
    clonalprev_df <- subset(clonalprev_df)
    clonalprev_df_names <- rownames(clonalprev_df)
    stream_df <- data.frame(Time = c(1:ncol(clonalprev_df)))
    for (k in clonalprev_df_names) {
        temp <- data.frame(k = clonalprev_df[k, ], stringsAsFactors = FALSE)
        stream_df <- cbind(stream_df, temp)
    }
    names(stream_df) <- c("Time", clonalprev_df_names)
    stream_df_colors = list()
    for (K in 1:length(colours)) {
        stream_df_colors[[clonalprev_df_names[K]]] = colours[K]
    }



    # - log2_print("info")
    # - log2_print(info)
    jsdata = list(elements = elements, data = toJSON(stream_df),
                  columns = toJSON(names(stream_df)),
                  colors = toJSON(stream_df_colors),
                  clone_labels = toJSON(unlist(clone_labels)[-1]),
                  x=x,
                  info=toJSON(list(info=info)),
                  tab=toJSON(tab))


    htmlwidgets::createWidget(name = "LACE", jsdata, width = width,
                              height = height, package = "LACE",
                              elementId = elementId,
                              sizingPolicy = htmlwidgets::sizingPolicy(
                                                              viewer.suppress = TRUE,
                                                              knitr.figure = FALSE,
                                                              browser.fill = TRUE,
                                                              browser.padding = 0,
                                                              knitr.defaultWidth = 300,
                                                              knitr.defaultHeight = 600)
                              )
}


####################


####################
#add modified LACEview_html function
####################
LACE_html<-function (id, style, class, ...) {
    htmltools::tags$div(id = id, class = class, style = style,
                        htmltools::div(id = "wrapper",
                                       style = "height:100%; width:100%;",
                                       htmltools::div(id = "navbar"),
                                       htmltools::div(id = "container",
                                                      style = "flex-flow: row wrap; display: flex; height:800px; width:100%;",
                                                      htmltools::div(
                                                                     style = "flex:0 0 48%; align-items: center; height:64%; margin: 5px; padding:5px; border: 1px solid black;",
                                                                     id = "cy_",
                                                                     htmltools::div(
                                                                                    style = "flex:100%; align-items: center; height:5%;",
                                                                                    id = "cy_title",
                                                                                    htmltools::p(
                                                                                                   style = "font-size:20px; text-align: center; font-weight: bold;", "Longitudinal clonal tree"
                                                                                               ),
                                                                                    ),
                                                                     htmltools::div(
                                                                                    style = "flex:100%; align-items: center; height:95%;",
                                                                                    id = "cy"
                                                                                ),
                                                                     ),
                                                      htmltools::div(
                                                                     style = "flex: 0 0 48%; align-items: center; height:64%; margin: 5px; padding:5px; border: 1px solid black;",
                                                                     id = "streamgraph_",
                                                                     htmltools::div(
                                                                                    style = "flex:100%; align-items: center; height:5%;",
                                                                                    id = "streamgraph_title",
                                                                                    htmltools::p(
                                                                                                   style = "font-size:20px; text-align: center; font-weight: bold;", "Fishplot"
                                                                                               ),
                                                                                    ),
                                                                     htmltools::div(
                                                                                    style = "flex:100%; align-items: center; height:95%; font-size:20px; font-weight: normal;",
                                                                                    id = "streamgraph"
                                                                                ),
                                                                     ),
                                                      htmltools::div(
                                                        style = "flex:100%; height:8%; display:flex; align-items: center; justify-content:flex-start; margin-left: 40px;",
                                                        id = "mutations"
                                                      ),
                                                      htmltools::div(
                                                                     style = "flex:100%; height:35%; display:flex; align-items: center; justify-content:center;",
                                                                     id = "lacetable",
                                                                     htmltools::div(
                                                                                    style = "flex:50%; height:100%; display:flex; align-items: flex-end; justify-content:flex-start; font-size:14px;",
                                                                                    id = "legend"
                                                                                ),
                                                                     htmltools::div(
                                                                                    style = "flex:50%; height:100%; display:flex; align-items: center; justify-content:center; font-size:14px;",
                                                                                    id = "table",
                                                                                    class='table'
                                                                                ),
                                                                     htmltools::div(
                                                                                    style = "flex:100%; height:100%; display:none; align-items: flex-end; justify-content:center;",
                                                                                    id = "other"
                                                                                )
                                                                 )
                                                      )
                                   )
                        )
}
####################



## Shiny bindings for LACE
##
## Output and render functions for using LACEview within Shiny
## applications and interactive Rmd documents.
# #' @export
LACEOutput <- function(outputId, width = '100%', height = '400px') {
    htmlwidgets::shinyWidgetOutput(outputId,
                                   'LACE',
                                   width,
                                   height,
                                   package = 'LACE')
}


# #' @rdname LACE-shiny
# #' @export
renderLACE <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, LACEOutput, env, quoted = TRUE)
}


### end of file -- LACEview2.R
