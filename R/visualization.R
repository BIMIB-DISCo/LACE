#' Plot a longitudinal tree inferred by LACE.
#' @title longitudinal.tree.plot
#'
#' @examples
#' data(inference)
#' clone_labels = c("ARPC2","PRAME","HNRNPC","COL1A2","RPL5","CCT8")
#' longitudinal.tree.plot(inference = inference,
#'                        labels = "clones",
#'                        clone_labels = clone_labels,
#'                        legend_position = "topleft")
#'
#' @param inference Results of the inference by LACE.
#' @param rem_unseen_leafs If TRUE (default) remove all the leafs that have never been observed (prevalence = 0 in each time point)
#' @param show_plot If TRUE (default) output the longitudinal tree to the current graphical device.
#' @param filename Specify the name of the file where to save the longitudinal tree. Dot or graphml formats are supported and are chosen based on the extenction of the filename (.dot or .xml).
#' @param labels_show Specify which type of label should be placed on the tree; options are, 
#'   "mutations": parental edges are labeled with the acquired mutation between the two nodes (genotypes); 
#'   "clones": nodes (genotypes) are labeled with their last acquired mutation; 
#'   "both": either nodes and edges are labeled as specified above; 
#'   "none": no labels will show on the longitudinal tree. 
#' @param clone_labels Character vector that specifies the name of the nodes (genotypes). If it is NULL (default), nodes will be labeled as specified by "label" parameter.
#' @param show_prev If TRUE (default) add to clones label the correspongind prevalance.
#' @param label.cex Specify the size of the labels.
#' @param size Specify size of the nodes. The final area is proportional with the node prevalence.
#' @param size2 Specify the size of the second dimension of the nodes. If NULL (default), it is set equal to "size".
#' @param tk_plot If TRUE, uses tkplot function from igraph library to plot an interactive tree. Default is FALSE.
#' @param tp_lines If TRUE (defaul) the function draws lines between timepoints.
#' @param tp_mark If TRUE (defaul) the function draws different colored area under the nodes in different time points.
#' @param tp_mark_alpha Specify the alpha value of the area drawed when tp_mark = TRUE.
#' @param legend If TRUE (default) a legend will be displayed on the plot.
#' @param legend_position Specify the legend position.
#' @param label_offset Move the mutation labels horizontally (default = 4)
#' @param legend_cex Specify size of the legend text.
#' @return An igraph object g with the longitudinal tree inferred by LACE. 
#' @export longitudinal.tree.plot
#' @rawNamespace import(graphics, except = c("box"))
# #' @import graphics
#' @import grDevices
# '@rawNamespace import(igraph, except = c("crossing","union", "as_data_frame", "groups",  "path"))
# #' @import igraph
#' @import RColorBrewer
#' @import utils
#'
longitudinal.tree.plot <- function( inference, 
                                    rem_unseen_leafs = TRUE,
                                    show_plot = TRUE, 
                                    filename = "lg_output.xml", 
                                    labels_show = "mutations", 
                                    clone_labels = NULL, 
                                    show_prev = TRUE, 
                                    label.cex = 1, 
                                    size = 500, 
                                    size2 = NULL, 
                                    tk_plot = FALSE, 
                                    tp_lines = TRUE, 
                                    tp_mark = TRUE, 
                                    tp_mark_alpha = 0.5, 
                                    legend = TRUE, 
                                    legend_position = "topright", 
                                    label_offset = 4, 
                                    legend_cex = 0.8 ) {
    
    if(is.null(size2)) {
        size2 <- size
    }

    adjMatrix_base <- as.adj.matrix.unsorted(inference$B, root = TRUE)
    
    M_leafs <- which(apply(X = adjMatrix_base, MARGIN = 1, FUN = sum)==0)
    Clone_Mut <- sapply(inference$clones_summary, function(x){tail(x,1)}, USE.NAMES = TRUE)
    C_leafs <- Clone_Mut[match(names(M_leafs), Clone_Mut)]
    
    
    if(rem_unseen_leafs == TRUE) {

        uns_cl_mut <- C_leafs[which(inference$clones_prevalence[match(names(C_leafs),rownames(inference$clones_prevalence)), "Total"] == 0)]

            while(length(uns_cl_mut) > 0) {
                
                # Delete each clone and corresponding mutation from B, clone_prevalence and clone_summary
                del_mut <- as.character(uns_cl_mut)
                del_clone <- names(uns_cl_mut)
                
                inference$B <- inference$B[!(rownames(inference$B) %in% del_clone), !(colnames(inference$B) %in% del_mut)]
                inference$clones_prevalence <- inference$clones_prevalence[!(rownames(inference$clones_prevalence) %in% del_clone),]
                inference$clones_summary <- inference$clones_summary[!(names(inference$clones_summary) %in% del_clone)]
                
                # Repeate
                adjMatrix_base <- as.adj.matrix.unsorted(inference$B, root = TRUE)
                M_leafs <- which(apply(X = adjMatrix_base, MARGIN = 1, FUN = sum)==0)
                Clone_Mut <- sapply(inference$clones_summary, function(x){tail(x,1)}, USE.NAMES = TRUE)
                C_leafs <- Clone_Mut[match(names(M_leafs), Clone_Mut)]
                uns_cl_mut <- C_leafs[which(inference$clones_prevalence[match(names(C_leafs),rownames(inference$clones_prevalence)), "Total"] == 0)]
            }
    }
    
    cl_vertex <- data.frame(prevalance = as.vector(inference$clones_prevalence[,-ncol(inference$clones_prevalence)]))
    cl_vertex$clone <- rep(c(0:(nrow(inference$clones_prevalence)-1)),ncol(inference$clones_prevalence)-1)
    cl_vertex$TP <- rep(1:(ncol(inference$clones_prevalence)-1), each = nrow(inference$clones_prevalence))
    cl_vertex$last_mutation <- rep(c("Root",Clone_Mut), ncol(inference$clones_prevalence)-1)

    if(!is.null(clone_labels)) {
        if(length(clone_labels)!=length(inference$clones_summary)) {
            warning("Label number is different from the number of clones.")
            cl_vertex$label <- ""
        }
        else {
            cl_vertex$label <- rep(c("Root",clone_labels), ncol(inference$clones_prevalence)-1)
        }
    } else if((labels_show == "clones" || labels_show == "both") && labels_show != "none") {
        cl_vertex$label <- rep(c("Root",names(inference$clones_summary)), ncol(inference$clones_prevalence)-1)
    } else {
        cl_vertex$label <- ""
        cl_vertex$label[cl_vertex$last_mutation == "Root"] <- "Root"
    }
    
    cl_vertex$branch <- NA
    cl_vertex$branch_level <- 0
    cl_vertex$names <- paste0("T", cl_vertex$TP, "-", cl_vertex$last_mutation)
    
    # Names must be the first column
    cl_vertex <- cl_vertex[,order(ncol(cl_vertex):1)]
    
    cl_edges <- data.frame(stringsAsFactors = FALSE)
    
    # Processing persistent relations
    for(c in unique(cl_vertex$clone)) {
        t <- 1
        while(t <= max(cl_vertex$TP)) {
            from_cc <- cl_vertex$names[cl_vertex$clone == c & cl_vertex$TP == t]
            if(cl_vertex$prevalance[cl_vertex$names == from_cc] <= 0) {
                t <- t + 1
            }
            else {
                nt <- t + 1
                while(nt <= max(cl_vertex$TP)) {
                    if(max(cl_vertex$prevalance[cl_vertex$clone == c & cl_vertex$TP >= nt]) > 0) {
                        to_cc <- cl_vertex$names[cl_vertex$clone == c & cl_vertex$TP == nt]
                        cl_edges <- rbind(cl_edges, data.frame(from = from_cc, to = to_cc, type = 2L, extincion = FALSE))
                        from_cc <- to_cc
                    }
                    else {
                        to_cc <- cl_vertex$names[cl_vertex$clone == c & cl_vertex$TP == nt]
                        cl_edges <- rbind(cl_edges, data.frame(from = from_cc, to = to_cc, type = 2L, extincion = TRUE))
                        break;
                    }
                    nt <- nt + 1
                }
                t <- nt
            }
        }
    }

    # Processing parental relations
    cl_edges_parental <- data.frame(stringsAsFactors = FALSE)
    
    # Start from one leaf of M_leafs
    for(cs_id in M_leafs) {
        curTP <- max(cl_vertex$TP)
        cp_id <- which(adjMatrix_base[,cs_id]==1)
        while(length(cp_id)>0) {
            
            # Finding first time point in which appear son clone
            TPs <- cl_vertex$TP[cl_vertex$clone == (cs_id-1) & cl_vertex$prevalance > 0 & cl_vertex$TP <= curTP]
            
            if(length(TPs)==0) {
                cs_tp <- curTP
            } else {
                cs_tp <- min(TPs)
            }
            
            # Finding the time point in which parental clone were present most close to the current time point
            TPs <- cl_vertex$TP[cl_vertex$clone == (cp_id-1) & cl_vertex$prevalance > 0 & cl_vertex$TP <= cs_tp]
            
            if(length(TPs) == 0) {
                # If the parental clone has ever prevalence == 0 -> I use the same time point of the son clone
                cp_tp = cs_tp
            } else {
                cp_tp <- max(TPs)
            }
            
            from_cc <- cl_vertex$names[cl_vertex$clone == (cp_id-1) & cl_vertex$TP == cp_tp]
            to_cc <- cl_vertex$names[cl_vertex$clone == (cs_id-1) & cl_vertex$TP == cs_tp]
            mut <- cl_vertex$last_mutation[cl_vertex$clone == (cs_id-1) & cl_vertex$TP == cs_tp]
            cl_edges_parental <- rbind(cl_edges_parental, data.frame(from = from_cc, 
                                                                     to = to_cc, 
                                                                     type = 1L, 
                                                                     extincion = FALSE, 
                                                                     stringsAsFactors = FALSE))
            
            # Now son clone becomes parental clone
            curTP <- cp_tp
            cs_id <- cp_id
            cp_id <- which(adjMatrix_base[,cs_id]==1)
            
        }
    }
    
    # Fix duplicate parental relation (keep earliest) ones
    
    cl_edges_parental$from_cl <- cl_vertex$clone[match(cl_edges_parental$from, cl_vertex$names)]
    cl_edges_parental$to_cl <- cl_vertex$clone[match(cl_edges_parental$to, cl_vertex$names)]
    cl_edges_parental$to_tp <- cl_vertex$TP[match(cl_edges_parental$to, cl_vertex$names)]
    
    cl_edges_parental <- cl_edges_parental[order(cl_edges_parental$to_tp),]
    
    cl_edges_parental <- cl_edges_parental[!duplicated(cl_edges_parental[,c("from_cl", "to_cl")], fromLast = FALSE), c("from","to","type","extincion")]
    
    cl_edges <- rbind(cl_edges, cl_edges_parental)
    
    # Fixing missing persistence relations
    fixing_clones <- data.frame(names = unique(as.character(cl_edges$from)))
    
    fixing_clones$clones <- cl_vertex$clone[match(fixing_clones$names,cl_vertex$names)]
    fixing_clones$TP <- cl_vertex$TP[match(fixing_clones$names,cl_vertex$names)]
    
    fixing_clones <- fixing_clones[duplicated(fixing_clones$clones) | duplicated(fixing_clones$clones, fromLast = TRUE),]
    fixing_clones <- fixing_clones[order(fixing_clones$TP),]
    
    for(fxc in unique(fixing_clones$clones)){
        
        fixing_clones_i <- fixing_clones[fixing_clones$clones == fxc,]
        for(i in 1:(nrow(fixing_clones_i)-1)) {
            
            if(sum(cl_edges$from == fixing_clones_i$names[i] & cl_edges$to == fixing_clones_i$names[i+1] & cl_edges$type ==  2L) == 0) {
                cl_edges <- rbind(cl_edges, data.frame(from = fixing_clones_i$names[i], 
                                                       to = fixing_clones_i$names[i+1],
                                                       type = 2L,
                                                       extincion = FALSE,
                                                       stringsAsFactors = FALSE)
                )
            }
            
        }
        
    }

    # Setting edges labels
    cl_edges$label <- ""
    cl_edges$name <- ""
    
    for(i in 1:nrow(cl_edges)) {
        if(cl_edges$type[i] == 2L) {
            cl_edges$label[i] <- ""
            cl_edges$name[i] <- ""
        }
        else {
            cl_edges$label[i] <- cl_vertex$last_mutation[cl_vertex$names == cl_edges$to[i]]
            cl_edges$name[i] <- cl_edges$label[i]
        }
    }
    
    if(labels_show == "clones" || labels_show == "none") { 
        cl_edges$label <- ""
    }
    
    # Setting the size of the clone circles
    cl_vertex <- cl_vertex[(cl_vertex$names %in% cl_edges$from | cl_vertex$names %in% cl_edges$to),]
    cl_vertex$size <- 2*sqrt(size*cl_vertex$prevalance/pi)
    cl_vertex$size2 <- 2*sqrt(size2*cl_vertex$prevalance/pi)
    cl_vertex$shape <- "circle"
    
    # Setting the size of the extincted clones
    cl_vertex$size[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$extincion])] <- 2*sqrt(size*0.01)
    cl_vertex$size2[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$extincion])] <- 2*sqrt(size2*0.01)
    cl_vertex$shape[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$extincion])] <- "vrectangle"
    
    cl_vertex$label.dist <- -1
    cl_vertex$label.degree <- 0
    
    # Removing labels from persistent clones
    idx_persistent <- sapply(cl_vertex$names, 
                             function(x){
                                 mut_type <- cl_edges$type[cl_edges$to == x]
                                 !(2L %in% mut_type) & length(mut_type) > 0
                             })
    cl_vertex$label[idx_persistent] <- ""
    if(is.null(clone_labels) && labels_show != "clones" && labels_show != "both") {
        cl_vertex$label.dist <- -1
    } else {
        cl_vertex$label.dist[idx_persistent] <- -1 
    }
    
    
    # Add prevalence
    if(show_prev) {
        num_str <- sub("^(-?)0.", "\\1.", sprintf("%.2f", cl_vertex$prevalance))
        cl_vertex$label <- paste0(cl_vertex$label, " (", num_str , ")")
        cl_vertex$label[cl_vertex$prevalance == 0.0] <- ""
        
    }
    
    # Defining extincted clones as 'ext.'
    cl_vertex$extincion <- 0
    cl_vertex$extincion[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$extincion])] <- 1
    
    cl_vertex$label[cl_vertex$extincion == 1] <- ""
    if((labels_show == "clones" || labels_show == "both") && labels_show != "none") {
        cl_vertex$label[cl_vertex$extincion == 1] <- "ext."
        cl_vertex$label.dist[cl_vertex$extincion == 1] <- 1.2  
    }
    
    cl_edges$lty <- ifelse(cl_edges$type == 2L, yes = 2, no = 1)
    
    g <- igraph::graph_from_data_frame(cl_edges, directed=TRUE, vertices=cl_vertex)

    parental_clones <- which(apply(X = adjMatrix_base, MARGIN = 2, function(x) sum(x == 1))==0)
    
    c_br <- 1
    c_level <- 1
    
    while(length(parental_clones)>0) {
        
        c_son <- which(adjMatrix_base[parental_clones[1],]==1)
        
        cl_vertex$branch[cl_vertex$last_mutation == colnames(adjMatrix_base)[parental_clones[1]]] <- c_br
        cl_vertex$branch_level[cl_vertex$last_mutation == colnames(adjMatrix_base)[parental_clones[1]]] <- c_level
        
        if(length(c_son)==0) {
            parental_clones <- parental_clones[-1]
            c_br <- c_br + 1
            c_level <- 1
        }
        else if(length(c_son) > 1) {
            c_br <- c_br + 1
            c_level <- 1
            parental_clones <- c(parental_clones[-1], c_son)
        }
        else {
            c_level <- c_level + 1
            parental_clones <- c(c_son, parental_clones[-1])
        }
        
    }
    
    # Initialize palette
    color_palette <- data.frame(base_color = c("#FF0000","#00FF00","#0000FF","#FF7F00","#8B00FF","#FFFF00","#2E2B5F"), stringsAsFactors = FALSE)
    color_palette$bright <- NA
    color_palette$dark <- NA
    
    for(i in 1:nrow(color_palette)) {
        cp <- colorRampPalette(c("#FFFFFF",color_palette$base_color[i],"#000000"), interpolate = "spline")(100)
        color_palette$bright[i] <- cp[15]
        color_palette$dark[i] <- cp[85]
    }
    
    for(br in unique(cl_vertex$branch[cl_vertex$last_mutation != "Root"])) {
        n_levels <- max(cl_vertex$branch_level[cl_vertex$last_mutation != "Root"])
        
        min_level <- 6 
        
        if(n_levels < min_level) {
            n_levels = min_level
        }
        
        base_colors <- br - 1
        base_colors <- (base_colors - floor(base_colors/nrow(color_palette))*nrow(color_palette)) + 1
        
        cl_vertex$color[cl_vertex$branch == br] <- colorRampPalette(c(color_palette$bright[base_colors],
                                                                      color_palette$dark[base_colors]),
                                                                    interpolate = "linear")(n_levels)[cl_vertex$branch_level[cl_vertex$branch == br]]
        
    }
    
    cl_vertex$color[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$extincion])] <- "#C0C0C0"
    cl_vertex$color[cl_vertex$last_mutation == "Root"] <- "#DDDDDD"
    
    igraph::vertex_attr(graph = g, name = "branch") <- cl_vertex$branch
    igraph::vertex_attr(graph = g, name = "branch_level") <- cl_vertex$branch_level
    igraph::vertex_attr(graph = g, name = "color") <- cl_vertex$color
    
    igraph::delete_edge_attr(graph = g, name = "extincion")

    cl_vertex$coord.x <- NA
    cl_vertex$coord.y <- NA

    g <- igraph::graph_from_data_frame(cl_edges, directed=TRUE, vertices=cl_vertex)
    #adjMatrix_overall <- igraph::get.adjacency(g, sparse = FALSE, attr = "type", names = TRUE)
    adjMatrix_overall <- igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "type", names = TRUE)
    
    # adjMatrix_overall[which(adjMatrix_overall=="")] <- 0
    # adjMatrix_overall[which(adjMatrix_overall=="persistence")] <- 2
    # adjMatrix_overall[which(adjMatrix_overall=="parental")] <- 1
    # storage.mode(adjMatrix_overall) <- "integer"
    
    # Count total level number in each time point
    timepoints <- unique(cl_vertex$TP)
    
    mut_TP <- c()
    for(tp in timepoints) {
        
        col_in <- which(colnames(adjMatrix_base) %in% cl_edges$name[cl_edges$to %in% cl_vertex$names[cl_vertex$TP == tp]])
        
        if(length(col_in) < 2) {
            mut_TP <- c(mut_TP, 0)
        } else {
            
            adjMatrix_base_tp <- adjMatrix_base[col_in, col_in]
            
            idx_anc_c <- which(colSums(adjMatrix_base_tp) == 0)
            
            max_deep_tp = -1
            for(idx in idx_anc_c){
                descend = list(
                    adjM = adjMatrix_base_tp,
                    l_path = 0,
                    ind_lPath = 1,
                    mut_list = NA,
                    row_v = idx
                )
                deep_tmp <- recursiveDescend(descend)$max_deep
                max_deep_tp <- max(max_deep_tp,deep_tmp) 
            }
            mut_TP <- c(mut_TP, max_deep_tp)
        }
    }
    
    
    mut_TP <- cumsum(mut_TP) + 1:length(mut_TP)
    names(mut_TP) <- timepoints
    
    cl_df <- list(cl_vertex = cl_vertex, cl_edges = cl_edges)
    idx_next_v <-  which(colSums(adjMatrix_overall != 0) == 0)
    
    # Root
    Xc = 0
    Yc = 0
    
    # Start building
    cl_df <- recursiveLongitudinalLayout(idx_Vc=idx_next_v, 
                                         Xc=Xc, 
                                         Yc=Yc,
                                         cl_df=cl_df,
                                         adjMatrix_base=adjMatrix_base,
                                         adjMatrix_overall=adjMatrix_overall,
                                         mut_TP=mut_TP,
                                         labels_show=labels_show,
                                         label_offset=label_offset)
    
    cl_vertex <- cl_df$cl_vertex[order(cl_df$cl_vertex$TP, cl_df$cl_vertex$coord.y),]
    cl_edges <- cl_df$cl_edges
    
    
    
    
    
    g_mod <- igraph::graph_from_data_frame(d = cl_edges, directed=TRUE, vertices=cl_vertex)

    time_point_grp <- split(cl_vertex$names[cl_vertex$last_mutation!="Root"], cl_vertex$TP[cl_vertex$last_mutation!="Root"])
    
    ratio = par("din")[1] / par("din")[2]
    
    g_mod$layout <- igraph::norm_coords(as.matrix(cl_vertex[,c("coord.x", "coord.y")]), xmin = -1*ratio, xmax = 1*ratio, ymin = 1, ymax = -1)
    
    if(tk_plot) {
        igraph::tkplot(g_mod,
               rescale = FALSE,
               vertex.label.dist = 0,
               vertex.label.cex = label.cex,
               edge.label.cex = label.cex,
               edge.arrow.size = 0,
        )        
    } else if(show_plot) {
        if(tp_mark) {
            plot(g_mod,
                 mark.groups = time_point_grp,
                 mark.shape = 1,
                 mark.border = 0,
                 mark.col = paste0(RColorBrewer::brewer.pal(9, 'Pastel1'), as.hexmode(round(tp_mark_alpha*255))),
                 rescale = FALSE,
                 vertex.label.cex = label.cex,
                 edge.label.cex = label.cex,
                 edge.arrow.size = 0
            )
        } else {
            plot(g_mod,
                 rescale = FALSE,
                 vertex.label.cex = label.cex,
                 edge.label.cex = label.cex,
                 edge.arrow.size = 0
            )
        } 
        if(legend && tp_mark) {
            legend(x = legend_position, 
                   legend = paste0("Time Point ", names(time_point_grp)), 
                   fill = RColorBrewer::brewer.pal(9, 'Pastel1')[1:length(time_point_grp)], 
                   cex = legend_cex)
        } 
        if(tp_lines) {
            TPs <- sort(unique(cl_vertex$TP))
            
            min_x <- min(g_mod$layout[,1], na.rm = TRUE)
            min_x <- ifelse(test = min_x < 0, yes = min_x*1.3, no = min_x*0.7)
            
            for(i in 1:(length(TPs))) {
                
                if(i < length(TPs)) {
                    range_Y_curr <- range(g_mod$layout[cl_vertex$TP == TPs[i],2], na.rm = TRUE)
                    range_Y_next<- range(g_mod$layout[cl_vertex$TP == TPs[i+1],2], na.rm = TRUE)
                    
                    pos_l <- mean(c(range_Y_curr[1],range_Y_next[2]))
                    
                    abline(h = pos_l)
                }
                pos_text <- mean(range(g_mod$layout[cl_vertex$TP == TPs[i],2], na.rm = TRUE))
                
                text(min_x, pos_text, paste0('TP: ', TPs[i]))
                
            }
            
        }
    }
    
    if(endsWith(x = filename,suffix = ".dot")) {
        igraph::write_graph(graph = g, file = filename, format = "dot")
    } else if(endsWith(x = filename,suffix = ".xml")) {
        igraph::write_graph(graph = g, file = filename, format = "graphml")
    } else if(filename != "" || !is.null(filename) || !is.na(filename)) {
        warning("Filename is not valid. Only dot or graphml format are currently supported.")
    }
    
    return(g)
    
}
