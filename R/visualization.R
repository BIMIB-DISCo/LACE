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
#' @param labels Specify which type of label should be placed on the tree; options are, 
#'   "mutations": parental edges are labeled with the acquired mutation between the two nodes (genotypes); 
#'   "clones": nodes (genotypes) are labeled with their last acquired mutation; 
#'   "both": either nodes and edges are labeled as specified above; 
#'   "none": no labels will show on the longitudinal tree. 
#' @param clone_labels Character vector that specifies the name of the nodes (genotypes). If it is NULL (default), nodes will be labeled as specified by "label" parameter.
#' @param label.cex Specify the size of the labels.
#' @param iter_max Maximum number of iteration to be used to remove intersecting edges.
#' @param size Specify size of the nodes. The final area is proportional with the node prevalence.
#' @param size2 Specify the size of the second dimension of the nodes. If NULL (default), it is set equal to "size".
#' @param tk_plot If TRUE, uses tkplot function from igraph library to plot an interactive tree. Default is FALSE.
#' @param tp_mark If TRUE (defaul) the function draws different colored area under the nodes in different time points.
#' @param tp_mark_alpha Specify the alpha value of the area drawed when tp_mark = TRUE.
#' @param legend If TRUE (default) a legend will be displayed on the plot.
#' @param legend_position Specify the legend position.
#' @param legend_cex Specify size of the legend text.
#' @export longitudinal.tree.plot
#' @import graphics
#' @import grDevices
#' @import igraph
#' @import RColorBrewer
#' @import utils
#'
longitudinal.tree.plot <- function( inference, labels = "mutations", clone_labels = NULL, label.cex = 1, iter_max = 100, size = 500, size2 = NULL, tk_plot = FALSE, tp_mark = TRUE, tp_mark_alpha = 0.5, legend = TRUE, legend_position = "topleft", legend_cex = 0.8 ) {
    
    if(is.null(size2)) {
        size2 <- size
    }
    
    cl_vertex <- data.frame(prevalance=as.vector(inference$clones_prevalence[,-ncol(inference$clones_prevalence)]))
    cl_vertex$clone <- rep(c(1:nrow(inference$clones_prevalence)),ncol(inference$clones_prevalence)-1)
    cl_vertex$TP <- rep(1:(ncol(inference$clones_prevalence)-1),each=nrow(inference$clones_prevalence))
    cl_vertex$last_mutation <- rep(unlist(lapply(inference$clones_summary, function(x){tail(x, 1)})), ncol(inference$clones_prevalence)-1)
    
    if(!is.null(clone_labels)) {
        if(length(clone_labels)!=length(inference$clones_summary)) {
            warning("Number of labels is different from number of clones.")
            cl_vertex$label <- ""
        }
        else {
            cl_vertex$label <- rep(clone_labels,ncol(inference$clones_prevalence)-1)
        }
    }
    else if((labels == "clones" || labels == "both") && labels != "none") {
        cl_vertex$label <-rep(names(inference$clones_summary), ncol(inference$clones_prevalence)-1)
    }
    else {
        cl_vertex$label <- ""
    }
    
    cl_vertex$branch <- NA
    cl_vertex$branch_level <- 0
    cl_vertex$names <- paste0("T", cl_vertex$TP, "_", cl_vertex$last_mutation)
    
    # names must be the first column
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
                        cl_edges <- rbind(cl_edges, data.frame(from = from_cc, to = to_cc, type = "persistence", existinciont = FALSE))
                        from_cc <- to_cc
                    }
                    else {
                        to_cc <- cl_vertex$names[cl_vertex$clone == c & cl_vertex$TP == nt]
                        cl_edges <- rbind(cl_edges, data.frame(from = from_cc, to = to_cc, type = "persistence", existinciont = TRUE))
                        break;
                    }
                    nt <- nt + 1
                }
                t <- nt
            }
        }
    }
    
    # Processing parental relations
    adjMatrix_base <- as.adj.matrix.unsorted(inference$B)
    
    # Start from one leaf
    B_leaf <- which(apply(X = adjMatrix_base, MARGIN = 1, FUN = sum)==0)
    for(cs_id in B_leaf) {
        curTP <- max(cl_vertex$TP)
        cp_id <- which(adjMatrix_base[,cs_id]==1)
        while(length(cp_id)>0) {
            
            # Finding first time point where children clone appears
            TPs <- cl_vertex$TP[cl_vertex$clone == cs_id & cl_vertex$prevalance > 0 & cl_vertex$TP <= curTP]
            
            if(length(TPs)==0) {
                cs_tp <- curTP
            }
            else {
                cs_tp <- min(TPs)
            }
            
            # Finding the time point where parental clone is present as close as possible to current time point
            TPs <- cl_vertex$TP[cl_vertex$clone == cp_id & cl_vertex$prevalance > 0 & cl_vertex$TP <= cs_tp]
            
            if(length(TPs) == 0) {
                # If the parental clone has ever had prevalence == 0 -> I use the same time point of the son clone
                cp_tp = cs_tp
            }
            else {
                cp_tp <- max(TPs)
            }
            
            from_cc <- cl_vertex$names[cl_vertex$clone == cp_id & cl_vertex$TP == cp_tp]
            to_cc <- cl_vertex$names[cl_vertex$clone == cs_id & cl_vertex$TP == cs_tp]
            mut <- cl_vertex$last_mutation[cl_vertex$clone == cs_id & cl_vertex$TP == cs_tp]
            cl_edges <- rbind(cl_edges, data.frame(from = from_cc, 
                                                   to = to_cc, 
                                                   type = "parental", 
                                                   existinciont = FALSE, 
                                                   stringsAsFactors = FALSE))
            
            # Now child clone becomes parental clone
            curTP <- cp_tp
            cs_id <- cp_id
            cp_id <- which(adjMatrix_base[,cs_id]==1)
            
        }
    }
    
    cl_edges <- cl_edges[!duplicated.data.frame(cl_edges[,c("from", "to", "type")]),]
    
    # Fixing missing prevalence relations
    included_clones_names <- unique(c(as.character(cl_edges$from), as.character(cl_edges$to)))
    for(icn in included_clones_names) {
        ic <- cl_vertex$clone[cl_vertex$names == icn]
        ic_tp <- cl_vertex$TP[cl_vertex$names == icn]
        next_cln <- cl_vertex$names[cl_vertex$clone == ic & cl_vertex$TP == (ic_tp + 1)]
        if(length(next_cln) == 0) {
            next;
        }
        else {
            if(sum(cl_edges$from == icn & cl_edges$to == next_cln) == 0 && next_cln %in% included_clones_names) {
                cl_edges <- rbind(cl_edges, data.frame(from = icn, 
                                                       to = next_cln, 
                                                       type = "persistence", 
                                                       existinciont = FALSE, 
                                                       stringsAsFactors = FALSE))
            }
        }
        
    }
    
    # Setting edges labels
    cl_edges$label <- ""
    
    if((labels == "mutations" || labels == "both")  && labels != "none") {
        for(i in 1:nrow(cl_edges)) {
            if(cl_edges$type[i] == "persistence") {
                cl_edges$label[i] <- ""
            }
            else {
                cl_edges$label[i] <- cl_vertex$last_mutation[cl_vertex$names == cl_edges$to[i]]
            }
        }
    }
    
    # Setting the size of the clone circles
    cl_vertex <- cl_vertex[(cl_vertex$names %in% cl_edges$from | cl_vertex$names %in% cl_edges$to),]
    cl_vertex$size <- 2*sqrt(size*cl_vertex$prevalance/pi)
    cl_vertex$size2 <- 2*sqrt(size2*cl_vertex$prevalance/pi)
    cl_vertex$shape <- "circle"
    
    # Setting the size of the extincted clones
    cl_vertex$size[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$existinciont])] <- 2*sqrt(size*0.01)
    cl_vertex$size2[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$existinciont])] <- 2*sqrt(size2*0.01)
    cl_vertex$shape[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$existinciont])] <- "rectangle"
    
    # Removing labels from persistent clones
    cl_vertex$label[sapply(cl_vertex$names, 
                           function(x) {
                               mut_type <- cl_edges$type[cl_edges$to == x]
                               !("parental" %in% mut_type) & length(mut_type) > 0
                           })
                    ] <- NA
    
    # Defining extincted clones as 'ext.'
    if((labels == 'clones' || labels == 'both') && labels != 'none') {
        cl_vertex$label[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$existinciont])] <- "ext."
    }
    
    cl_edges$lty <- ifelse(cl_edges$type == "persistence", yes = 2, no = 1)
    
    g <- graph_from_data_frame(cl_edges, directed=TRUE, vertices=cl_vertex)
    
    # get coordinate for each vertex
    org_coordinates <- layout_(g, as_tree())
    
    cl_vertex$coord.x <- round(org_coordinates[,1], digits = 3)
    cl_vertex$coord.y <- round(org_coordinates[,2], digits = 3)
    
    # Navigate the tree from the root and set color  based on relation
    parental_clones <- which(apply(X = adjMatrix_base, MARGIN = 2, function(x) sum(x == 1))==0)
    
    c_br <- 1
    c_level <- 1
    
    while(length(parental_clones)>0) {

        # get child node
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
    
    for(br in unique(cl_vertex$branch)) {
        n_levels <- max(cl_vertex$branch_level)
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

    cl_vertex$color[cl_vertex$names %in% as.character(cl_edges$to[cl_edges$existinciont])] <- "#C0C0C0"

    # Fixing clone y position based on the time point 
    for(tp in seq(from = 1, to = max(cl_vertex$TP)-1)) {
        m_c <- min(cl_vertex$coord.y[which(cl_vertex$TP == tp)]) - 1
        cl_vertex$coord.y[which(cl_vertex$TP == tp+1 & cl_vertex$coord.y >= m_c)] <- m_c
    }
    
    # Fixing overlapping points
    for(c_x in unique(cl_vertex$coord.x)) {
        idx_coord_y <- which(cl_vertex$coord.x == c_x)[order(org_coordinates[which(cl_vertex$coord.x == c_x,), 2], decreasing = TRUE)]
        if(length(idx_coord_y) < 2) {
            next;
        }
        for(i in 1:(length(idx_coord_y)-1)) {
            if(cl_vertex$coord.y[idx_coord_y[i]] <= cl_vertex$coord.y[idx_coord_y[i+1]]) {
                cl_vertex$coord.y[idx_coord_y[i+1]] = cl_vertex$coord.y[idx_coord_y[i]] - 1
            }
        }
    }
    
    deltaX <- 2*(sum(abs(range(cl_vertex$coord.x))) / length(cl_vertex$coord.x))
    
    # Fixing overlapping edges
    found = TRUE
    maxIter = iter_max
    while(found & maxIter > 0) {
        found = FALSE
        maxIter <- maxIter - 1
        for(i in 1:(nrow(cl_edges)-1)) {
            for(j in 2:nrow(cl_edges)) {
                if(i == j) {
                    next;
                }
                
                edgeA_1 = as.character(cl_edges$from[i])
                edgeA_2 = as.character(cl_edges$to[i])
                edgeB_1 = as.character(cl_edges$from[j])
                edgeB_2 = as.character(cl_edges$to[j])
                
                ax1 <- cl_vertex$coord.x[which(cl_vertex$names == edgeA_1)]
                ay1 <- cl_vertex$coord.y[which(cl_vertex$names == edgeA_1)]
                ax2 <- cl_vertex$coord.x[which(cl_vertex$names == edgeA_2)]
                ay2 <- cl_vertex$coord.y[which(cl_vertex$names == edgeA_2)]
                
                bx1 <- cl_vertex$coord.x[which(cl_vertex$names == edgeB_1)]
                by1 <- cl_vertex$coord.y[which(cl_vertex$names == edgeB_1)]
                bx2 <- cl_vertex$coord.x[which(cl_vertex$names == edgeB_2)]
                by2 <- cl_vertex$coord.y[which(cl_vertex$names == edgeB_2)]
                
                # Check if two ends are on the same place (if truw, increase the Y values)
                if(ax2 == bx2 && ay2 == by2) {
                    ax2 <- ax2 + deltaX
                    cl_vertex$coord.x[which(cl_vertex$names == edgeA_2)] <- ax2
                }
                
                # Check if parental clone have y values minor respect to the son clones ones
                if(ay1 <= ay2) {
                    ay2 <- ay1 - 1
                    cl_vertex$coord.y[which(cl_vertex$names == edgeA_2)] <- ay2
                }
                if(by1 <= by2) {
                    by2 <- by1 - 1
                    cl_vertex$coord.y[which(cl_vertex$names == edgeB_2)] <- by2
                }

                if(length(unique(c(edgeA_1,edgeA_2,edgeB_1,edgeB_2))) < 4 ) {
                    next;
                }
                
                if(checkSegmentIntersect(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)) {
                    found = TRUE
                    cl_vertex$coord.x[which(cl_vertex$names == edgeA_2)] <- bx2
                    cl_vertex$coord.x[which(cl_vertex$names == edgeB_2)] <- ax2 
                }
            }
        }
    }
    
    time_point_grp <- split(cl_vertex$names, cl_vertex$TP)
    
    ratio = par("din")[1] / par("din")[2]
    
    if(tk_plot) {
        tkplot(g,
               layout = norm_coords(as.matrix(cl_vertex[,c("coord.x", "coord.y")]), xmin = -1*(ratio), xmax = 1*(ratio), ymin = -1, ymax = 1),
               rescale = FALSE,
               vertex.label = cl_vertex$label,
               vertex.label.cex = label.cex,
               vertex.size = cl_vertex$size,
               vertex.size2 = cl_vertex$size2,
               vertex.color = cl_vertex$color,
               edge.label = cl_edges$label,
               edge.label.cex = label.cex,
               edge.arrow.size = 0,
               edge.lty = cl_edges$lty)
    }
    else {
        if(tp_mark) {
            plot(g,
                 mark.groups = time_point_grp,
                 mark.shape = 1,
                 mark.border = 0,
                 mark.col = paste0(RColorBrewer::brewer.pal(9, 'Pastel1'), as.hexmode(round(tp_mark_alpha*255))),
                 layout = norm_coords(as.matrix(cl_vertex[,c("coord.x", "coord.y")]), xmin = -1*(ratio), xmax = 1*(ratio), ymin = -1, ymax = 1),
                 rescale = FALSE,
                 vertex.label = cl_vertex$label,
                 vertex.label.cex = label.cex,
                 vertex.size = cl_vertex$size,
                 vertex.size2 = cl_vertex$size2,
                 vertex.color = cl_vertex$color,
                 edge.label = cl_edges$label,
                 edge.label.cex = label.cex,
                 edge.arrow.size = 0,
                 edge.lty = cl_edges$lty)
        }
        else {
            plot(g,
                 layout = norm_coords(as.matrix(cl_vertex[,c("coord.x", "coord.y")]), xmin = -1*(ratio), xmax = 1*(ratio), ymin = -1, ymax = 1),
                 rescale = FALSE,
                 vertex.label = cl_vertex$label,
                 vertex.label.cex = cl_vertex$label,
                 vertex.size = cl_vertex$size,
                 vertex.size2 = cl_vertex$size2,
                 vertex.color = cl_vertex$color,
                 edge.label = cl_edges$label,
                 edge.label.cex = label.cex,
                 edge.arrow.size = 0,
                 edge.lty = cl_edges$lty)
        }
        if(legend && tp_mark) {
            legend(x = legend_position,
                   legend = paste0("Time Point ", names(time_point_grp)),
                   fill = RColorBrewer::brewer.pal(9, 'Pastel1')[1:length(time_point_grp)],
                   cex = legend_cex)
        }
    }
}
