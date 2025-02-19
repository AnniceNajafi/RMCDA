#' Plot decision tree
#'
#' @param A the comparison matrix
#' @param comparing.competitors the list of matrices related to pairwise comparisons of competitors for each criteria
#' @param results results of running AHP on data
#' @param vertex_font font of text on vertex
#' @param edge_font size of the arrows
#' @param asp aspect ratio of the graph
#' @param max_width maximum width
#' @param vertex_size vertex size
#'
#' @return the decision tree plot
#' @export plot.AHP.decision.tree
plot.AHP.decision.tree <- function(A, comparing.competitors, results, vertex_font=1.2, edge_font = 1,
                                   asp = 0.8, max_width = 5, vertex_size=50){

  nodes <- c("Choose alternative",rownames(comparing.competitors[[1]]), rownames(A))

  edges <- c()


  for(idx in seq(1, length(rownames(A)), 1)){
    edges <- c(edges, "Choose alternative")
    edges <- c(edges, rownames(A)[idx])
  }

  for(idx in seq(1,length(rownames(comparing.competitors[[1]])), 1)){
    for(idx.prod in seq(1, length(rownames(A)), 1)){
      edges <- c(edges, rownames(A)[idx.prod])
      edges <- c(edges, rownames(comparing.competitors[[1]])[idx])
    }
  }

  edge_list <- matrix(edges, ncol = 2, byrow = TRUE)
  weights <- c(results[[1]][[2]],
               as.vector(results[[3]]))

  g <- graph_from_edgelist(edge_list)

  E(g)$weight <- weights


  edge_widths <- max_width * (weights / max(weights))


  p <- plot(g,
            layout = layout_as_tree(g, root = 1),  # Tree-like layout with root at the top
            vertex.size = vertex_size,  # Adjust node size
            vertex.label.cex = vertex_font,  # Font size for labels
            vertex.label.color = "black",  # Label color
            vertex.color="#9B7EBD",
            edge.arrow.size = edge_font,  # Arrow size
            edge.width = edge_widths*edge_font,  # Edge widths based on weights
            asp = asp,
            main = "AHP Decision Tree with Weighted Edges")
  return(p)
}

#' Plot spider plot
#'
#' @param data the result of MCDA scores
#' @param colors the color scheme of choice
#'
#' @return the spider plot
#' @export plot.spider
plot.spider <- function(data, colors=palette("default")){

  as.data.frame(data)->data
  rownames(data)->criteria
  data <- rbind(rep(1, ncol(data)), rep(0, ncol(data)), data)

  radarchart(data,
             axistype = 2,
             pcol=colors,
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.3,
             plwd = 2,
             plty = 1,
             title = "Spider Chart")

  legend(x = "topright",
         legend = criteria,
         col = colors,
         pch = 15,
         bty = "n")
}
