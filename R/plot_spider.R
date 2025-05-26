#' Plot spider plot
#'
#' @param data the result of MCDA scores
#' @param colors the color scheme of choice
#'
#' @return the spider plot
#' @importFrom grDevices palette
#' @importFrom fmsb radarchart
#' @importFrom graphics legend
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
