#' sankeyonekey
#'
#' @param make_long the data.frame after ggsankey::make_long()
#' @param title the title of the plot
#'
#' @return a ggplot2 object of a sankey plot
#'
#' @import ggplot2
#' @import ggsankey
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggsankey)
#' data <- data.frame(
#'   group1 = c(1, 1, 2, 2),
#'   group2 = c(3, 4, 3, 4)
#' )
#' data <- ggsankey::make_long(data, group1, group2)
#'
#' data$node <- factor(data$node, levels = rev(unique(data$node)))
#' data$next_node <- factor(data$next_node, levels = rev(unique(data$next_node)))
#'
#' sankeyonekey(make_long = data, title = "TITLE")
sankeyonekey <- function(make_long, title = NULL) {
  p <- ggplot(
    data = make_long,
    mapping = aes(
      x = make_long$x,
      next_x = make_long$next_x,
      node = make_long$node,
      next_node = make_long$next_node,
      fill = make_long$node,
      label = make_long$node
    ),
  ) +
      geom_sankey(
          flow.alpha = 0.6,
          width = 0.05,
          show.legend = F
      ) +
      geom_sankey_text(
          size = 4,
          angle = 0,
          hjust = 0,
          position = position_nudge(x = 0.05)
      ) +
      theme_minimal() +
      theme(
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold", size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
      ) +
      labs(x = NULL) +
      ggtitle(title)
  return(p)
}
