#' forestonekey
#'
#' @param data the data.frame that is to be drawn
#' @param features the colname of which contains the features
#' @param HR the colname of which contains Hazard Rate
#' @param l95 the colname of which contains the lower .95 CI
#' @param h95 the colname of which contains the higher .95 CI
#' @param pvalue the colname of which contains the pvalue
#' @param title the title of the plot
#'
#' @return a ggplot object of a forest plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
#' @examples
#' library(ggplot2)
#' unicox <- data.frame(
#'   id = c("a", "b", "c"),
#'   hr = c(0.5, 1, 1.5),
#'   low.95 = c(0.45, 0.9, 1.1),
#'   high.95 = c(0.55, 1.1, 1.9),
#'   p.value = c(0.01, 0.01, 0.01)
#' )
#'
#' forestonekey(
#'   data = unicox,
#'   features = "id",
#'   l95 = "low.95",
#'   h95 = "high.95",
#'   HR = "hr",
#'   pvalue = "p.value",
#'   title = "TITLE"
#' )
forestonekey <- function(data, features, HR, l95, h95, pvalue, title = NULL) {
  data$group <- ifelse(data[, pvalue] >= 0.05, "ns",
    ifelse(data[, h95] < 1, "protective",
      ifelse(data[, l95] > 1, "risk",
        "ns"
      )
    )
  )
  data$group <- factor(data$group, levels = c("protective", "ns", "risk"))
  labels <- unique(data$group)
  labels <- factor(labels, levels = c("protective", "ns", "risk"))
  values <- c()
  if ("protective" %in% labels) {
    values <- append(values, "forestgreen")
  }
  if ("ns" %in% labels) {
    values <- append(values, "gray")
  }
  if ("risk" %in% labels) {
    values <- append(values, "firebrick3")
  }
  data$label <- paste0("(", format(data[, l95], digits = 3, scientific = T), "~", format(data[, h95], digits = 3, scientific = T), ")")
  data$l.HR <- format(data[, HR], digits = 3, scietific = T)
  data$l.p <- format(data[, pvalue], digits = 3, scientific = T)
  total <- exp(2 * log(max(data[, h95])) - log(min(data[, l95])))
  p <- ggplot() +
    geom_errorbar(
      data = data,
      mapping = aes_string(y = features, xmin = l95, xmax = h95, color = "group"),
      linewidth = 1.25,
      width = 0.3
    ) +
    geom_point(
      data = data,
      mapping = aes_string(y = features, x = HR, color = "group"),
      size = 3
    ) +
    geom_vline(xintercept = 1, color = "steelblue") +
    scale_x_continuous(trans = "log", limits = c(min(data[, l95]), total), breaks = c(signif(min(data[, l95]), 2), signif(sqrt(min(data[, l95])), 2), 1, signif(sqrt(max(data[, h95])), 2), signif(max(data[h95]), 2))) +
    scale_y_discrete(labels = data[, features]) +
    scale_color_manual(
      # labels = labels,
      values = values
    ) +
    labs(x = "Hazard Ratio", y = NULL) +
    ggtitle(title) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.justification = c(0.5, 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      # panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_blank(),
      axis.title.x = element_text(hjust = 0.5),
      axis.text.x = element_text(hjust = 1, angle = 90),
      axis.title.y = element_blank(),
      axis.text.y = element_text(face = "bold")
    ) +
    geom_text(
      data = data,
      aes_string(x = as.character(total^(17 / 32)), y = features, label = "l.HR"),
      hjust = 0,
      size = 3
    ) +
    geom_text(
      aes(x = total^(17 / 32), y = length(data[, features]) + 0.5, label = "HR"),
      hjust = 0,
      fontface = "bold"
    ) +
    geom_text(
      data = data,
      aes_string(x = as.character(total^(24 / 32)), y = features, label = "label"),
      hjust = 0.5,
      size = 3
    ) +
    geom_text(
      aes(x = total^(24 / 32), y = length(data[, features]) + 0.5, label = "95% CI"),
      hjust = 0.5,
      fontface = "bold"
    ) +
    geom_text(
      data = data,
      aes_string(x = as.character(total^(31 / 32)), y = features, label = "l.p"),
      hjust = 1,
      size = 3
    ) +
    geom_text(
      aes(x = total^(31 / 32), y = length(data[, features]) + 0.5, label = "pvalue"),
      hjust = 1,
      fontface = "bold"
    )
  return(p)
}
