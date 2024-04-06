#' roconekey
#'
#' @param data the data to be drawn
#' @param start the index where the columns to be drawn starts
#' @param end the index where the columns to be drawn ends(default end = start)
#' @param title the title of the plot
#' @param y how high the auc label should be
#' @param truth the colname which contains the ground truth
#' @param nrow how many rows of the plots will be
#' @param colors a vector of two colors of the curves
#'
#' @return a ggplot2 object of multiple ROC plots
#'
#' @import ggplot2
#' @import multipleROC
#' @importFrom grDevices colorRampPalette
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(multipleROC)
#' roconekey(data = mtcars, start = 2, end = 7, truth = "am", nrow = 2)
roconekey <- function(data, start, end = start, title = NULL, y = 0.45, truth, nrow = 1, colors = c("red", "orange")) {
    p1 <- as.list(colnames(data)[c(start:end)])
    names(p1) <- p1
    p1 <- lapply(p1, function(x) {
        multipleROC(as.formula(paste0(truth, "~", x)), data = data, plot = F)
    })
    x <- as.list(p1)
    no <- as.list(1:length(x))
    df <- purrr::map2_dfr(x, no, makeCoord)
    df$no <- factor(df$no)
    df2 <- purrr::map2_dfr(x, no, makeLabels)
    df2$no <- factor(df2$no)
    df2$sens <- substr(df2$sens, 1, nchar(df2$sens) - 1)
    df3 <- data.frame(no = 1:length(x))
    df3$no <- factor(df3$no)
    df3$x <- df2$x
    df3$y <- df2$y
    df3$auc <- lapply(x, function(x) {
        round(x$auc, 3)
    })
    df3$cutoff <- lapply(x, function(x) {
        round(x$cutoff[[1]][1], 3)
    })
    df$name <- lapply(df$no, function(x) {
        names(p1)[x]
    })
    df$name <- as.character(df$name)
    df2$name <- lapply(df2$no, function(x) {
        names(p1)[x]
    })
    df2$name <- as.character(df2$name)
    df3$name <- lapply(df3$no, function(x) {
        names(p1)[x]
    })
    df3$name <- as.character(df3$name)
    df3$label <- paste0("AUC:", df3$auc, "\nCutoff:", df3$cutoff)
    p <- ggplot(df, aes_string(x = "x", y = "y", group = "no", color = "no")) +
        theme_bw() +
        geom_line(
            linewidth = 1.2,
            show.legend = F
        ) +
        geom_point(data = df2, pch = 4, size = 5, stroke = 1.5, show.legend = F) +
        geom_segment(
            aes(
                x = 0, xend = 1,
                y = 0, yend = 1
            ),
            linetype = 6,
            color = "gray50",
            linewidth = 1.2
        ) +
        geom_label(
            data = df2,
      aes_string(x = "1", y = "0", label = "sens"),
      hjust = 1,
      vjust = 0,
      show.legend = F,
      label.size = 1,
    ) +
    geom_label(
      data = df3,
      aes_string(x = "1", y = y, label = "label"),
      hjust = 1,
      vjust = 0,
      show.legend = F,
      label.size = 1
    ) +
    ggtitle(title) +
    scale_color_manual(values = colorRampPalette(colors)(end - start + 1)) +
    labs(x = "1-Specificity", y = "Sensitivity") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title = element_text(hjust = 0.5, face = "bold", size = 12)
    ) +
    coord_fixed(1)
  multiple <- start != end
  if (multiple) {
    p <- p + facet_wrap(facets = . ~ name, nrow = nrow)
  }
  return(p)
}
