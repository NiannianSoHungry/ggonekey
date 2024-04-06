#' Title
#'
#' @param data the data.frame to be drawn
#' @param gene the colname of genes
#' @param logfc the colname of log2FoldChange
#' @param pvalue the colname of p value or adjusted p value
#' @param title the title of the plot
#' @param labeled if the top 10 genes are to be labeled
#' @param colors a vector of three colors on behalf of "down", "ns" and "up"
#'
#' @return a ggplot2 object of a volcano plot
#'
#' @import DESeq2
#' @import ggplot2
#' @import ggrepel
#'
#' @export
#'
#' @examples
#' library(DESeq2)
#' library(ggplot2)
#' library(ggrepel)
#' set.seed(123)
#' cnts <- matrix(rnbinom(n = 1000, mu = 100, size = 1 / 0.5), ncol = 10)
#' cond <- factor(rep(1:2, each = 5))
#'
#' dds <- DESeq2::DESeqDataSetFromMatrix(cnts, DataFrame(cond), ~cond)
#'
#' dds <- DESeq2::DESeq(dds)
#' res <- DESeq2::results(dds)
#'
#' DEG <- as.data.frame(res)
#' DEG$gene <- rownames(DEG)
#'
#' volcanoonekey(
#'   data = DEG,
#'   gene = "gene",
#'   logfc = "log2FoldChange",
#'   pvalue = "padj",
#'   title = "TITLE",
#'   labeled = TRUE
#' )
volcanoonekey <- function(data, gene, logfc, pvalue, title = NULL, labeled = F, colors = c("blue", "gray", "red")) {
    data$group <- ifelse(data[, pvalue] >= 0.05, "ns",
                         ifelse(data[, logfc] < -1, "down",
                                ifelse(data[, logfc] > 1, "up",
                                       "ns"
                                )
                         )
    )
    data$group <- factor(data$group, levels = c("down", "ns", "up"))
    labels <- unique(data$group)
    labels <- factor(labels, levels = c("down", "ns", "up"))
    values <- c()
    if ("down" %in% labels) {
        values <- append(values, colors[1])
    }
    if ("ns" %in% labels) {
        values <- append(values, colors[2])
    }
    if ("up" %in% labels) {
        values <- append(values, colors[3])
    }
    data$p2 <- -log10(data[, pvalue])
    p <- ggplot(data = data, mapping = aes_string(logfc, "p2", color = "group"), size = 2) +
        geom_point() +
        theme_bw() +
        scale_color_manual(
            values = values
    ) +
    geom_vline(xintercept = c(-1, 1), lty = 2, color = "gray") +
    geom_hline(yintercept = -log10(0.05), lty = 2, color = "gray") +
    ggtitle(title) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 18
      ),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(x = "log2FoldChange", y = "-log10(adjusted P Value)")
  if (labeled) {
    p <- p +
      geom_text_repel(
        data = rbind(data[rev(order(data[, logfc]))[1:10], ], data[order(data[, logfc])[1:10], ]),
        mapping = aes_string(logfc, "p2", label = gene),
        box.padding = 0.5,
        min.segment.length = 0.1,
        color = "black"
      )
  }
  return(p)
}
