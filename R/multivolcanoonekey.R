#' multivolcanoonekey
#'
#' @param res data.frame of DEGs to be plot
#' @param cluster the colname of clusters
#' @param logFC the colname of log2FoldChange
#' @param padj the colname of adjusted P value
#' @param gene the colname of gene
#' @param top10 boolean if top10 DEGs labeled
#' @param title the title of the plot
#' @param colors the vector of colors of the clusters
#' @param levels the vector of levels of the clusters
#'
#' @return a ggplot2 object of multiple volcano plots
#'
#' @import ggplot2
#' @import ggrepel
#' @import tidyverse
#' @import ggnewscale
#'
#' @export
#'
#' @examples
#' library(ggonekey)
#' multivolcanoonekey(
#'     res = DEG,
#'     cluster = "cluster",
#'     logFC = "logFC",
#'     padj = "adj.P.Val",
#'     gene = "gene",
#'     top10 = T
#' )
multivolcanoonekey = function(
        res,
        cluster,
        logFC,
        padj,
        gene,
        top10 = F,
        title = NA,
        colors = NA,
        levels = NA
) {
    library(ggplot2)
    library(ggrepel)
    library(tidyverse)
    library(ggnewscale)

    res <- as.data.frame(res)
    res$group <- ifelse(
        res[, padj] >= 0.05, "ns",
        ifelse(
            res[, logFC] > 1, "up",
            ifelse(
                res[, logFC] < -1, "down",
                "ns"
            )
        )
    )

    res$group <- factor(res$group, levels = c("down", "ns", "up"))

    if(!sum(is.na(levels))) {
        res[, cluster] <- factor(res[, cluster], levels = levels)
    }else{
        res[, cluster] <- factor(res[, cluster])
    }
    res$x <- jitter(as.numeric(res[, cluster]), factor = 1.8)

    top10_up <- res[res$group == 'up', ] %>% group_by_at(cluster) %>% top_n(10, logFC)
    top10_down <- res[res$group == 'down', ] %>% group_by_at(cluster) %>% top_n(-10, logFC)

    top10_up <- as.data.frame(top10_up)
    top10_down <- as.data.frame(top10_down)

    if(!sum(is.na(levels))) {
        top10_up[, cluster] <- factor(top10_up[, cluster], levels = levels)
        top10_down[, cluster] <- factor(top10_down[, cluster], levels = levels)
    }

    tile <- data.frame(x = 1:length(levels(res[, cluster])), y = 0, fill = levels(res[, cluster]))
    text <- data.frame(x = 1:length(levels(res[, cluster])), y = 0, label = levels(res[, cluster]))

    tile$fill <- factor(tile$fill, levels = levels(res[, cluster]))
    text$label <- factor(text$label, levels = levels(res[, cluster]))

    p <- ggplot() +
        ggtitle(title) +
        geom_point(
            data = res,
            mapping = aes_string(
                x = "x",
                y = logFC,
                size = paste0("-log10(", padj, ")"),
                alpha = paste0("-log10(", padj, ")"),
                color = "group"
            )
        ) +
        geom_tile(
            data = tile,
            mapping = aes(x = x, y = 0, width = 1, height = 2, fill = fill),
            alpha = 0.8,
            color = "black",
            show.legend = F
        ) +
        scale_size_continuous(
            breaks = c(0, 0.33, 0.67, 1) * max(-log10(res[, padj])),
            labels = signif(10^-(c(0, 0.33, 0.67, 1) * max(-log10(res[, padj]))), digits = 3)
        ) +
        scale_alpha_continuous(
            breaks = c(0, 0.33, 0.67, 1) * max(-log10(res[, padj])),
            labels = signif(10^-(c(0, 0.33, 0.67, 1) * max(-log10(res[, padj]))), digits = 3)
        ) +
        geom_text(
            data = text,
            mapping = aes(x = x, y = 0, label = label)
        ) +
        scale_color_manual(values = c("blue", "gray", "red")) +
        labs(x = "Clusters", y = "log2FoldChange") +
        theme_minimal() +
        theme(
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.box = "vertical",
            legend.spacing = unit(0, "npc")
        )

    if(!sum(is.na(colors))) {
        p <- p +
            scale_fill_manual(values = colors)
    }

    if(top10) {
        p <- p +
            new_scale_color() +
            geom_text_repel(
                data = top10_up,
                aes_string(x = "x", y = logFC, label = gene, color = cluster),
                min.segment.length = 0,
                max.overlaps = Inf,
                size = 3,
                show.legend = F
            ) +
            geom_text_repel(
                data = top10_down,
                aes_string(x = "x", y = logFC, label = gene, color = cluster),
                min.segment.length = 0,
                max.overlaps = Inf,
                size = 3,
                show.legend = F
            )
    }

    if(!sum(is.na(colors))) {
        p <- p +
            scale_color_manual(values = colors)
    }

    return(p)
}
