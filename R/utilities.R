## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    ggplot_build(plot)$layout[[var]][[1]]$range$range
}


set_axis_label <- function(p, xlab, ylab, p2 = NULL) {
    p <- p + xlab(xlab) + ylab(ylab) +
        theme(axis.title = element_text())

    if (is.null(p2)) {
        has_theme <- FALSE
    } else {
        has_theme <- length(p2$theme) != 0
    }

    if (has_theme) {
        xlab_param <- p2$theme$axis.title.x
        ylab_param <- p2$theme$axis.title.y
        
        p <- p + theme(axis.title.x = do.call(element_text, xlab_param),
                       axis.title.y = do.call(element_text, ylab_param))
    } else {
        p <- p + theme(axis.title.x = element_text(vjust = 1),
                       axis.title.y = element_text(angle = 90, vjust = 1)) 
    }
    return(p)
}

