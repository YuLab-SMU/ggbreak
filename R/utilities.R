## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    axis_range <- ggplot_build(plot)$layout[[var]][[1]]$range$range
    flagrev <- ggplot_build(plot)$layout[[var]][[1]]$trans$name
    list(axis_range=axis_range, flagrev=flagrev)
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

combine_range <- function(breaks, rangeres){
    if (rangeres$flagrev=="reverse"){
        rangeres$axis_range <- sort(abs(rangeres$axis_range))
    }
    newbreaks <- merge_intervals(breaks)
    newbreaks <- c(rangeres$axis_range[1], unlist(newbreaks), rangeres$axis_range[2])
    newbreaks <- lapply(data.frame(matrix(newbreaks, nrow=2)), function(i)i)
    if (rangeres$flagrev=="reverse"){
        newbreaks <- lapply(newbreaks, function(i)rev(i))
        return(rev(newbreaks))
    }
    return(newbreaks)
}

merge_intervals <- function(breaks){
    if (!inherits(breaks, "list")){
        breaks <- list(breaks)
    }
    out <- list()
    breaks <- lapply(breaks, function(i) sort(i))
    breaks <- breaks[rank(unlist(lapply(breaks, function(i)i[[1]])))]
    for (i in breaks){
        if (length(out) > 1 && i[1] <= out[[length(out)]][2]){
            out[[length(out)]][2] <- max(out[[length(out)]][2], i[2])
        }else{
            out <- c(out, list(i))
        }
    }
    return(out)
}

extract_axis_break <- function(object){
    if (inherits(object, "ggbreak_params")){
        axis <- object$axis
        breaks <- object$breaks
    }else{
        axis <- object[[1]]$axis
        breaks <- lapply(object, function(i)i$breaks)
    }
    return(list(axis=axis, breaks=breaks))
}

