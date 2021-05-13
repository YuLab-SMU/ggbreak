## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    axis_range <- ggplot_build(plot)$layout[[var]][[1]]$range$range
    flagrev <- ggplot_build(plot)$layout[[var]][[1]]$trans$name
    list(axis_range=axis_range, flagrev=flagrev)
}

#' @importFrom ggplot2 labs
set_label <- function(p, totallabs, p2 = NULL) {
    p <- p + 
         do.call(labs, totallabs) +
         theme(axis.title = element_text())

    if (is.null(p2)) {
        has_theme <- FALSE
    } else {
        has_theme <- length(p2$theme) != 0
    }

    if (has_theme) {
        x <- p2
    } else {
        x <- NULL
    }
    labs_params <- c("axis.title.x","axis.title.y", "plot.title", "plot.title.position", "plot.subtitle",
                    "plot.caption", "plot.caption.position", "plot.tag", "plot.tag.position")
    p <- p + theme_fp(x=x, i=labs_params)
    return(p)
}

extract_totallabs <- function(plot){
    alllabs <- plot$labels
    totallabs <- alllabs[names(alllabs) %in% c("x", "y", "title", "subtitle", "caption", "tag")]
    totallabs
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
        scales <- object$scales
    }else{
        axis <- object[[1]]$axis
        breaks <- lapply(object, function(i)i$breaks)
        scales <- lapply(object, function(i)i$scales) 
    }
    return(list(axis=axis, breaks=breaks, scales=scales))
}

theme_no_margin <- getFromNamespace("theme_no_margin", "aplot")

#' @importFrom ggplot2 theme_get
get_theme_params = function(x, i) {
    if (!inherits(x, "theme")) x <- x$theme
    if (length(x) == 0) {
        x <- ggplot2::theme_get()
    }
    x[i]
}

theme_fp <- function(x, i) {
    params <- get_theme_params(x, i)
    do.call(theme, params)
}
