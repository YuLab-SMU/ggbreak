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


combine_range <- function(breaks, rangeres, scales){
    if (rangeres$flagrev=="reverse"){
        rangeres$axis_range <- rev(-1 * (rangeres$axis_range))
    }
    res <- merge_intervals(breaks, scales)
    newbreaks <- res$breaks
    newscales <- res$scales
    newbreaks <- c(rangeres$axis_range[1], unlist(newbreaks), rangeres$axis_range[2])
    newbreaks <- lapply(data.frame(matrix(newbreaks, nrow=2)), function(i)i)
    if (rangeres$flagrev=="reverse"){
        newbreaks <- lapply(newbreaks, function(i)rev(i))
        return(list(breaks=rev(newbreaks), scales=rev(newscales)))
    }
    return(list(breaks=newbreaks, scales=newscales))
}

merge_intervals <- function(breaks, scales){
    if (!inherits(breaks, "list")){
        breaks <- list(breaks)
    }
    newbreaks <- list()
    newscales <- list()
    breaks <- lapply(breaks, function(i) sort(i))
    ind <- order(unlist(lapply(breaks, function(i)i[1])))
    scales <- scales[ind]
    breaks <- breaks[ind]
    
    for (i in seq_len(length(breaks))){
        if (length(newbreaks) >= 1 && breaks[[i]][1] <= newbreaks[[length(newbreaks)]][2]){
            newbreaks[[length(newbreaks)]][2] <- max(newbreaks[[length(newbreaks)]][2], breaks[[i]][2])
            mergescales <- c(scales[[i]], newscales[[length(newscales)]])
            if (any("fixed" %in% mergescales)){
                newscales[[length(newscales)]] <- "fixed"
            }
            if ((!"fixed" %in% mergescales) && any("free" %in% mergescales)){
                newscales[[length(newscales)]] <- "free"
            }
            if (is.numeric(mergescales)){
                newscales[[length(newscales)]] <- max(mergescales)
            }
        }else{
            newbreaks <- c(newbreaks, list(breaks[[i]]))
            newscales <- c(newscales, list(scales[[i]]))
        }
    }
    return(list(breaks=newbreaks, scales=unlist(newscales)))
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

compute_ggcut_breaks_relrange <- function(ggcut_params, rngrev){
    if (rngrev$flagrev == "reverse"){
        rngrev$axis_range <- rev(-1 * (rngrev$axis_range))
    }
    breaks <- ggcut_params$breaks
    if (any(breaks < rngrev$axis_range[1]) || any(breaks > rngrev$axis_range[2])){
        abort("Some breaks are not in the plot range. Please check all breaks!")
    }
    if (length(breaks) > 1){
        breaks <- c(rngrev$axis_range[1], sort(breaks), rngrev$axis_range[2])
        breaks <- lapply(seq_len(length(breaks)-1), function(i) c(breaks[i], breaks[i+1]))
    }else{
        breaks <- list(c(rngrev$axis_range[1], breaks), 
                       c(breaks, rngrev$axis_range[2]))
    }
    relrange <- rep(1, length(breaks))
    if (!is.null(ggcut_params$which) && !is.null(ggcut_params$scales)){
        relrange[ggcut_params$which] <- ggcut_params$scales
    }
    if (rngrev$flagrev == "reverse"){
        breaks <- rev(lapply(breaks, function(i) rev(i)))
        #relrange <- rev(relrange)
    }
    return(list(breaks=breaks, relrange=relrange))
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

list.add <- function(obj, ...){
    if (inherits(obj, "ggbreak_params")){
        c(list(obj), list(...))
    }else{
        c(obj, list(...))
    }
}
