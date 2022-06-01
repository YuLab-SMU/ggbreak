## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    gb <- ggplot_build(plot)
    limits <- gb$layout[[var]][[1]]$limits
    if (!is.null(limits)){
        axis_range <- limits
    }else{
        axis_range <- gb$layout[[var]][[1]]$range$range
    }
    flagrev <- gb$layout[[var]][[1]]$trans$name
    transfun <- gb$layout[[var]][[1]]$trans$transform
    inversefun <- gb$layout[[var]][[1]]$trans$inverse
    list(axis_range=axis_range, flagrev=flagrev, transfun=transfun, inversefun=inversefun)
}

check_legend_position <- function(plot){
    if (!is.null(plot$theme$legend.position)){
        tm <- theme(legend.position=plot$theme$legend.position)
    }else{
        tm <- theme()
    }
    return (tm)
}

#' @importFrom ggplot2 labs
set_label <- function(p, totallabs, p2 = NULL) {
    p <- p + 
         do.call(labs, totallabs) 

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
    labs_params <- c("text", "title", "axis.title", 
                     "axis.title.x", "axis.title.x.top", "axis.title.x.bottom",
                     "axis.title.y", "axis.title.y.left", "axis.title.y.right",
                     "plot.title", "plot.title.position", "plot.subtitle",
                     "plot.caption", "plot.caption.position", "plot.tag", "plot.tag.position")
    p <- p + 
         theme_fp(x=x, i=labs_params) +
         theme(axis.text = element_blank(),
               axis.ticks = element_blank()
               )
    return(p)
}

extract_totallabs <- function(plot){
    alllabs <- plot$labels
    totallabs <- alllabs[names(alllabs) %in% c("x", "y", "title", "subtitle", "caption", "tag")]
    totallabs
}


combine_range <- function(breaks, rangeres, scales, ticklabs){
    if (rangeres$flagrev=="reverse"){
        rangeres$axis_range <- rev(-1 * (rangeres$axis_range))
    }
    if (rangeres$flagrev=="date"){
        if (is.list(breaks)){
            breaks <- lapply(breaks, function(i) as.Date(i))
        }else{
            breaks <- as.Date(breaks)
        }
    }
    if (!rangeres$flagrev %in% c("identity", "reverse")){
        if (is.list(breaks)){
            breaks <- lapply(breaks, function(i) rangeres$transfun(i))
        }else{
            breaks <- rangeres$transfun(breaks)
        }
    }
    res <- merge_intervals(breaks, scales, ticklabs)
    newbreaks <- res$breaks
    newscales <- res$scales
    newticklabs <- res$ticklabs
    newbreaks <- c(rangeres$axis_range[1], unlist(newbreaks), rangeres$axis_range[2])
    newbreaks <- lapply(data.frame(matrix(newbreaks, nrow=2)), function(i)i)
    if (rangeres$flagrev=="reverse"){
        newbreaks <- lapply(newbreaks, function(i)rev(i))
        return(list(breaks=rev(newbreaks), scales=rev(newscales), 
                    ticklabs=c(rev(newticklabs[-length(newticklabs)]), newticklabs[length(newticklabs)])))
    }
    return(list(breaks=newbreaks, scales=newscales, ticklabs=newticklabs))
}

merge_intervals <- function(breaks, scales, ticklabs){
    if (!inherits(breaks, "list")){
        breaks <- list(breaks)
    }
    if (!inherits(ticklabs, "list")){
        ticklabs <- list(ticklabs)
    }
    newbreaks <- list()
    newscales <- list()
    newticklabs <- list()
    breaks <- lapply(breaks, function(i) sort(i))
    ind <- order(unlist(lapply(breaks, function(i)i[1])))
    scales <- scales[ind]
    breaks <- breaks[ind]
    ticklabs <- ticklabs[ind]
    for (i in seq_len(length(breaks))){
        if (length(newbreaks) >= 1 && breaks[[i]][1] <= newbreaks[[length(newbreaks)]][2]){
            newbreaks[[length(newbreaks)]][2] <- max(newbreaks[[length(newbreaks)]][2], breaks[[i]][2])
            mergescales <- c(scales[[i]], newscales[[length(newscales)]])
            mergeticks <- c(list(ticklabs[[i]], list(newticklabs[[length(newticklabs)]])))
            if (any("fixed" %in% mergescales)){
                newscales[[length(newscales)]] <- "fixed"
            }
            if ((!"fixed" %in% mergescales) && any("free" %in% mergescales)){
                newscales[[length(newscales)]] <- "free"
            }
            if (is.numeric(mergescales)){
                newscales[[length(newscales)]] <- max(mergescales)
            }
            newticklabs[[length(newticklabs)]] <- mergeticks[[which.max(unlist(lapply(mergeticks, function(i)length(i))))]]
        }else{
            newbreaks <- c(newbreaks, list(breaks[[i]]))
            newscales <- c(newscales, list(scales[[i]]))
            newticklabs <- c(newticklabs, list(ticklabs[[i]]))
        }
    }
    return(list(breaks=newbreaks, scales=unlist(newscales), ticklabs=newticklabs))
}

extract_axis_break <- function(object){
    if (inherits(object, "ggbreak_params")){
        axis <- object$axis
        breaks <- object$breaks
        expand <- object$expand
        scales <- object$scales
        ticklabs <- object$ticklabels
        space <- object$space
    }else{
        axis <- object[[1]]$axis
        space <- object[[1]]$space
        expand <- object[[length(object)]]$expand
        breaks <- lapply(object, function(i)i$breaks)
        scales <- lapply(object, function(i)i$scales) 
        ticklabs <- lapply(object, function(i)i$ticklabels)
    }
    return(list(
                axis = axis, 
                space = space,
                breaks = breaks, 
                expand = expand, 
                scales = scales, 
                ticklabs = ticklabs
               )
    )
}

compute_ggcut_breaks_relrange <- function(ggcut_params, rngrev){
    if (rngrev$flagrev == "reverse"){
        rngrev$axis_range <- rev(-1 * (rngrev$axis_range))
    }
    breaks <- ggcut_params$breaks
    if (rngrev$flagrev == "date"){
        breaks <- as.Date(breaks)
    }
    if (!rngrev$flagrev %in% c("identity", "reverse")){
        breaks <- rngrev$transfun(breaks)
    }
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
    params <- c(params, list(complete = TRUE))
    do.call(theme, params)
}

list.add <- function(obj, ...){
    if (inherits(obj, "ggbreak_params")){
        c(list(obj), list(...))
    }else{
        c(obj, list(...))
    }
}

remove_axis_title <- function(plot, axis, coord_fun, second = FALSE){
    axis <- switch(coord_fun,
                   coord_flip = setdiff(c("x", "y"), axis),
                   coord_cartesian = intersect(c("x", "y"), axis))    
    if (axis == "x"){
        if (second){
            plot <- plot + ggplot2::guides(x.sec = ggplot2::guide_axis(title = NULL))
        }else{
            plot <- plot + ggplot2::guides(x = ggplot2::guide_axis(title = NULL))
        }
    }else if (axis == "y"){
        if (second){
            plot <- plot + ggplot2::guides(y.sec = ggplot2::guide_axis(title = NULL))
        }else{
            plot <- plot + ggplot2::guides(y = ggplot2::guide_axis(title = NULL))
        }
    }
    return(plot)
}

check_axis_title <- function(plot, axis, coord_fun, axis.title, axis.sec.title, another.axis){
    if (another.axis){
        axis <- setdiff(c('x','y'), axis)
    }
        
    axis <- switch(coord_fun, 
                   coord_flip = setdiff(c("x", "y"), axis), 
                   coord_cartesian = axis)
    if (!is.null(axis.sec.title)){
        if (axis == "x"){
            plot <- plot + ggplot2::guides(x.sec = ggplot2::guide_axis(title=axis.sec.title))
        }
        if (axis == "y"){
            plot <- plot + ggplot2::guides(y.sec = ggplot2::guide_axis(title=axis.sec.title))
        }
    }
    if (!is.null(axis.title)){
        if (axis == "x"){
            plot <- plot + ggplot2::guides(x = ggplot2::guide_axis(title=axis.title))
        }
        if (axis == "y"){
            plot <- plot + ggplot2::guides(y = ggplot2::guide_axis(title=axis.title))
        }
    }
    return (plot)
}

find_scale_index <- function(plot, aesthetic){
    if (plot$scales$has_scale(aesthetic)){
        scaleind <- which(plot$scales$find(aesthetic))
    }else{
        scaleind <- NULL
    }
    return(scaleind)
}


split_discrete_range <- function(x, n){
    if (n >= 2){
        l <- max(table(cut(seq_along(x), n, label=FALSE)))
    }else{
        l <- length(x)
    }
    x <- split(x, ceiling(seq_along(x)/l))
    return(x)
}

split_discrete_scale <- function(limits, plot, axis='x'){
   var <- paste0("panel_scales_", axis)
   gb <- ggplot_build(plot)
   scales_axis_obj <- gb$layout[[var]][[1]]
   scales_axis_obj$limits <- limits
   plot <- plot + scales_axis_obj
   return(plot)
}

numeric2Date <- function(x) {
    as.Date(x, origin="1970-01-01")
}

