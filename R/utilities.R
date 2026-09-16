## function taken from ggtree
#' @importFrom ggplot2 ggplot_build
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
    ## only datetime scales have a timezone, it is `NULL` otherwise
    tz <- gb$layout[[var]][[1]]$timezone
    list(axis_range=axis_range, flagrev=flagrev, transfun=transfun, inversefun=inversefun, tz=tz)
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
    alllabs <- ggplot_build(plot)$plot$labels
    totallabs <- alllabs[names(alllabs) %in% c("x", "y", "title", "subtitle", "caption", "tag")]
    totallabs
}


## Coerce the break points of a date or datetime axis to the class that the
## scale transform expects.  `transform_time()` accepts `POSIXct` objects only,
## while the range of a datetime scale is stored as numeric seconds since the
## epoch, so numeric, `Date` and character break points all failed with
## "`transform_time()` works with objects of class <POSIXct> only" (#84).
convert_axis_breaks <- function(breaks, rangeres){
    if (identical(rangeres$flagrev, "date")){
        return(as.Date(breaks))
    }
    if (identical(rangeres$flagrev, "time")){
        return(as_datetime_break(breaks, tz = rangeres$tz))
    }
    breaks
}


as_datetime_break <- function(x, tz = NULL){
    if (inherits(x, "POSIXct")){
        return(x)
    }
    if (is.null(tz)){
        tz <- "UTC"
    }
    ## `Date` and character break points refer to a wall clock time, so they
    ## have to be interpreted in the timezone of the scale
    if (inherits(x, "Date") || is.character(x)){
        return(as.POSIXct(as.character(x), tz = tz))
    }
    ## numeric break points are seconds since the epoch, mirroring how
    ## `as.Date()` reads numeric break points as days since the epoch
    as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = tz)
}


combine_range <- function(breaks, rangeres, scales, ticklabs){
    if (rangeres$flagrev=="reverse"){
        rangeres$axis_range <- rev(-1 * (rangeres$axis_range))
    }
    if (is.list(breaks)){
        breaks <- lapply(breaks, convert_axis_breaks, rangeres = rangeres)
    }else{
        breaks <- convert_axis_breaks(breaks, rangeres)
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
    check_break_range(newbreaks)
    if (rangeres$flagrev=="reverse"){
        newbreaks <- lapply(newbreaks, function(i)rev(i))
        return(list(breaks=rev(newbreaks), scales=rev(newscales), 
                    ticklabs=c(rev(newticklabs[-length(newticklabs)]), newticklabs[length(newticklabs)])))
    }
    return(list(breaks=newbreaks, scales=newscales, ticklabs=newticklabs))
}


## The subplot ranges are built by splicing the break points into the axis
## range.  This assumes that every break point lies inside the axis range;
## a break interval that is (partly) outside of it yields ranges that are
## not ordered the same way as the axis, e.g. `scale_y_break(c(10, 20))` for
## a plot whose `y` spans 4 to 6 gives the ranges (4, 10) and (20, 6), and the
## second one silently flips the axis of that subplot, see #43.  `scale_x_cut()`
## already validates its breaks, so use the same message here.
check_break_range <- function(breaks){
    bad <- vapply(breaks, function(i){
        length(i) == 2 && i[1] > i[2]
    }, logical(1))
    if (any(bad)){
        abort("Some breaks are not in the plot range. Please check all breaks!")
    }
    invisible(NULL)
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
        symbol <- object$symbol
    }else{
        axis <- object[[1]]$axis
        space <- object[[1]]$space
        expand <- object[[length(object)]]$expand
        breaks <- lapply(object, function(i)i$breaks)
        scales <- lapply(object, function(i)i$scales)
        ticklabs <- lapply(object, function(i)i$ticklabels)
        symbol <- object[[1]]$symbol
    }
    return(list(
                axis = axis,
                space = space,
                breaks = breaks,
                expand = expand,
                scales = scales,
                ticklabs = ticklabs,
                symbol = symbol
            ))
}

compute_ggcut_breaks_relrange <- function(ggcut_params, rngrev){
    if (rngrev$flagrev == "reverse"){
        rngrev$axis_range <- rev(-1 * (rngrev$axis_range))
    }
    breaks <- convert_axis_breaks(ggcut_params$breaks, rngrev)
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


## check whether a scale defines a real secondary axis.
## Note that `scale$secondary.axis` is a `waiver` for continuous scales without
## a secondary axis, but is `NULL` for `ScaleDiscretePosition`.  Testing
## `scale$secondary.axis$name` (as was done before) never matches a `waiver`,
## which made ggbreak add a spurious secondary axis, see #64 and #83.
has_secondary_axis <- function(scale) {
    inherits(scale$secondary.axis, "AxisSecondary")
}


theme_no_margin <- getFromNamespace("theme_no_margin", "ggfun")
theme_fp <- getFromNamespace('theme_fp', 'ggfun')


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


.remove_axis_lab <- function(x, labels){
   params <- lapply(seq(length(labels)), function(i)NULL)
   names(params) <- names(labels)
   x + do.call('labs', params)
}

.drop_class <- function(x, class){
    old <- class(x)
    class(x) <- old[!old %in% class]
    return(x)
}
