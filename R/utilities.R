## function taken from ggtree
#' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    gb <- ggplot_build(plot)
    scales <- gb$layout[[var]]
    limits <- scales[[1]]$limits
    if (!is.null(limits)){
        axis_range <- limits
    }else if (length(scales) > 1L){
        ## `facet_grid(scales = "free_y")` gives every panel a scale of its own,
        ## and the first of them holds the range of the first panel only.  Taking
        ## it for the whole axis clips the data of every other panel away: the
        ## subplots are given a window of that range, and a point outside it is
        ## dropped.  The axis is as wide as all the panels together.
        rng <- unlist(lapply(scales, function(s) s$range$range))
        axis_range <- if (length(rng)) range(rng) else scales[[1]]$range$range
    }else{
        axis_range <- scales[[1]]$range$range
    }
    flagrev <- scales[[1]]$trans$name
    transfun <- scales[[1]]$trans$transform
    inversefun <- scales[[1]]$trans$inverse
    ## only datetime scales have a timezone, it is `NULL` otherwise
    tz <- scales[[1]]$timezone
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
               axis.ticks = element_blank(),
               ## The outer ggplot is only the container of the assembled
               ## subplots: the panels, their axis lines, their borders and
               ## their backgrounds all belong to the subplots.  The elements
               ## below are still taken from the *global* default theme for this
               ## plot, though, so `theme_set(theme_bw())` draws a second border
               ## around the whole figure and `theme_set(theme_classic())` a
               ## second pair of axis lines, see #57.
               axis.line = element_blank(),
               axis.line.x = element_blank(),
               axis.line.y = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid = element_blank()
               )

    ## The background of the figure belongs to this outer ggplot, and it is the
    ## one the user asked for: forward `plot.background` the way the margin is
    ## forwarded below, so that `theme(plot.background = element_blank())` really
    ## makes a broken plot transparent, see #52.
    if (has_theme && !is.null(x$theme$plot.background)) {
        p <- p + theme(plot.background = x$theme$plot.background)
    }

    ## Bring the user's `theme(plot.margin = ...)` onto the outer ggplot that
    ## holds the annotation_custom, see #71. Without this the inner subplots
    ## carry the margin but it is invisible in the final draw.
    ## Skip when the margin matches `theme_gray()`'s default (5.5pt) so we do
    ## not shift the figure by ~7px on each side for users who never set it.
    if (has_theme &&
        !identical(x$theme$plot.margin, ggplot2::theme_gray()$plot.margin)) {
        p <- p + theme(plot.margin = x$theme$plot.margin)
    }

    return(p)
}

extract_totallabs <- function(plot){
    ## `ggplot_build(plot)$plot$labels` only holds what `labs()` set plus the
    ## aesthetic defaults, so the title of `scale_x_continuous("test")` still
    ## reads "x" here while the plot draws "test" -- a position scale resolves
    ## its `name` into the label later, in the layout.  `get_labs()` returns the
    ## completed labels, which is what `set_label()` has to hand to `labs()` for
    ## the assembled figure to carry the title the user asked for (#85).
    alllabs <- ggplot2::get_labs(plot)
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
        bridge <- object$bridge
    }else{
        axis <- object[[1]]$axis
        space <- object[[1]]$space
        expand <- object[[length(object)]]$expand
        breaks <- lapply(object, function(i)i$breaks)
        scales <- lapply(object, function(i)i$scales)
        ticklabs <- lapply(object, function(i)i$ticklabels)
        symbol <- object[[1]]$symbol
        bridge <- object[[1]]$bridge
    }
    return(list(
                axis = axis,
                space = space,
                breaks = breaks,
                expand = expand,
                scales = scales,
                ticklabs = ticklabs,
                symbol = symbol,
                bridge = isTRUE(bridge)
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
    ## The primary axis title is deliberately *not* re-added here.  It already
    ## reaches the outer ggplot through `xlab()`/`ylab()` (and, for a bottom
    ## legend, through `hoist_bottom_axis_title()`); drawing it again with
    ## `guides(x = guide_axis(title =))` lands it on the `xlab-b` row that sits
    ## *below* `guide-box`, so a plot whose title comes from a scale `name` was
    ## labelled twice -- once above the legend and once below it (#85).  Only the
    ## secondary-axis title is left here, because it cannot be expressed with
    ## `labs()` and #64 depends on it.
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

## Split the levels of a discrete axis into the panels of a break.
##
## The continuous code splices the break points into the axis range and reads the
## result as a two-row matrix, so that the panels are (start, first break),
## (second break, next break) ...  The same is done here on the *positions* of
## the levels, which also keeps the order of the levels instead of sorting them
## alphabetically the way `merge_intervals()` would.
##
## `breaks` are level names, so `c("a", "b")` with `a` and `b` adjacent only
## inserts a gap, while `c("b", "d")` additionally drops the levels between them.
discrete_break_index <- function(breaks, levels){
    if (!inherits(breaks, "list")){
        breaks <- list(breaks)
    }
    pos <- lapply(breaks, function(i){
        p <- match(as.character(i), as.character(levels))
        if (anyNA(p)){
            abort(paste0("Some breaks are not levels of the ", length(levels),
                         "-level axis: ",
                         paste(i[is.na(p)], collapse = ", "),
                         ". Please check all breaks!"))
        }
        p
    })

    if (any(vapply(pos, function(i) i[1] > i[2], logical(1)))){
        abort("Some breaks are not in ascending order. Please check all breaks!")
    }

    ## merge overlapping intervals the way `merge_intervals()` does, but on the
    ## positions of the levels
    pos <- pos[order(vapply(pos, function(i) i[1], numeric(1)))]
    merged <- list()
    for (i in pos){
        if (length(merged) > 0 && i[1] <= merged[[length(merged)]][2]){
            merged[[length(merged)]][2] <- max(merged[[length(merged)]][2], i[2])
        }else{
            merged[[length(merged) + 1]] <- i
        }
    }

    ## the panel limits come in pairs: (first level, first break), (second
    ## break, third break) ... (last break, last level)
    bounds <- unlist(merged)
    start  <- c(1, bounds[seq(2, length(bounds), by = 2)])
    end    <- c(bounds[seq(1, length(bounds), by = 2)], length(levels))

    ## a break at the very first or the very last level would give an empty panel
    if (any(start > end)){
        abort("Some breaks are not in the plot range. Please check all breaks!")
    }
    lapply(seq_along(start), function(i) seq(start[i], end[i]))
}


## Split the levels of a discrete axis at the cut points of `scale_*_cut()`.
## Unlike a break, a cut keeps everything, so neighbouring panels share the
## level they are cut at, exactly the way the continuous code does.
discrete_cut_index <- function(breaks, levels){
    pos <- match(as.character(breaks), as.character(levels))
    if (anyNA(pos)){
        abort(paste0("Some breaks are not levels of the ", length(levels),
                     "-level axis: ", paste(breaks[is.na(pos)], collapse = ", "),
                     ". Please check all breaks!"))
    }
    if (is.unsorted(pos)){
        abort("Some breaks are not in ascending order. Please check all breaks!")
    }
    bounds <- c(1, pos, length(levels))
    lapply(seq_len(length(bounds) - 1), function(i) seq(bounds[i], bounds[i + 1]))
}


## Relative width or height of the panels of a discrete cut.  `which` and
## `scales` mean the same as they do for a continuous one.
discrete_cut_relative_range <- function(idx, which, scales){
    rel <- vapply(idx, length, numeric(1))
    if (!is.null(which) && !is.null(scales)){
        rel[which] <- rel[which] * unlist(scales)[seq_along(which)]
    }
    rel
}


## Relative width or height of the panels of a discrete break.  A panel of a
## discrete axis is as wide as it has levels (plus the usual padding), so the
## number of levels plays the role of the range of a continuous panel.
discrete_relative_range <- function(idx, scales){
    rel <- vapply(idx, length, numeric(1))
    if ("free" %in% unlist(scales)){
        return(rep(1, length(rel)))
    }
    if (is.numeric(scales) && length(scales) == 1 && scales != 1){
        return(c(rel[1], rep(rel[1] * scales, length(rel) - 1)))
    }
    rel
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
