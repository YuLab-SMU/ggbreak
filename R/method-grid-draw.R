##' @importFrom grid grid.draw
##' @method grid.draw ggbreak
##' @importFrom ggplotify as.ggplot
##' @importFrom ggplot2 coord_cartesian
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom aplot plot_list
##' @importFrom stats setNames

## Dual-axis break rendering: creates an Nx * Ny 2D grid of subplots
render_dual_break <- function(x, axis_break_x, axis_break_y) {
    # Process x-axis breaks
    xb <- extract_axis_break(object = axis_break_x)
    rng_x <- ggrange2(plot = x, var = "x")
    res_x <- combine_range(xb$breaks, rng_x, xb$scales, xb$ticklabs)
    x_breaks <- res_x$breaks
    x_relrange <- compute_relative_range(breaks = x_breaks, scales = res_x$scales, rng = rng_x)
    if (!rng_x$flagrev %in% c("identity", "reverse")) {
        x_breaks <- lapply(x_breaks, function(i) rng_x$inversefun(i))
    }

    # Process y-axis breaks
    yb <- extract_axis_break(object = axis_break_y)
    rng_y <- ggrange2(plot = x, var = "y")
    res_y <- combine_range(yb$breaks, rng_y, yb$scales, yb$ticklabs)
    y_breaks <- res_y$breaks
    y_relrange <- compute_relative_range(breaks = y_breaks, scales = res_y$scales, rng = rng_y)
    if (!rng_y$flagrev %in% c("identity", "reverse")) {
        y_breaks <- lapply(y_breaks, function(i) rng_y$inversefun(i))
    }

    # Reverse y for top-to-bottom layout (high values at top)
    y_breaks <- rev(y_breaks)
    y_relrange <- rev(y_relrange)

    nx <- length(x_breaks)
    ny <- length(y_breaks)
    margin_x <- xb$space
    margin_y <- yb$space

    # Handle labels
    totallabs <- extract_totallabs(plot = x)
    if (length(totallabs) > 0) {
        x <- .remove_axis_lab(x, totallabs)
    }

    # Handle expand for both axes
    expand_x <- convert_expand(xb$expand)
    expand_y <- convert_expand(yb$expand)
    scaleind_x <- find_scale_index(x, "x")
    scaleind_y <- find_scale_index(x, "y")
    if (!is.null(scaleind_x)) {
        x$scales$scales[[scaleind_x]]$expand <- expand_x
    } else {
        x <- suppressMessages(x + scale_x_continuous(expand = expand_x))
    }
    if (!is.null(scaleind_y)) {
        x$scales$scales[[scaleind_y]]$expand <- expand_y
    } else {
        x <- suppressMessages(x + scale_y_continuous(expand = expand_y))
    }

    legendpos <- check_legend_position(plot = x)

    # Build 2D grid: row-major order (top-to-bottom, left-to-right)
    plots <- vector("list", nx * ny)
    idx <- 0
    for (row_i in seq_len(ny)) {
        row_type <- if (ny == 1) "first"
                    else if (row_i == 1) "last"      # top row
                    else if (row_i == ny) "first"    # bottom row
                    else "other"

        for (col_j in seq_len(nx)) {
            col_type <- if (nx == 1) "first"
                        else if (col_j == 1) "first"     # left column
                        else if (col_j == nx) "last"     # right column
                        else "other"

            idx <- idx + 1
            te <- subplot_theme_2d(
                plot = x,
                col_type = col_type, row_type = row_type,
                margin_x = margin_x, margin_y = margin_y,
                rev_x = rng_x$flagrev, rev_y = rng_y$flagrev
            )

            plots[[idx]] <- suppressMessages(
                x + coord_cartesian(
                    xlim = c(x_breaks[[col_j]][1], x_breaks[[col_j]][2]),
                    ylim = c(y_breaks[[row_i]][1], y_breaks[[row_i]][2])
                ) + te
            )
        }
    }

    g <- plot_list(
        gglist = setNames(plots, NULL),
        ncol = nx, nrow = ny,
        widths = x_relrange,
        heights = y_relrange,
        guides = "collect",
        output = "patchwork"
    ) & legendpos

    newxlab <- totallabs$x
    newylab <- totallabs$y
    totallabs$x <- NULL
    totallabs$y <- NULL

    g <- ggplotify::as.ggplot(g) + xlab(newxlab) + ylab(newylab)
    g <- set_label(g, totallabs = totallabs, p2 = x)

    return(g)
}

##' @export
grid.draw.ggbreak <- function(x, recording = TRUE) {
    class(x) <- class(x)[class(x) != "ggbreak"]
    x <- check_xy_intercept(plot=x)

    axis_break_x <- attr(x, 'axis_break_x')
    axis_break_y <- attr(x, 'axis_break_y')
    has_x <- !is.null(axis_break_x)
    has_y <- !is.null(axis_break_y)

    if (has_x && has_y) {
        g <- render_dual_break(x, axis_break_x, axis_break_y)
        if (recording) {
            print(g)
        }
        return(invisible(g))
    }

    if (has_x) {
        axis_break <- axis_break_x
    } else {
        axis_break <- axis_break_y
    }

    axis_breaks <- extract_axis_break(object=axis_break)
    axis <- axis_breaks$axis
    margin <- axis_breaks$space
    breaks <- axis_breaks$breaks
    expand <- axis_breaks$expand
    scales <- axis_breaks$scales
    ticklabs <- axis_breaks$ticklabs
    rng <- ggrange2(plot=x, var=axis)
    res <- combine_range(breaks, rng, scales, ticklabs)
    breaks <- res$breaks
    scales <- res$scales
    ticklabs <- res$ticklabs

    totallabs <- extract_totallabs(plot=x)
    if (length(totallabs) > 0){
        #x$labels[names(totallabs)] <- NULL
        x <- .remove_axis_lab(x, totallabs)
    }
    nbreaks <- length(breaks)
    subplottheme1 <- subplot_theme(plot=x, axis=axis, type="first", margin = margin, rev = rng$flagrev)
    subplottheme2 <- subplot_theme(plot=x, axis=axis, type="other", margin = margin, rev = rng$flagrev)
    subplottheme3 <- subplot_theme(plot=x, axis=axis, type="last", margin = margin, rev = rng$flagrev)
    coord_fun <- check_coord_flip(plot=x) 
    relrange <- compute_relative_range(breaks=breaks, scales=scales, rng=rng)
    legendpos <- check_legend_position(plot=x)
    if (!rng$flagrev %in% c("identity","reverse")){
        breaks <- lapply(breaks, function(i)rng$inversefun(i))
    }
    scaleind <- find_scale_index(x, axis)
    expand <- convert_expand(expand=expand)
    another.axis = FALSE
    if (!is.null(scaleind)){
        x$scales$scales[[scaleind]]$expand <- expand
        if (!inherits(x$scales$scales[[scaleind]]$name, "waiver")){
            axis.title <- x$scales$scales[[scaleind]]$name
            x <- remove_axis_title(x, axis, coord_fun)
        }else{
            axis.title <- NULL
        }
        if (!inherits(x$scales$scales[[scaleind]]$secondary.axis$name, "waiver")){
            axis.sec.title <- x$scales$scales[[scaleind]]$secondary.axis$name
            x <- remove_axis_title(x, axis, coord_fun, second = TRUE)
        }else{
            axis.sec.title <- NULL
        }
    }else{
        scale_axis <- switch(axis, x=scale_x_continuous, y=scale_y_continuous)
        x <- suppressMessages(x + do.call(scale_axis, list(expand=expand)))
        another_axis <- setdiff(c('x', 'y'), axis)
        another_scaleind <- find_scale_index(x, another_axis)
        if (!is.null(another_scaleind) && !inherits(x$scales$scales[[another_scaleind]]$name, "waiver")){
            axis.title <- x$scales$scales[[another_scaleind]]$name
            x <- remove_axis_title(x, another_axis, coord_fun)
            another.axis = TRUE
        }else{
            axis.title <- NULL
        }
        if (!is.null(another_scaleind) && !inherits(x$scales$scales[[another_scaleind]]$secondary.axis$name, "waiver")){
            axis.sec.title <- x$scales$scales[[another_scaleind]]$secondary.axis$name
            x <- remove_axis_title(x, another_axis, coord_fun, second = TRUE)
            another.axis = TRUE
        }else{
            axis.sec.title <- NULL
        }
    }
    newxlab <- switch(coord_fun, coord_flip=totallabs$y, coord_cartesian=totallabs$x)
    newylab <- switch(coord_fun, coord_flip=totallabs$x, coord_cartesian=totallabs$y)
    if(axis == 'x') {
        p1 <- suppressMessages(x + do.call(coord_fun, list(xlim = c(breaks[[1]][1], breaks[[1]][2]))) + subplottheme1)

        pp1 <- suppressMessages(lapply(breaks[-c(1, nbreaks)], function(i) 
                            x + do.call(coord_fun, list(xlim=c(i[1], i[2]))) + 
                            subplottheme2))
        
        pp2 <- suppressMessages(x + do.call(coord_fun, list(xlim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) +
               subplottheme3)

        if (length(ticklabs) > 1){
            newticklabs <- ticklabs[-length(ticklabs)]
            for (i in seq_len(length(newticklabs))){
                if (!is.null(scaleind) && !is.null(newticklabs[[i]])){
                    pp1[[i]]$scales$scales[[scaleind]]$breaks <- newticklabs[[i]]
                    pp1[[i]]$scales$scales[[scaleind]]$labels <- newticklabs[[i]]
                }
                if (is.null(scaleind) && !is.null(newticklabs[[i]])){
                    pp1[[i]] <- suppressMessages(pp1[[i]] + scale_x_continuous(breaks=newticklabs[[i]], labels=newticklabs[[i]], expand=expand))
                }
            }
        }

        if (!is.null(scaleind) && !is.null(ticklabs[[length(ticklabs)]]) && rng$flagrev != "reverse"){
            pp2$scales$scales[[scaleind]]$breaks <- ticklabs[[length(ticklabs)]]
            pp2$scales$scales[[scaleind]]$labels <- ticklabs[[length(ticklabs)]]
        }
        if (is.null(scaleind) && !is.null(ticklabs[[length(ticklabs)]]) && rng$flagrev != "reverse"){
            pp2 <- suppressMessages(pp2 + scale_x_continuous(breaks=ticklabs[[length(ticklabs)]], labels=ticklabs[[length(ticklabs)]], expand = expand))
        }
        if (!is.null(scaleind) && !is.null(ticklabs[[length(ticklabs)]]) && rng$flagrev == "reverse"){
            p1$scales$scales[[scaleind]]$breaks <- ticklabs[[length(ticklabs)]]
            p1$scales$scales[[scaleind]]$labels <- ticklabs[[length(ticklabs)]]
        }
        if (is.null(scaleind) && !is.null(ticklabs[[length(ticklabs)]]) && rng$flagrev == "reverse"){
            p1 <- suppressMessages(p1 + scale_x_continuous(breaks = ticklabs[[length(ticklabs)]], labels=ticklabs[[length(ticklabs)]], expand=expand))
            #p1 <- add_expand_new_axis_scale(plot = p1, expand = expand, axis = "x",
            #                                breaks = ticklabs[[length(ticklabs)]], labels=ticklabs[[length(ticklabs)]]
            #      )
        }
        
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=setNames(c(list(pp2), rev(pp1), list(p1)), NULL),
                                           ncol=1,
                                           heights=c(rev(relrange[-1]), relrange[1]),
                                           guides = 'collect', output = "patchwork") & legendpos,
                    coord_cartesian = plot_list(gglist=setNames(c(list(p1), pp1, list(pp2)), NULL), 
                                                nrow=1, 
                                                widths=relrange,
                                                guides = 'collect', output = "patchwork") & legendpos
                    )
    } else {
        breaks <- rev(breaks)
        ticklabs <- rev(ticklabs)

        p1 <- suppressMessages(x + do.call(coord_fun, list(ylim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) + subplottheme1)

        pp1 <- suppressMessages(lapply(breaks[-c(1, nbreaks)], function(i) 
                      x + do.call(coord_fun, list(ylim=c(i[1], i[2]))) +
                            subplottheme2))

        pp2 <- suppressMessages(x + do.call(coord_fun, list(ylim = c(breaks[[1]][1], breaks[[1]][2]))) +
               subplottheme3)
        
        if (length(ticklabs) > 1){
            newticklabs <- ticklabs[-1]
            for (i in seq_len(length(newticklabs))){
                if (!is.null(scaleind) && !is.null(newticklabs[[i]])){
                    pp1[[i]]$scales$scales[[scaleind]]$breaks <- newticklabs[[i]]
                    pp1[[i]]$scales$scales[[scaleind]]$labels <- newticklabs[[i]]
                }
                if (is.null(scaleind) && !is.null(newticklabs[[i]])){
                    pp1[[i]] <- suppressMessages(pp1[[i]] + scale_y_continuous(breaks=newticklabs[[i]], labels=newticklabs[[i]], expand = expand))
                    #pp1[[i]] <- add_expand_new_axis_scale(plot = pp1[[i]], expand = expand, axis= "y",
                    #                                      breaks=newticklabs[[i]], labels=newticklabs[[i]]
                    #            )
                }
            }
        }

        if (!is.null(scaleind) && !is.null(ticklabs[[1]]) && rng$flagrev != "reverse"){
            pp2$scales$scales[[scaleind]]$breaks <- ticklabs[[1]]
            pp2$scales$scales[[scaleind]]$labels <- ticklabs[[1]]
        }
        if (is.null(scaleind) && !is.null(ticklabs[[1]]) && rng$flagrev != "reverse"){
            pp2 <- suppressMessages(pp2 + scale_y_continuous(breaks=ticklabs[[1]], labels=ticklabs[[1]], expand = expand))
            #pp2 <- add_expand_new_axis_scale(plot = pp2, expand = expand, axis = "y",
            #                                 breaks=ticklabs[[1]], labels=ticklabs[[1]])
        }
        if (!is.null(scaleind) && !is.null(ticklabs[[1]]) && rng$flagrev == "reverse"){
            p1$scales$scales[[scaleind]]$breaks <- ticklabs[[1]]
            p1$scales$scales[[scaleind]]$labels <- ticklabs[[1]]
        }
        if (is.null(scaleind) && !is.null(ticklabs[[1]]) && rng$flagrev == "reverse"){
            p1 <- suppressMessages(p1 + scale_y_continuous(breaks=ticklabs[[1]], labels=ticklabs[[1]], expand = expand))
            #p1 <- add_expand_new_axis_scale(plot = p1, expand = expand, axis = "y",
            #                                breaks=ticklabs[[1]], labels=ticklabs[[1]]
            #      )
        }
        
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=setNames(c(list(p1), rev(pp1), list(pp2)), NULL), 
                                           nrow=1, 
                                           widths=relrange,
                                           guides = 'collect', output = "patchwork") & legendpos,
                    coord_cartesian = plot_list(gglist=setNames(c(list(pp2), pp1, list(p1)), NULL), 
                                                ncol=1, 
                                                heights=c(rev(relrange[-1]), relrange[1]),
                                                guides = 'collect', output = "patchwork") & legendpos
               )
    }

    totallabs$x <- NULL
    totallabs$y <- NULL
    g <- ggplotify::as.ggplot(g) + xlab(newxlab) + ylab(newylab)
    
    g <- check_axis_title(
            plot = g, 
            axis = axis, 
            coord_fun = coord_fun,
            axis.title = axis.title, 
            axis.sec.title = axis.sec.title,
            another.axis = another.axis
         )

    g <- set_label(g, totallabs = totallabs, p2 = x)
    
    if (recording){
        print(g)
    }
    invisible(g)
}

##' @method grid.draw ggwrap
##' @export
grid.draw.ggwrap <- function(x, recording=TRUE){
    class(x) <- class(x)[class(x) != "ggwrap"]
    x <- check_xy_intercept(plot=x)
    axis_wrap <- attr(x, "axis_wrap")
    totallabs <- extract_totallabs(plot=x)
    if (length(totallabs) > 0){
        #x$labels[names(totallabs)] <- NULL
        x <- .remove_axis_lab(x, totallabs)
    }
    nstep <- axis_wrap$n
    expand <- axis_wrap$expand
    rngrev <- ggrange2(plot=x, 'x')
    rng <- rngrev$axis_range
    if (!is.null(rngrev$flagrev) && rngrev$flagrev == "reverse"){
        rng <- rev(-1 * (rng))
    }
    
    if (!is.null(rngrev$flagrev)) {
        breaks <- seq(rng[1], rng[2], length.out=nstep + 1)
        if (!rngrev$flagrev %in% c("identity", "reverse")){
            breaks <- rngrev$inversefun(breaks)
        }
        x <- add_expand(plot = x, expand = expand, axis = "x")
        gg <- lapply(seq_len(length(breaks)-1), function(i) x + coord_cartesian(xlim=c(breaks[i], breaks[i+1])))
    }else{
        limits <- split_discrete_range(x = rng, n = nstep) 
        gg <- lapply(limits, split_discrete_scale, plot=x, axis='x')
    }
    legendpos <- check_legend_position(plot=x)
    pg <- plot_list(gglist=setNames(gg, NULL), ncol=1, guides="collect", output = "patchwork") & legendpos
    g <- set_label(as.ggplot(pg), totallabs=totallabs, p2=x)
    if (recording){
        print(g)
    }

    invisible(g)

}

#' @method grid.draw ggcut
#' @export
grid.draw.ggcut <- function(x, recording=TRUE){
    class(x) <- class(x)[class(x) != "ggcut"]
    x <- check_xy_intercept(plot=x)
    axis_cut <- attr(x, "axis_cut")
    axis <- axis_cut$axis
    margin <- axis_cut$space
    expand <- axis_cut$expand
    totallabs <- extract_totallabs(plot=x)
    if (length(totallabs) > 0){
        #x$labels[names(totallabs)] <- NULL
        x <- .remove_axis_lab(x, totallabs)
    }
    rngrev <- ggrange2(plot=x, var = axis)
    breaks_relrange <- compute_ggcut_breaks_relrange(ggcut_params=axis_cut, rngrev=rngrev)
    breaks <- breaks_relrange$breaks
    relrange <- breaks_relrange$relrange

    nbreaks <- length(breaks)
    subplottheme1 <- subplot_theme(plot=x, axis=axis, type="first", margin = margin, rev = rngrev$flagrev)
    subplottheme2 <- subplot_theme(plot=x, axis=axis, type="other", margin = margin, rev = rngrev$flagrev)
    subplottheme3 <- subplot_theme(plot=x, axis=axis, type="last", margin = margin, rev = rngrev$flagrev)
    coord_fun <- check_coord_flip(plot=x)
    newxlab <- switch(coord_fun, coord_flip=totallabs$y, coord_cartesian=totallabs$x)
    newylab <- switch(coord_fun, coord_flip=totallabs$x, coord_cartesian=totallabs$y)
    legendpos <- check_legend_position(plot=x)
    if (!rngrev$flagrev %in% c("identity", "reverse")){
        breaks <- rngrev$inversefun(breaks)
    }
    #expand <- getOption(x="scale_xy_expand", default = FALSE)
    x <- add_expand(plot = x, expand = expand, axis = axis)
    if(axis == 'x') {
        p1 <- suppressMessages(x + do.call(coord_fun, list(xlim = c(breaks[[1]][1], breaks[[1]][2]))) + subplottheme1)
        pp1 <- suppressMessages(lapply(breaks[-c(1, nbreaks)], function(i)
                            x + do.call(coord_fun, list(xlim=c(i[1], i[2]))) +
                            subplottheme2))
        pp2 <- suppressMessages(x + do.call(coord_fun, list(xlim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) +
               subplottheme3)
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=setNames(c(list(pp2), rev(pp1), list(p1)), NULL),
                                           ncol=1,
                                           heights=relrange,#c(rev(relrange[-1]), relrange[1]),
                                           guides = 'collect', output = "patchwork") & legendpos, 
                    coord_cartesian = plot_list(gglist=setNames(c(list(p1), pp1, list(pp2)), NULL),
                                                nrow=1,
                                                widths=relrange,
                                                guides = 'collect', output = "patchwork") & legendpos
                    )
    } else {
        breaks <- rev(breaks)
        p1 <- suppressMessages(x + do.call(coord_fun, list(ylim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) + subplottheme1)
        pp1 <- suppressMessages(lapply(breaks[-c(1, nbreaks)], function(i)
                      x + do.call(coord_fun, list(ylim=c(i[1], i[2]))) +
                            subplottheme2))
        pp2 <- suppressMessages(x + do.call(coord_fun, list(ylim = c(breaks[[1]][1], breaks[[1]][2]))) +
               subplottheme3)
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=setNames(c(list(p1), rev(pp1), list(pp2)), NULL),
                                           nrow=1,
                                           widths=relrange,
                                           guides = 'collect', output = "patchwork") & legendpos,
                    coord_cartesian = plot_list(gglist=setNames(c(list(pp2), pp1, list(p1)), NULL),
                                                ncol=1,
                                                heights=relrange,#c(rev(relrange[-1]), relrange[1]),
                                                guides = 'collect', output = "patchwork") & legendpos
               )
    }
    totallabs$x <- NULL
    totallabs$y <- NULL
    g <- ggplotify::as.ggplot(g) + xlab(newxlab) + ylab(newylab)
    g <- set_label(g, totallabs = totallabs, p2 = x)
    if (recording){
        print(g)
    }
    
    invisible(g)

}
