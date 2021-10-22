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
##' @export
grid.draw.ggbreak <- function(x, recording = TRUE) {
    class(x) <- class(x)[class(x) != "ggbreak"]
    x <- check_xy_intercept(plot=x)
    axis_break <- attr(x, 'axis_break')
    axis_breaks <- extract_axis_break(object=axis_break)
    axis <- axis_breaks$axis
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
        x$labels[names(totallabs)] <- NULL
    }
    nbreaks <- length(breaks)
    subplottheme1 <- subplot_theme(plot=x, axis=axis, type="first")
    subplottheme2 <- subplot_theme(plot=x, axis=axis, type="other")
    subplottheme3 <- subplot_theme(plot=x, axis=axis, type="last")
    coord_fun <- check_coord_flip(plot=x) 
    newxlab <- switch(coord_fun, coord_flip=totallabs$y, coord_cartesian=totallabs$x)
    newylab <- switch(coord_fun, coord_flip=totallabs$x, coord_cartesian=totallabs$y)
    relrange <- compute_relative_range(breaks=breaks, scales=scales, rng=rng)
    legendpos <- check_legend_position(plot=x)
    if (!rng$flagrev %in% c("identity","reverse")){
        breaks <- lapply(breaks, function(i)rng$inversefun(i))
    }
    if (x$scales$has_scale(axis)){
        scaleind <- which(x$scales$find(axis))
    }else{
        scaleind <- NULL
    }
    #expand <- getOption(x="scale_xy_expand", default = FALSE)
    expand <- convert_expand(expand=expand)
    if (!is.null(scaleind)){
        x$scales$scales[[scaleind]]$expand <- expand
    }else{
        scale_axis <- switch(axis, x=scale_x_continuous, y=scale_y_continuous)
        x <- suppressMessages(x + do.call(scale_axis, list(expand=expand)))
    }
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
                                           guides = 'collect') & legendpos,
                    coord_cartesian = plot_list(gglist=setNames(c(list(p1), pp1, list(pp2)), NULL), 
                                                nrow=1, 
                                                widths=relrange,
                                                guides = 'collect') & legendpos
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
                                           guides = 'collect') & legendpos,
                    coord_cartesian = plot_list(gglist=setNames(c(list(pp2), pp1, list(p1)), NULL), 
                                                ncol=1, 
                                                heights=c(rev(relrange[-1]), relrange[1]),
                                                guides = 'collect') & legendpos
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

##' @method grid.draw ggwrap
##' @export
grid.draw.ggwrap <- function(x, recording=TRUE){
    class(x) <- class(x)[class(x) != "ggwrap"]
    x <- check_xy_intercept(plot=x)
    axis_wrap <- attr(x, "axis_wrap")
    totallabs <- extract_totallabs(plot=x)
    if (length(totallabs) > 0){
        x$labels[names(totallabs)] <- NULL
    }
    nstep <- axis_wrap$n
    expand <- axis_wrap$expand
    rngrev <- ggrange2(plot=x, 'x')
    rng <- rngrev$axis_range
    if (rngrev$flagrev == "reverse"){
        rng <- rev(-1 * (rng))
    }
    breaks <- seq(rng[1], rng[2], length.out=nstep + 1)
    if (!rngrev$flagrev %in% c("identity", "reverse")){
        breaks <- rngrev$inversefun(breaks)
    }
    x <- add_expand(plot = x, expand = expand, axis = "x")
    legendpos <- check_legend_position(plot=x)
    gg <- lapply(seq_len(length(breaks)-1), function(i) x + coord_cartesian(xlim=c(breaks[i], breaks[i+1])))
    pg <- plot_list(gglist=setNames(gg, NULL), ncol=1, guides="collect") & legendpos
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
    expand <- axis_cut$expand
    totallabs <- extract_totallabs(plot=x)
    if (length(totallabs) > 0){
        x$labels[names(totallabs)] <- NULL
    }
    rngrev <- ggrange2(plot=x, var = axis)
    breaks_relrange <- compute_ggcut_breaks_relrange(ggcut_params=axis_cut, rngrev=rngrev)
    breaks <- breaks_relrange$breaks
    relrange <- breaks_relrange$relrange

    nbreaks <- length(breaks)
    subplottheme1 <- subplot_theme(plot=x, axis=axis, type="first")
    subplottheme2 <- subplot_theme(plot=x, axis=axis, type="other")
    subplottheme3 <- subplot_theme(plot=x, axis=axis, type="last")
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
                                           guides = 'collect') & legendpos, 
                    coord_cartesian = plot_list(gglist=setNames(c(list(p1), pp1, list(pp2)), NULL),
                                                nrow=1,
                                                widths=relrange,
                                                guides = 'collect') & legendpos
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
                                           guides = 'collect') & legendpos,
                    coord_cartesian = plot_list(gglist=setNames(c(list(pp2), pp1, list(p1)), NULL),
                                                ncol=1,
                                                heights=relrange,#c(rev(relrange[-1]), relrange[1]),
                                                guides = 'collect') & legendpos
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
