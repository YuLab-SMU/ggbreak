##' @importFrom grid grid.draw
##' @method grid.draw ggbreak
##' @importFrom ggplotify as.ggplot
##' @importFrom ggplot2 coord_cartesian
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_text
##' @importFrom aplot plot_list
##' @export
grid.draw.ggbreak <- function(x, recording = TRUE) {
    class(x) <- class(x)[class(x) != "ggbreak"]
    axis_break <- attr(x, 'axis_break')
    axis_breaks <- extract_axis_break(object=axis_break)
    axis <- axis_breaks$axis
    breaks <- axis_breaks$breaks
    scales <- axis_breaks$scales
    rng <- ggrange2(plot=x, var=axis)
    res <- combine_range(breaks, rng, scales)
    breaks <- res$breaks
    scales <- res$scales

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
    if(axis == 'x') {
        p1 <- x + do.call(coord_fun, list(xlim = c(breaks[[1]][1], breaks[[1]][2]))) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i) 
                            x + do.call(coord_fun, list(xlim=c(i[1], i[2]))) + 
                            subplottheme2)
        pp2 <- x + do.call(coord_fun, list(xlim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) + 
               subplottheme3
        #g <- switch(coord_fun,
        #            coord_flip=plot_grid(plotlist=c(list(pp2), pp1, list(p1)), align="v", ncol=1),
        #            coord_cartesian=plot_grid(plotlist=c(list(p1), pp1, list(pp2)), align="h", nrow=1)
        #            )
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=c(list(pp2), rev(pp1), list(p1)), 
                                           ncol=1,
                                           heights=c(rev(relrange[-1]), relrange[1]),
                                           guides = 'collect'),
                    coord_cartesian = plot_list(gglist=c(list(p1), pp1, list(pp2)), 
                                                nrow=1, 
                                                widths=relrange,
                                                guides = 'collect')
                    )
    } else {
        breaks <- rev(breaks)
        p1 <- x + do.call(coord_fun, list(ylim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i) 
                      x + do.call(coord_fun, list(ylim=c(i[1], i[2]))) +
                            subplottheme2)
        pp2 <- x + do.call(coord_fun, list(ylim = c(breaks[[1]][1], breaks[[1]][2]))) +
               subplottheme3
        #g <- switch(coord_fun,
        #            coord_flip = plot_grid(plotlist=c(list(p1), pp1, list(pp2)), align="h", nrow=1),
        #            coord_cartesian = plot_grid(plotlist=c(list(pp2), pp1, list(p1)), align="v", ncol=1)
        #       )
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=c(list(p1), rev(pp1), list(pp2)), 
                                           nrow=1, 
                                           widths=relrange,
                                           guides = 'collect'),
                    coord_cartesian = plot_list(gglist=c(list(pp2), pp1, list(p1)), 
                                                ncol=1, 
                                                heights=c(rev(relrange[-1]), relrange[1]),
                                                guides = 'collect')
               )
    }

    totallabs$x <- NULL
    totallabs$y <- NULL
    g <- ggplotify::as.ggplot(g) + xlab(newxlab) + ylab(newylab)
    g <- set_label(g, totallabs = totallabs, p2 = x)
    print(g)
}

##' @method grid.draw ggwrap
##' @export
grid.draw.ggwrap <- function(x, recording=TRUE){
    class(x) <- class(x)[class(x) != "ggwrap"]
    axis_wrap <- attr(x, "axis_wrap")
    totallabs <- extract_totallabs(plot=x)
    if (length(totallabs) > 0){
        x$labels[names(totallabs)] <- NULL
    }
    nstep <- axis_wrap$n
    rngrev <- ggrange2(plot=x, 'x')
    rng <- rngrev$axis_range
    if (rngrev$flagrev == "reverse"){
        rng <- rev(-1 * (rng))
    }
    breaks <- seq(rng[1], rng[2], length.out=nstep + 1)
    gg <- lapply(seq_len(length(breaks)-1), function(i) x + coord_cartesian(xlim=c(breaks[i], breaks[i+1])))
    pg <- plot_list(gg, ncol=1, guides="collect")
    g <- set_label(as.ggplot(pg), totallabs=totallabs, p2=x)
    print (g)
}

#' @method grid.draw ggcut
#' @export
grid.draw.ggcut <- function(x, recording=TRUE){
    class(x) <- class(x)[class(x) != "ggcut"]
    axis_cut <- attr(x, "axis_cut")
    axis <- axis_cut$axis
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

    if(axis == 'x') {
        p1 <- x + do.call(coord_fun, list(xlim = c(breaks[[1]][1], breaks[[1]][2]))) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i)
                            x + do.call(coord_fun, list(xlim=c(i[1], i[2]))) +
                            subplottheme2)
        pp2 <- x + do.call(coord_fun, list(xlim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) +
               subplottheme3
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=c(list(pp2), rev(pp1), list(p1)),
                                           ncol=1,
                                           heights=relrange,#c(rev(relrange[-1]), relrange[1]),
                                           guides = 'collect'),
                    coord_cartesian = plot_list(gglist=c(list(p1), pp1, list(pp2)),
                                                nrow=1,
                                                widths=relrange,
                                                guides = 'collect')
                    )
    } else {
        breaks <- rev(breaks)
        p1 <- x + do.call(coord_fun, list(ylim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i)
                      x + do.call(coord_fun, list(ylim=c(i[1], i[2]))) +
                            subplottheme2)
        pp2 <- x + do.call(coord_fun, list(ylim = c(breaks[[1]][1], breaks[[1]][2]))) +
               subplottheme3
        g <- switch(coord_fun,
                    coord_flip = plot_list(gglist=c(list(p1), rev(pp1), list(pp2)),
                                           nrow=1,
                                           widths=relrange,
                                           guides = 'collect'),
                    coord_cartesian = plot_list(gglist=c(list(pp2), pp1, list(p1)),
                                                ncol=1,
                                                heights=relrange,#c(rev(relrange[-1]), relrange[1]),
                                                guides = 'collect')
               )
    }
    totallabs$x <- NULL
    totallabs$y <- NULL
    g <- ggplotify::as.ggplot(g) + xlab(newxlab) + ylab(newylab)
    g <- set_label(g, totallabs = totallabs, p2 = x)
    print(g)
}
