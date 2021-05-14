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
    coord_fun <- check_coord_flip(plot=x, axis=axis) 
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
