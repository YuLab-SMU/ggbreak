##' @importFrom grid grid.draw
##' @method grid.draw ggbreak
##' @importFrom ggplotify as.ggplot
##' @importFrom cowplot plot_grid
##' @importFrom ggplot2 coord_cartesian
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_text
##' @export
grid.draw.ggbreak <- function(x, recording = TRUE) {
    axis_break <- attr(x, 'axis_break')
    axis_break <- extract_axis_break(object=axis_break)
    axis <- axis_break$axis
    breaks <- axis_break$breaks
    rng <- ggrange2(x, axis)
    breaks <- combine_range(breaks, rng)

    xlab <- x$label$x
    ylab <- x$label$y
    x <- x + xlab(NULL) + ylab(NULL)
    nbreaks <- length(breaks)
    subplottheme1 <- subplot_theme(plot=x, axis=axis, type="first")
    subplottheme2 <- subplot_theme(plot=x, axis=axis, type="other")
    subplottheme3 <- subplot_theme(plot=x, axis=axis, type="last")
    coord_fun <- check_coord_flip(plot=x, axis=axis) 
    newxlab <- switch(coord_fun, coord_flip=ylab, coord_cartesian=xlab)
    newylab <- switch(coord_fun, coord_flip=xlab, coord_cartesian=ylab)
    if(axis == 'x') {
        p1 <- x + do.call(coord_fun, list(xlim = c(breaks[[1]][1], breaks[[1]][2]))) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i) 
                            x + do.call(coord_fun, list(xlim=c(i[1], i[2]))) + 
                            subplottheme2)
        pp2 <- x + do.call(coord_fun, list(xlim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2]))) + 
               subplottheme3
        g <- switch(coord_fun,
                    coord_flip=plot_grid(plotlist=c(list(pp2), pp1, list(p1)), align="v", ncol=1),
                    coord_cartesian=plot_grid(plotlist=c(list(p1), pp1, list(pp2)), align="h", nrow=1)
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
                    coord_flip = plot_grid(plotlist=c(list(p1), pp1, list(pp2)), align="h", nrow=1),
                    coord_cartesian = plot_grid(plotlist=c(list(pp2), pp1, list(p1)), align="v", ncol=1)
               )
    }

    g <- ggplotify::as.ggplot(g) 

    g <- set_axis_label(g, xlab = newxlab, ylab = newylab, p2 = x)
        
    print(g)
}
