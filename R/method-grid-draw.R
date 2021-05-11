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
    if(axis == 'x') {
        p1 <- x + coord_cartesian(xlim = c(breaks[[1]][1], breaks[[1]][2])) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i) 
                            x + coord_cartesian(xlim=c(i[1], i[2])) + 
                            subplottheme2)
        pp2 <- x + coord_cartesian(xlim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2])) + 
               subplottheme3
        g <- do.call(plot_grid, list(plotlist=c(list(p1), pp1, list(pp2)), align="h", nrow=1))
    } else {
        breaks <- rev(breaks)
        p1 <- x + coord_cartesian(ylim = c(breaks[[nbreaks]][1], breaks[[nbreaks]][2])) + subplottheme1
        pp1 <- lapply(breaks[-c(1, nbreaks)], function(i) x + coord_cartesian(ylim=c(i[1], i[2])) +
                            subplottheme2)
        pp2 <- x + coord_cartesian(ylim = c(breaks[[1]][1], breaks[[1]][2])) +
               subplottheme3
        g <- do.call(plot_grid, list(plotlist=c(list(pp2), pp1, list(p1)), align="v", ncol=1))
    }

    g <- ggplotify::as.ggplot(g) 

    g <- set_axis_label(g, xlab = xlab, ylab = ylab, p2 = x)
        
    print(g)
}

subplot_theme <- function(plot, axis, type){
    switch(type,
           first = strip_theme(plot=plot, axis=axis),
           other = axis_theme(plot=plot, axis=axis) + 
                   strip_theme(plot, axis=axis),
           last = axis_theme(plot=plot, axis=axis))
}

axis_theme <- function(plot, axis){
    axis_theme <- switch(axis, 
                        x = theme(axis.text.y=element_blank(),
                                  axis.ticks.y=element_blank()),
                        y = theme(axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank())
                  )
    return(axis_theme)
}

strip_theme <- function(plot, axis){
    sp_theme <- switch(axis, 
                      x = theme(strip.background.y=element_blank(),
                                strip.text.y=element_blank()),
                      y = theme(strip.background.x=element_blank(),
                                strip.text.x=element_blank())
                )
    return(sp_theme)
}
