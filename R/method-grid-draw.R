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
	#relrange <- unlist(lapply(breaks, function(i) abs(diff(i))))
    if(axis == 'x') {
        p1 <- x + coord_cartesian(xlim = c(breaks[[1]][1], breaks[[1]][2]))
        pp <- lapply(breaks[-1], function(i) x + coord_cartesian(xlim=c(i[1], i[2])) + 
                            theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()))
        g <- do.call(plot_grid, list(plotlist=c(list(p1), pp), align="h", nrow=1))#, rel_widths=relrange))
    } else {
        p1 <- x + coord_cartesian(ylim = c(breaks[[1]][1], breaks[[1]][2]))
        pp <- lapply(rev(breaks[-1]), function(i) x + coord_cartesian(ylim=c(i[1], i[2])) +
                            theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()))
        #relrange <- c(relrange[-1], relrange[1])
        g <- do.call(plot_grid, list(plotlist=c(pp,list(p1)), align="v", ncol=1))#, rel_heights=relrange))
    }

    g <- ggplotify::as.ggplot(g) 

    g <- set_axis_label(g, xlab = xlab, ylab = ylab, p2 = x)
        
    print(g)
}


