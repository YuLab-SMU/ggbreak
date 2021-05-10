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
    axis <- axis_break$axis
    breaks <- axis_break$breaks

    xlab <- x$label$x
    ylab <- x$label$y
    x <- x + xlab(NULL) + ylab(NULL)
    rng <- ggrange2(x, axis)
    if(axis == 'x') {
        p1 <- x + coord_cartesian(xlim = c(rng[1], breaks[1]))
        p2 <- x + coord_cartesian(xlim = c(breaks[2], rng[2]))
        p2 <- p2 +
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        g <- cowplot::plot_grid(p1, p2, align = 'h', nrow = 1)
    } else {
        p1 <- x + coord_cartesian(ylim = c(breaks[2], rng[2]))
        p1 <- p1 +
            theme(axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
        p2 <- x + coord_cartesian(ylim = c(rng[1], breaks[1]))
        g <- cowplot::plot_grid(p1,p2, align = 'v', ncol = 1)
    }

    g <- ggplotify::as.ggplot(g) 

    g <- set_axis_label(g, xlab = xlab, ylab = ylab, p2 = x)
        
    print(g)
}


