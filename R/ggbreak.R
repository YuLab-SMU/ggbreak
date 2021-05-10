## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    ggplot_build(plot)$layout[[var]][[1]]$range$range
}

##' Set an axis break point for a 'gg' plot
##'
##' This scale function set an axis break point for a 'gg' plot. Either 'x' and 'y' axes are supported. The result is still a 'gg' object and user can progressively add layers to it. 
##' @title scale_x_break
##' @rdname scale_break
##' @param breaks break point
##' @return gg object
##' @export
##' @author Guangchuang Yu
scale_x_break <- function(breaks) {
    scale_break('x', breaks)
}

##' @title scale_y_break
##' @rdname scale_break
##' @export
##' @examples
##' require(ggplot2 )
##' set.seed(2019-01-19)
##' d <- data.frame(
##'   x = 1:20,
##'   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
##' )
##'
##' p <- ggplot(d, aes(x, y)) + geom_col()
##' x <- p+scale_y_break(c(7, 17 ) )
##' print(x)
scale_y_break <- function(breaks) {
    scale_break('y', breaks)
}

scale_break <- function(axis, breaks) {
    structure(list(axis = axis, breaks = breaks),
              class = "ggbreak_params")
}

##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add ggbreak_params
##' @export
ggplot_add.ggbreak_params <- function(object, plot, object_name) {
    attr(plot, 'axis_break') <- object
    class(plot) <- c("ggbreak", class(plot))
    return(plot)
}

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

    g <- ggplotify::as.ggplot(g) +
        xlab(xlab) + ylab(ylab) +
        theme(axis.title = element_text()) 
    return(g)
}

##' @method plot ggbreak
##' @export
plot.ggbreak <- function(x, y, ...) {
    NextMethod()
}

##' @method print ggbreak
##' @export
print.ggbreak <- function(x, ...) {
    grid.draw.ggbreak(x)
}
