## A broken plot is not drawn by `ggplot2`.  `ggbreak` assembles the windows and
## draws them itself, so the break is applied by `grid.draw()` and by `print()`
## and nowhere else.  `patchwork` and `cowplot` go through neither of them: they
## call `ggplotGrob()`, which is `ggplot_gtable(ggplot_build(x))`, and for a
## `ggbreak` object that builds the plot *without* the break, because the break
## is kept in an attribute that only `grid.draw.ggbreak()` ever reads.  A broken
## plot combined with other plots therefore came out with no break at all and
## without a warning, see #46.
##
## `ggplotGrob()` itself cannot be intercepted, it is not a generic, but the two
## functions it is made of are.  `ggplot_build()` dispatches on the class of the
## *plot* and `ggplot_gtable()` on the class of the *built* object, so the built
## object is tagged below and picked up by the method that follows.

#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build ggbreak
#' @export
ggplot_build.ggbreak <- function(plot, ...) {
    mark_ggbreak_built(NextMethod(), plot)
}

#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build ggwrap
#' @export
ggplot_build.ggwrap <- function(plot, ...) {
    mark_ggbreak_built(NextMethod(), plot)
}

#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build ggcut
#' @export
ggplot_build.ggcut <- function(plot, ...) {
    mark_ggbreak_built(NextMethod(), plot)
}

## the class is what `ggplot_gtable()` dispatches on, the attribute carries the
## plot itself because the built object does not remember the break
mark_ggbreak_built <- function(built, plot) {
    attr(built, "ggbreak_plot") <- plot
    class(built) <- c("ggbreak_built", class(built))
    built
}

#' @importFrom ggplot2 ggplot_gtable
#' @method ggplot_gtable ggbreak_built
#' @export
ggplot_gtable.ggbreak_built <- function(data) {
    ## `grid.draw()` hands back the outer ggplot that carries the assembled
    ## figure rather than the figure itself, so it is converted here
    ##
    ## the drawing is done under `suppressWarnings()` for the same reason
    ## `print.ggbreak()` does it: `aplot::all_ggplot()` still calls the
    ## deprecated `is.ggplot()`, and that warning used to be swallowed by the
    ## `print()` path and would show up here instead
    ggplot2::ggplotGrob(
        suppressWarnings(grid::grid.draw(attr(data, "ggbreak_plot"), recording = FALSE))
    )
}
