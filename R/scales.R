##' Set an axis break point for a 'gg' plot
##'
##' This scale function set an axis break point for a 'gg' plot. Either 'x' and 'y' axes are supported. The result is still a 'gg' object and user can progressively add layers to it. 
##' @title scale_x_break
##' @rdname scale_break
##' @param breaks break point
##' @param scales relative width or height of subplots,
##' default is "fixed". If scale is 'free', all subplots have 
##' equal width or height. It also can be any number to set 
##' relative width or height compare to first subplot.
##' @param ticklabels the axis labels to subplot, default is NULL.
##' @param expand default is TRUE, logical or a vector of range expansion constants
##' used to add some padding around the data to ensure that they
##' are placed some distance away from the axes. Use the convenience 
##' function \code{expansion()} of \code{ggplot2} to generate the values for
##' the \code{expand} argument. The defaults are to expand the scale
##' by 5% on each side for continuous variables. If it is logical, the \code{TRUE} means
##' the default of \code{ggplot2} (foregoing statement), and \code{FALSE} means no expand for the plot.
##' @param space the blank space among the subplots after break, default is 0.1 (cm).
##' @return gg object
##' @export
##' @author Guangchuang Yu
scale_x_break <- function(breaks, scales="fixed", ticklabels=NULL, expand=TRUE, space = 0.1) {
    scale_break('x', breaks, scales, ticklabels, expand, space)
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
scale_y_break <- function(breaks, scales="fixed", ticklabels=NULL, expand=TRUE, space = .1) {
    scale_break('y', breaks, scales, ticklabels, expand, space)
}

scale_break <- function(axis, breaks, scales, ticklabels=NULL, expand=TRUE, space = .1) {
    structure(list(axis = axis, breaks = breaks, scales=scales, 
                   ticklabels=ticklabels, expand = expand, space = space),
              class = "ggbreak_params")
}

#' @title scale_wrap
#'
#' This scale function wraps a 'gg' plot over multiple rows to make plots with long x axes easier to read.
#' @param n the number of subplot pieces.
#' @return gg object
#' @export
#' @examples
#' library(ggplot2)
#' library(ggbreak)
#' p <- ggplot(economics, aes(x=date, y = unemploy, colour = uempmed)) +
#'      geom_line()
#' p + scale_wrap(n=4)
scale_wrap <- function(n){
    structure(list(n = n, expand = FALSE), 
              class = "wrap_params")
}


#' @title scale_x_cut
#' @param breaks a numeric or numeric vector, the points to be divided
#' @param which integer, the position of subplots to scales, started from left to right or top to bottom.
#' @param scales numeric, relative width or height of subplots.
#' @param expand default is FALSE, logical a vector of range expansion constants
#' used to add some padding around the data to ensure that they
#' are placed some distance away from the axes. Use the convenience
#' function \code{expansion()} of \code{ggplot2} to generate the values for
#' the \code{expand} argument. The defaults are to expand the scale
#' by 5% on each side for continuous variables. If it is logical, the \code{TRUE} means
#' the default of \code{ggplot2} (foregoing statement), and \code{FALSE} means no expand for the plot.
#' @param space the blank space among the subplots after cut, default is 0.1 (cm). 
#' @rdname scale_cut
#' @return gg object
#' @export
scale_x_cut <- function(breaks, which=NULL, scales=NULL, expand = FALSE, space = .1){
    scale_cut(
        axis = "x",
        breaks = breaks,
        which = which,
        scales = scales,
        expand = expand,
        space = space
    )
}

#' @title scale_y_cut
#' @rdname scale_cut
#' @export
#' @examples
#' library(ggplot2)
#' library(ggbreak)
#' set.seed(2019-01-19)
#' d <- data.frame(
#'      x = 1:20,
#'      y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
#'  )
#' p <- ggplot(d, aes(x, y)) + geom_col()
#' p + scale_y_cut(breaks=c(7, 18), which=c(1, 3), scales=c(3, 0.5))
scale_y_cut <- function(breaks, which=NULL, scales=NULL, expand = FALSE, space = .1){
    scale_cut(
        axis = "y",
        breaks = breaks,
        which = which,
        scales = scales,
        expand = expand,
        space = space
    )

}

scale_cut <- function(axis, breaks, which=NULL, scales=NULL, expand = FALSE, space = .1){
    structure(list(axis = axis, 
                   breaks = breaks, 
                   which = which, 
                   scales = scales,
                   expand = expand,
                   space = space
                   ),
    class = "ggcut_params")
}
