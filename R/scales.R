##' Set an axis break point for a 'gg' plot
##'
##' This scale function set an axis break point for a 'gg' plot. Either 'x' and 'y' axes are supported. The result is still a 'gg' object and user can progressively add layers to it. 
##' @title scale_x_break
##' @rdname scale_break
##' @param breaks break point
##' @param scales, relative width or height of subplots,
##' default is "fixed". If scale is 'free', all subplots have 
##' equal width or height. It also can be any number to set 
##' relative width or height compare to first subplot.
##' @return gg object
##' @export
##' @author Guangchuang Yu
scale_x_break <- function(breaks, scales="fixed") {
    scale_break('x', breaks, scales)
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
scale_y_break <- function(breaks, scales="fixed") {
    scale_break('y', breaks, scales)
}

scale_break <- function(axis, breaks, scales) {
    structure(list(axis = axis, breaks = breaks, scales=scales),
              class = "ggbreak_params")
}

#' @title scale_wrap
#' @param n the number of subplot pieces.
#' @return gg object
#' @export 
scale_wrap <- function(n){
    structure(list(n = n), 
              class = "wrap_params")
}
