##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add ggbreak_params
##' @export
ggplot_add.ggbreak_params <- function(object, plot, object_name) {
    attr(plot, 'axis_break') <- object
    class(plot) <- c("ggbreak", class(plot))
    return(plot)
}

