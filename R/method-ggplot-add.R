##' @importFrom ggplot2 ggplot_add
##' @importFrom rlang abort
##' @method ggplot_add ggbreak_params
##' @export
ggplot_add.ggbreak_params <- function(object, plot, object_name) {
    if (inherits(plot, "ggbreak")){
        origin_axis_break <- attr(plot, 'axis_break')
        if (origin_axis_break$axis != object$axis){
            rlang::abort("The truncation of different axis is not be supported simultaneously.
                         The ", origin_axis_break$axis," axis of original plot has been
                         truncated.")
        }else{
            object <- list(origin_axis_break, object)
        }
    }
    attr(plot, 'axis_break') <- object
    class(plot) <- c("ggbreak", class(plot))
    return(plot)
}

##' @method ggplot_add ggbreak
##' @export
ggplot_add.ggbreak <- function(object, plot, object_name) {
    ggplot_add(print(object), print(plot), object_name)
}
