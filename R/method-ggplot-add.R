##' @importFrom ggplot2 ggplot_add
##' @importFrom rlang abort
##' @method ggplot_add ggbreak_params
##' @export
ggplot_add.ggbreak_params <- function(object, plot, object_name) {
    if (inherits(plot, "ggbreak")){
        origin_axis_break <- attr(plot, 'axis_break')
        origin_axis <- ifelse(inherits(origin_axis_break, "ggbreak_params"), 
                              origin_axis_break$axis, 
                              origin_axis_break[[1]]$axis)
        if (origin_axis != object$axis){
            rlang::abort("The truncation of different axis is not be supported simultaneously.
                         The ", origin_axis_break$axis," axis of original plot has been
                         truncated.")
        }else{
            object <- list.add(origin_axis_break, object)
        }
    }
    attr(plot, 'axis_break') <- object
    class(plot) <- c("ggbreak", class(plot))
    return(plot)
}


##' @method ggplot_add wrap_params
##' @export
ggplot_add.wrap_params <- function(object, plot, object_name){
    attr(plot, "axis_wrap") <- object
    class(plot) <- c("ggwrap", class(plot))
    return(plot)
}


##' @method ggplot_add ggcut_params
##' @export
ggplot_add.ggcut_params <- function(object, plot, object_name){
    attr(plot, "axis_cut") <- object
    class(plot) <- c("ggcut", class(plot))
    return (plot)
}

##' @method ggplot_add ggbreak
##' @export
ggplot_add.ggbreak <- function(object, plot, object_name) {
    ggplot_add(as.ggplot(print(object)),
               as.ggplot(print(plot)),
               object_name)
}

##' @method ggplot_add ggwrap
##' @export
ggplot_add.ggwrap <- ggplot_add.ggbreak


##' @method ggplot_add ggcut
##' @export
ggplot_add.ggcut <- ggplot_add.ggbreak

##' @method ggplot_add gg
##' @export
ggplot_add.gg <- function(object, plot, object_name){
    if (inherits(plot, "ggbreak") || inherits(plot, "ggcut") || inherits(plot, "ggwrap")){
        ggplot_add(as.ggplot(print(object)), as.ggplot(print(plot)), object_name)
    }else{
        NextMethod()
    }
}
