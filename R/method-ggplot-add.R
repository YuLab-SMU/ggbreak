#' @importFrom ggplot2 ggplot_add
#' @importFrom rlang abort
#' @method ggplot_add ggbreak_params
#' @export
ggplot_add.ggbreak_params <- function(object, plot, ...) {
    axis <- object$axis
    attr_name <- paste0('axis_break_', axis)

    if (inherits(plot, "ggbreak")) {
        existing <- attr(plot, attr_name)
        if (!is.null(existing)) {
            # Same axis: merge multiple breaks
            object <- list.add(existing, object)
        }
    }

    attr(plot, attr_name) <- object
    if (!"ggbreak" %in% class(plot)) {
        class(plot) <- c("ggbreak", class(plot))
    }

    return(plot)
}


#' @method ggplot_add wrap_params
#' @export
ggplot_add.wrap_params <- function(object, plot, ...){
    attr(plot, "axis_wrap") <- object
    class(plot) <- c("ggwrap", class(plot))
    return(plot)
}


#' @method ggplot_add ggcut_params
#' @export
ggplot_add.ggcut_params <- function(object, plot, ...){
    attr(plot, "axis_cut") <- object
    class(plot) <- c("ggcut", class(plot))
    return (plot)
}

#' @method ggplot_add ggbreak
#' @importFrom ggfun is.ggbreak
#' @export
ggplot_add.ggbreak <- function(object, plot, ...) {
    if (is.ggbreak(plot)) {
        ggplot_add(ggbreak2ggplot(object),
                   ggbreak2ggplot(plot),
                   ...)
    } else{
        ggplot_add(as.ggplot(grid.draw(object, recording=FALSE)),
                   as.ggplot(plot),
                   ...)
    }
}


#' @method ggplot_add ggwrap
#' @export
ggplot_add.ggwrap <- ggplot_add.ggbreak

#' @method ggplot_add ggcut
#' @export
ggplot_add.ggcut <- ggplot_add.ggbreak

#' @method ggplot_add gg
#' @importFrom ggfun ggbreak2ggplot
#' @export
ggplot_add.gg <- function(object, plot, ...){
    if (is.ggbreak(plot)){
        if (inherits(object, all_class_gg)){
            tmp <- class(plot)
            plot <- .drop_class(plot, "ggbreak")
            plot <- ggplot_add(object, plot, ...)
            class(plot) <- tmp
            return(plot)
        }else{
            ggplot_add(as.ggplot(object), ggbreak2ggplot(plot), ...)
	}
    } else{
        NextMethod()
    }
}


all_class_gg <- c("Scale", "Guides", "Coord", "Facet", "Layer", 
                  "Layout", "theme", "ggplot2::labels", "ggplot2::mapping")
