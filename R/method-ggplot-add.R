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
    ## A cut is stored as a single attribute, so a second cut on the other axis
    ## would replace the first one and the plot would come out cut on one axis
    ## only, without a word about the cut that was dropped.  A break does keep
    ## both axes (`axis_break_x` and `axis_break_y` are separate), a cut does
    ## not, see #31.
    existing <- attr(plot, "axis_cut")
    if (!is.null(existing) && !identical(existing$axis, object$axis)) {
        abort(c(
            paste0("`scale_", existing$axis, "_cut()` cannot be combined with ",
                   "`scale_", object$axis, "_cut()`."),
            i = paste0("Only one axis can be cut, and the second cut would ",
                       "replace the first one.")
        ))
    }
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
            ## `is.ggbreak()` is TRUE for "ggbreak", "ggwrap" and "ggcut", so
            ## all three have to go.  Dropping only "ggbreak" leaves a "ggwrap"
            ## or "ggcut" object behind and `ggplot_add()` re-dispatches to this
            ## method for ever: adding any coord, theme, scale, facet or layer
            ## to a wrapped or cut plot used to die with a C stack overflow.
            plot <- .drop_class(plot, c("ggbreak", "ggwrap", "ggcut"))
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
