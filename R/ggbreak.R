## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    axis_range <- ggplot_build(plot)$layout[[var]][[1]]$range$range
    flagrev <- ggplot_build(plot)$layout[[var]][[1]]$trans$name
    list(axis_range=axis_range, flagrev=flagrev)
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
    axis_break <- extract_axis_break(object=axis_break)
    axis <- axis_break$axis
    breaks <- axis_break$breaks
    rng <- ggrange2(x, axis)
    breaks <- combine_range(breaks, rng)
    has_theme <- length(x$theme) != 0
    if (has_theme) {
        xlab_param <- x$theme$axis.title.x
        ylab_param <- x$theme$axis.title.y
    }

    xlab <- x$label$x
    ylab <- x$label$y
    x <- x + xlab(NULL) + ylab(NULL)
    if(axis == 'x') {
        p1 <- x + coord_cartesian(xlim = c(breaks[[1]][1], breaks[[1]][2]))
        pp <- lapply(breaks[-1], function(i) x + coord_cartesian(xlim=c(i[1], i[2])) + 
                                             theme(axis.text.y=element_blank(), 
                                                   axis.ticks.y=element_blank()))
        g <- do.call(plot_grid, list(plotlist=c(list(p1), pp), align="h", nrow=1))
    } else {
        p1 <- x + coord_cartesian(ylim = c(breaks[[1]][1], breaks[[1]][2]))
        pp <- lapply(rev(breaks[-1]), function(i) x + coord_cartesian(ylim=c(i[1], i[2])) +
                                           theme(axis.text.x=element_blank(),
                                                 axis.ticks.x=element_blank()))
        g <- do.call(plot_grid, list(plotlist=c(pp,list(p1)), align="v", ncol=1))
    }

    g <- ggplotify::as.ggplot(g) +
        xlab(xlab) + ylab(ylab)

    if (has_theme) {
        g <- g + theme(axis.title.x = do.call(element_text, xlab_param),
                       axis.title.y = do.call(element_text, ylab_param))
    } else {
        g <- g + theme(axis.title.x = element_text(vjust = 1),
                       axis.title.y = element_text(angle = 90, vjust = 1)) 

    }
        
    print(g)
}

combine_range <- function(breaks, rangeres){
    if (rangeres$flagrev=="reverse"){
        rangeres$axis_range <- sort(abs(rangeres$axis_range))
    }
    newbreaks <- merge_intervals(breaks)
    newbreaks <- c(rangeres$axis_range[1], unlist(newbreaks), rangeres$axis_range[2])
    newbreaks <- lapply(data.frame(matrix(newbreaks, nrow=2)), function(i)i)
    return(newbreaks)
}

merge_intervals <- function(breaks){
    if (!inherits(breaks, "list")){
        breaks <- list(breaks)
    }
    out <- list()
    breaks <- lapply(breaks, function(i) sort(i))
    breaks <- breaks[rank(unlist(lapply(breaks, function(i)i[[1]])))]
    for (i in breaks){
        if (length(out) > 1 && i[1] <= out[[length(out)]][2]){
            out[[length(out)]][2] <- max(out[[length(out)]][2], i[2])
        }else{
            out <- c(out, list(i))
        }
    }
    return(out)
}

extract_axis_break <- function(object){
    if (inherits(object, "ggbreak_params")){
        axis <- object$axis
        breaks <- object$breaks
    }else{
        axis <- object[[1]]$axis
        breaks <- lapply(object, function(i)i$breaks)
    }
    return(list(axis=axis, breaks=breaks))
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
