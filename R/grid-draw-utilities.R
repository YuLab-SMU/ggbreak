subplot_theme <- function(plot, axis, type){
    type <- check_strip_pos(plot=plot, type=type)
    axis <- check_theme_coflip(plot=plot, axis=axis)
    te <- switch(type,
                 first = strip_theme(plot=plot, axis=axis),
                 other = axis_theme(plot=plot, axis=axis) + 
                         strip_theme(plot, axis=axis),
                 last = axis_theme(plot=plot, axis=axis),
                 #internalfirst = axis_theme(plot=plot, axis=axis) + 
                 #    strip_theme(plot=plot, axis=axis),
                 internallast = list()) 
    te <- te + theme_no_margin()
    return(te)
}

axis_theme <- function(plot, axis){
    axis_theme <- switch(axis, 
                        x = theme(axis.text.y=element_blank(),
                                  axis.ticks.y=element_blank()),
                        y = theme(axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank())
                  )
    return(axis_theme)
}

strip_theme <- function(plot, axis){
    sp_theme <- switch(axis, 
                      x = theme(strip.background.y=element_blank(),
                                strip.text.y=element_blank()),
                      y = theme(strip.background.x=element_blank(),
                                strip.text.x=element_blank())
                )
    return(sp_theme)
}

check_strip_pos <- function(plot, type){
    if (length(plot$facet$params)>0){
        flagwrap <- plot$facet$params$strip.position
        if ((!is.null(flagwrap) && flagwrap %in% c("bottom", "left")) || 
            !is.null(plot$facet$params$switch)){
            type <- switch(type,
                           first = "internallast",
                           last = "other",
                           other = "other"
                          )
        }
    }
    return(type)
}

check_theme_coflip <- function(plot, axis){
    if (inherits(plot, "gg") && inherits(plot$coordinates, "CoordFlip")){
        axis <- switch(axis, x = "y", y = "x" )
    }
    return(axis)
}

#' @importFrom ggplot2 coord_flip
check_coord_flip <- function(plot, axis){
    if (inherits(plot, "gg") && inherits(plot$coordinates, "CoordFlip")){
        return("coord_flip")
    }
    return ("coord_cartesian")
}