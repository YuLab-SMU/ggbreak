subplot_theme <- function(plot, axis, type, margin = .2, rev){
    type <- check_strip_pos(plot=plot, type=type)
    axis.pos <- check_another_position(plot = plot, axis = axis)
    axis <- check_theme_coflip(plot=plot, axis=axis)
    if (axis.pos == 'another.position'){
        te <- switch(type,
                     first = axis_theme(plot=plot, axis = axis) +
                             strip_theme(plot=plot, axis=axis) +
                             first_margin_theme(axis = axis, margin = margin, rev=rev),
                     other = axis_theme(plot=plot, axis=axis) +
                             strip_theme(plot, axis=axis) +
                             other_margin_theme(axis = axis, margin = margin),
                     last = last_margin_theme(axis = axis, margin = margin, rev=rev),
                     #internalfirst = axis_theme(plot=plot, axis=axis) +
                     #    strip_theme(plot=plot, axis=axis),
                     internallast = list())        
    }else if (axis.pos == 'another.secondary.axis'){
        te <- switch(type,
                     first = axis_theme(plot = plot, axis = axis, secondary.axis = TRUE) +
                             strip_theme(plot=plot, axis=axis) + 
                             first_margin_theme(axis = axis, margin = margin, rev=rev),
                     other = axis_theme(plot=plot, axis=axis, secondary.axis=TRUE, type = 'other') +
                             strip_theme(plot, axis=axis) +
                             other_margin_theme(axis = axis, margin = margin),
                     last =  axis_theme(plot=plot, axis = axis, secondary.axis = TRUE, type='last') +
                             last_margin_theme(axis = axis, margin = margin, rev=rev),
        
              )
    
    }else{
        te <- switch(type,
                     first = strip_theme(plot=plot, axis=axis) +
                             first_margin_theme(axis = axis, margin = margin, rev=rev),
                     other = axis_theme(plot=plot, axis=axis) + 
                             strip_theme(plot, axis=axis) +
                             other_margin_theme(axis = axis, margin = margin),
                     last = axis_theme(plot=plot, axis = axis) +
                            last_margin_theme(axis = axis, margin = margin, rev=rev),
                     #internalfirst = axis_theme(plot=plot, axis=axis) + 
                     #    strip_theme(plot=plot, axis=axis),
                     internallast = list())
    } 
    #te <- te + theme_no_margin()
    return(te)
}

axis_theme <- function(plot, axis, secondary.axis=FALSE, type='first'){
    if (secondary.axis){
        if (type == 'first'){
            axis_theme <- switch(axis,
                                 x = theme(axis.text.y.right = element_blank(),
                                           axis.ticks.y.right = element_blank(),
                                           axis.line.y.right = element_blank(),
                                           axis.title.y.right = element_blank()
                                     ),
                                 y = theme(axis.text.x.top = element_blank(),
                                           axis.ticks.x.top = element_blank(),
                                           axis.line.x.top = element_blank(),
                                           axis.title.x.top = element_blank()
                                 )
            
                          )    
        }else if (type == 'last'){
            axis_theme <- switch(axis,
                                 x = theme(axis.text.y.left = element_blank(),
                                           axis.ticks.y.left = element_blank(),
                                           axis.line.y.left = element_blank(),
                                           axis.title.y.left = element_blank()
                                     ),
                                 y = theme(axis.text.x.bottom = element_blank(),
                                           axis.ticks.x.bottom = element_blank(),
                                           axis.line.x.bottom = element_blank(),
                                           axis.title.x.bottom = element_blank()
                                 )

                          ) 
        
        }else{
        axis_theme <- switch(axis,
                             x = theme(axis.text.y=element_blank(),
                                      axis.ticks.y=element_blank(),
                                      axis.line.y=element_blank(),
                                      axis.title.y=element_blank()),
                             y = theme(axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank(),
                                      axis.line.x=element_blank(),
                                      axis.title.x=element_blank())

                      )        
        
        }
    }else{
        axis_theme <- switch(axis, 
                            x = theme(axis.text.y=element_blank(),
                                      axis.ticks.y=element_blank(),
                                      axis.line.y=element_blank()),
                            y = theme(axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank(),
                                      axis.line.x=element_blank())
                      )
    }
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

#' @importFrom ggplot2 margin
first_margin_theme <- function(axis, margin, rev){
    if (rev == "reverse"){
      fmg_theme <- switch(axis,
        x = theme(plot.margin = margin(l = margin/2, unit = "cm")),
        y = theme(plot.margin = margin(b = margin/2, unit = "cm")),
      )
    }else{
      fmg_theme <- switch(axis,
        x = theme(plot.margin = margin(r = margin/2, unit = "cm")),
        y = theme(plot.margin = margin(t = margin/2, unit = "cm"))
      )
    }
    return (fmg_theme)
}

other_margin_theme <- function(axis, margin){
    mg_theme <- switch(axis,
      x = theme(plot.margin = margin(r = margin/2, l = margin/2, unit = "cm")),
      y = theme(plot.margin = margin(t = margin/2, b = margin/2, unit = "cm"))
    )
}

last_margin_theme <- function(axis, margin, rev){
    if (rev == "reverse"){
      lmg_theme <- switch(axis,
        x = theme(plot.margin = margin(r=margin/2, unit = "cm")),
        y = theme(plot.margin = margin(t=margin/2, unit = "cm"))
      )
    }else{
      lmg_theme <- switch(axis,
        x = theme(plot.margin = margin(l = margin/2, unit = "cm")),
        y = theme(plot.margin = margin(b = margin/2, unit = "cm"))
      )
    }
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

check_another_position <- function(plot, axis){
    another.axis <- setdiff(c("x", "y"), axis)
    if (plot$scales$has_scale(another.axis)){
        another.scaleind <- which(plot$scales$find(another.axis)) 
        if (#!inherits(plot$scales$scales[[another.scaleind]]$secondary.axis, "waiver") ||
            (another.axis == "x" && plot$scales$scales[[another.scaleind]]$position !='bottom') ||
            (another.axis == 'y' && plot$scales$scales[[another.scaleind]]$position !='left')
        ){
            return('another.position')
        }else if (!inherits(plot$scales$scales[[another.scaleind]]$secondary.axis, "waiver")){
            return("another.secondary.axis")
        }else{
            return('normal.position')
        }
    }else{
        return('normal.position')
    }
}

#' @importFrom ggplot2 coord_flip
check_coord_flip <- function(plot){
    if (inherits(plot, "gg") && inherits(plot$coordinates, "CoordFlip")){
        return("coord_flip")
    }
    return ("coord_cartesian")
}

compute_relative_range <- function(breaks, scales, rng){
    if(rng$flagrev=="reverse"){
        baserange <- abs(diff(rev(breaks)[[1]]))
        otherbk <- breaks[-length(breaks)]
    }else{ 
        baserange <- abs(diff(breaks[[1]]))
        otherbk <- breaks[-1]
    }
    relranges <- unlist(mapply(compute_relative_range_, 
                     breaks_= otherbk, 
                     scales_=scales, 
                     MoreArgs=list(baserange_=baserange), 
                     #baserange_ = baserange,
                     SIMPLIFY=FALSE))
    if (rng$flagrev == "reverse"){
        return (c(relranges, baserange)) 
    }else{
        return (c(baserange, relranges))
    }
}

compute_relative_range_ <- function(breaks_, scales_, baserange_){
    if (scales_=="fixed"){
        return(abs(diff(breaks_)))
    }
    if (scales_=="free"){
        scales_ = 1
    }
    if (!is_numeric(scales_) || length(scales_) > 1){
        abort("The scales must be a numeric or one of 'fixed', 'free' !")
    }
    relrange <- baserange_ * as.numeric(scales_)
    return (relrange)
}

is_numeric <- function(x) {
    !anyNA(suppressWarnings(as.numeric(x)))
}

check_xy_intercept <- function(plot){
    confuse_xy_labs <- c("xend", "xmax", "xmin", "xintercept", 
                         "yend", "ymax", "ymin", "yintercept")
    index <- which(names(plot$labels) %in% confuse_xy_labs)
    plot$labels[index] <- NULL
    return (plot)
}

add_expand <- function(plot, expand, axis){
   expand <- convert_expand(expand=expand)
   var <- paste0("panel_scales_", axis)
   gb <- ggplot_build(plot)
   scales_axis_obj <- gb$layout[[var]][[1]]
   scales_axis_obj$expand <- expand
   plot <- suppressWarnings(plot + scales_axis_obj)
   return(plot)
}

convert_expand <- function(expand){
   if (!is.numeric(expand) && !expand){
       expand <- c(0, 0)
   }
   if (!is.numeric(expand) && expand){
       expand <- ggplot2::waiver()
   }
   return(expand)
}
