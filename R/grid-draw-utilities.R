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
    index <- confuse_xy_labs[confuse_xy_labs %in% names(ggplot_build(plot)$plot$labels)]
    if (length(index) == 0){
       return(plot)
    }
    params <- lapply(seq(length(index)), function(i)NULL)
    names(params) <- index
    plot <- plot + do.call('labs', params)
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

## Theme for 2D grid cells (dual-axis breaks)
## col_type: "first" (left), "other" (middle), "last" (right) — controls y-axis visibility
## row_type: "first" (bottom), "other" (middle), "last" (top) — controls x-axis visibility
subplot_theme_2d <- function(plot, col_type, row_type, margin_x, margin_y, rev_x, rev_y) {
    te <- theme()

    # Y-axis visibility: hide for non-first columns
    if (col_type %in% c("other", "last")) {
        te <- te + axis_theme(plot, axis = "x")
    }

    # X-axis visibility: hide for non-bottom rows
    if (row_type %in% c("other", "last")) {
        te <- te + axis_theme(plot, axis = "y")
    }

    # Y-strips: show only in last column (rightmost)
    if (col_type != "last") {
        te <- te + strip_theme(plot, axis = "x")
    }

    # X-strips: show only in last row (topmost = "last" type)
    if (row_type != "last") {
        te <- te + strip_theme(plot, axis = "y")
    }

    # Combined margins from both axes
    l <- if (col_type == "first") 0 else margin_x / 2
    r <- if (col_type == "last")  0 else margin_x / 2
    t <- if (row_type == "last")  0 else margin_y / 2
    b <- if (row_type == "first") 0 else margin_y / 2

    if (rev_x == "reverse") { tmp <- l; l <- r; r <- tmp }
    if (rev_y == "reverse") { tmp <- t; t <- b; b <- tmp }

    te <- te + theme(plot.margin = margin(t = t, r = r, b = b, l = l, unit = "cm"))

    # Handle panel border for boxed themes (theme_bw, theme_linedraw, etc.)
    # Remove the full rectangle and draw lines only on outer-facing edges
    border_te <- .border_theme_2d(plot, col_type, row_type)
    if (!is.null(border_te)) {
        te <- te + border_te
    }

    return(te)
}

## Custom theme element: rectangle that draws only specified sides.
## Used for panel.border in 2D break grids to avoid doubled borders.
## Useful for for theme where there is a panel border (e.g. theme_bw, linedraw) and we want to avoid drawing borders on internal edges of the 2D grid.
element_partial_rect <- function(colour = "grey20", linewidth = 0.5,
                                 linetype = 1, sides = c("top", "bottom", "left", "right")) {
    structure(
        list(colour = colour, linewidth = linewidth, linetype = linetype,
             sides = sides, fill = NA, inherit.blank = FALSE),
        class = c("element_partial_rect", "element_rect", "element")
    )
}

#' @importFrom ggplot2 element_grob
#' @method element_grob element_partial_rect
#' @export
element_grob.element_partial_rect <- function(element, ...) {
    gp <- grid::gpar(
        col = element$colour,
        lwd = element$linewidth * 72.27 / 25.4,   # mm -> points
        lty = element$linetype
    )

    grobs <- list()
    sides <- element$sides
    if ("left" %in% sides)
        grobs <- c(grobs, list(grid::segmentsGrob(
            x0 = 0, y0 = 0, x1 = 0, y1 = 1, default.units = "npc", gp = gp)))
    if ("right" %in% sides)
        grobs <- c(grobs, list(grid::segmentsGrob(
            x0 = 1, y0 = 0, x1 = 1, y1 = 1, default.units = "npc", gp = gp)))
    if ("bottom" %in% sides)
        grobs <- c(grobs, list(grid::segmentsGrob(
            x0 = 0, y0 = 0, x1 = 1, y1 = 0, default.units = "npc", gp = gp)))
    if ("top" %in% sides)
        grobs <- c(grobs, list(grid::segmentsGrob(
            x0 = 0, y0 = 1, x1 = 1, y1 = 1, default.units = "npc", gp = gp)))

    if (length(grobs) == 0) return(grid::nullGrob())
    do.call(grid::grobTree, grobs)
}

## Replace panel.border rectangle with a partial-rect that draws only
## the outer-facing edges of each cell in the 2D grid.
.border_theme_2d <- function(plot, col_type, row_type) {
    # Resolve panel.border from plot theme + global default
    full_theme <- ggplot2::theme_get() + plot$theme
    border <- full_theme$panel.border
    if (is.null(border) || inherits(border, "element_blank")) {
        return(NULL)
    }

    # Extract border appearance
    border_colour <- if (!is.null(border$colour)) border$colour else "grey20"
    border_lw <- if (!is.null(border$linewidth)) border$linewidth
                 else if (!is.null(border$size)) border$size
                 else 0.5

    # Draw only outer-facing edges of the grid perimeter
    sides <- character(0)
    if (col_type == "first") sides <- c(sides, "left")
    if (col_type == "last")  sides <- c(sides, "right")
    if (row_type == "first") sides <- c(sides, "bottom")
    if (row_type == "last")  sides <- c(sides, "top")

    theme(
        panel.border = element_partial_rect(
            colour = border_colour, linewidth = border_lw, sides = sides
        )
    )
}
