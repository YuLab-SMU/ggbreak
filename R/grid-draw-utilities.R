subplot_theme <- function(plot, axis, type, margin = .2, rev, symbol = NULL){
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
    if (!is.null(symbol)) {
        border_te <- .symbol_theme(te, plot, axis, type, symbol, rev)
        if (!is.null(border_te)) {
            te <- te + border_te
        }
    }
    return(te)
}

.symbol_theme <- function(te, plot, axis, type, symbol, rev) {
    full_theme <- ggplot2::theme_get() + plot$theme + te
    border <- full_theme$panel.border
    
    # We should only draw symbols on the edges that actually have a border or axis line.
    # To keep it robust, we can use the `sides` parameter to specifically target `top`, `bottom`, `left`, `right`.
    # For 1D break, if it's an X-axis break, we typically only want symbols on the bottom (or top if there's an axis/border there).
    # Since `ggbreak` handles top/bottom via `first`/`last`/`other` for strips and axes,
    # let's assume we want to draw symbols on the broken edges.
    
    sides <- character(0)
    if (axis == "x") {
        # The break gap is vertical, so the symbols should be drawn on the top and bottom borders.
        # But we only want them on the side where the break happens.
        # Wait, if X is broken, the left panel's right edge is broken, and the right panel's left edge is broken.
        # So we want symbols on the top-right & bottom-right of the left panel,
        # and top-left & bottom-left of the right panel.
        if (type == "first") {
            sides <- if (rev == "reverse") "left" else "right"
        } else if (type == "last") {
            sides <- if (rev == "reverse") "right" else "left"
        } else if (type == "other") {
            sides <- c("left", "right")
        }
    } else if (axis == "y") {
        # The break gap is horizontal.
        if (type == "first") {
            sides <- if (rev == "reverse") "bottom" else "top"
        } else if (type == "last") {
            sides <- if (rev == "reverse") "top" else "bottom"
        } else if (type == "other") {
            sides <- c("top", "bottom")
        }
    }
    
    if (length(sides) > 0) {
        # By default, we will just pass these sides. 
        # But `element_break_symbol` should be smart enough to only draw where there is an actual border.
        # Actually, let's just pass `axis` so the grob knows if it's an X or Y break.
        return(theme(panel.border = element_break_symbol(symbol = symbol, sides = sides, base_border = border, axis = axis)))
    }
    return(NULL)
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
subplot_theme_2d <- function(plot, col_type, row_type, margin_x, margin_y, rev_x, rev_y, symbol_x = NULL, symbol_y = NULL) {
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

    # Add break symbols if requested
    if (!is.null(symbol_x) || !is.null(symbol_y)) {
        sym_te <- .symbol_theme_2d(te, plot, col_type, row_type, symbol_x, symbol_y, rev_x, rev_y)
        if (!is.null(sym_te)) {
            te <- te + sym_te
        }
    }

    return(te)
}

.symbol_theme_2d <- function(te, plot, col_type, row_type, symbol_x, symbol_y, rev_x, rev_y) {
    # Determine the current border from the composed theme or plot theme
    full_theme <- ggplot2::theme_get() + plot$theme + te
    border <- full_theme$panel.border

    sides_x <- character(0)
    if (!is.null(symbol_x)) {
        # Only add X symbols on the top/bottom outer edges, so we check row_type
        # For X-break (vertical gap), we want symbols on the top/bottom edges of the gap.
        # But in a 2D grid, the gap exists between columns.
        # So we want symbols on the top/bottom edges of the *plot*, at the column boundaries.
        # The column boundaries are between col_type="first" and "other", etc.
        # Wait, if we attach the symbol to the panel border, it will draw on the panel's edge.
        # If we attach to col="first", side="right", it draws on the right edge of the first column.
        # We only want to draw this IF this panel is at the top row or bottom row.
        
        if (row_type == "first" || row_type == "last") {
            # Only draw X-break symbols if we are in the top or bottom row
            if (col_type == "first") {
                sides_x <- if (rev_x == "reverse") "left" else "right"
            } else if (col_type == "last") {
                sides_x <- if (rev_x == "reverse") "right" else "left"
            } else if (col_type == "other") {
                sides_x <- c("left", "right")
            }
        }
    }

    sides_y <- character(0)
    if (!is.null(symbol_y)) {
        # Only add Y symbols on the left/right outer edges, so we check col_type
        if (col_type == "first" || col_type == "last") {
            # Only draw Y-break symbols if we are in the left or right column
            if (row_type == "first") {
                sides_y <- if (rev_y == "reverse") "bottom" else "top"
            } else if (row_type == "last") {
                sides_y <- if (rev_y == "reverse") "top" else "bottom"
            } else if (row_type == "other") {
                sides_y <- c("top", "bottom")
            }
        }
    }
    
    # However, `element_break_symbol` logic for "dual" axis currently draws on BOTH top and bottom corners
    # if `axis="dual"` or "x".
    # If we are in the top row (row_type="last"), we only want the TOP symbol if there is a border,
    # and maybe we don't want the bottom symbol because the bottom symbol is "inside" the plot?
    # No, for X-break, the gap is vertical. The symbol should appear on the X-axis line.
    # In a 2D grid, the X-axis is usually at the bottom of the bottom row.
    # So for `row_type="first"` (bottom row), we want the symbol on the BOTTOM edge.
    # For `row_type="last"` (top row), we generally don't have an X-axis unless it's `scale_x_top`.
    # But if there is a full border (boxed), we might want it on the top edge of the top row too.
    # Crucially, we do NOT want symbols on the top edge of the bottom row, or bottom edge of the top row (internal edges).
    
    # We need to pass more info to `element_break_symbol` or refine `sides`.
    # Let's refine `sides` to be very specific: "bottom-right", "top-right", etc?
    # `element_break_symbol` uses "right", "left", "top", "bottom" to choose which GROBS to generate.
    # If we pass "right", it generates both top-right and bottom-right slashes (if boxed) or just bottom-right (if not boxed).
    # But here we need finer control.
    # If row_type="first" (bottom), and we have an X-break on the right edge, we ONLY want the BOTTOM-right slash.
    # If row_type="last" (top), and we have an X-break on the right edge, we ONLY want the TOP-right slash (and only if boxed).
    
    # The current `element_break_symbol` implementation for `axis="dual"` or "x":
    # if "right" in sides: draws bottom-right. If has_border, also draws top-right.
    # This "automagic" behavior is causing the issue. It assumes if you are on the right edge, you want both corners if boxed.
    # But in the middle of the plot (internal edges), we might have a border but we don't want the internal corner symbol.
    
    # To fix this without changing `element_break_symbol` structure too much:
    # We can pass a new argument `corner_mask` or `draw_top`/`draw_bottom`.
    # Or, simpler:
    # We can define `sides` more granularly in `element_break_symbol`? No, it expects standard side names.
    
    # Let's modify `element_break_symbol` to accept `clip_corners` or similar?
    # Or just let `.symbol_theme_2d` decide exactly which corners.
    
    # Let's change the logic in `element_break_symbol` to strictly follow what we need.
    # Actually, we can use `axis="x_bottom"` or `axis="x_top"`?
    
    # Let's update `element_break_symbol` to handle `which` argument?
    # Or we can just be smart in `.symbol_theme_2d` and passing a custom `axis` string is a hacky but effective way.
    # e.g. axis="x_bottom" -> only draw bottom corners.
    
    # Let's refine `.symbol_theme_2d` logic:
    
    sides <- c()
    specific_axis <- "dual" # default
    
    # We need to separate X and Y symbol addition or combine them carefully.
    # Actually, we can return a list of theme elements or combine them.
    # But `panel.border` is unique.
    
    # Strategy:
    # 1. Calculate which specific corners need symbols.
    # 2. Pass this specific instruction to `element_break_symbol`.
    
    # Let's allow `element_break_symbol` to take a `corners` argument? 
    # e.g. corners = c("tr", "br", "tl", "bl")
    
    # Let's update `element_break_symbol` first (I will do this in the next step).
    # Here I will prepare the logic to determine corners.
    
    corners <- character(0)
    
    # Logic for X-breaks (vertical gaps between cols)
    if (!is.null(symbol_x)) {
        # We only care if we are in the bottom row (standard x-axis) or top row (if boxed).
        # We NEVER want symbols on the internal horizontal edges (top of bottom row, bottom of top row).
        
        is_bottom_row <- (row_type == "first")
        is_top_row <- (row_type == "last")
        
        if (is_bottom_row || is_top_row) {
            # Determine which vertical edge has the break
            edge <- NULL
            if (col_type == "first") edge <- if (rev_x == "reverse") "left" else "right"
            else if (col_type == "last") edge <- if (rev_x == "reverse") "right" else "left"
            else if (col_type == "other") edge <- c("left", "right")
            
            if (!is.null(edge)) {
                if ("right" %in% edge) {
                    if (is_bottom_row) corners <- c(corners, "x_bottom_right")
                    if (is_top_row) corners <- c(corners, "x_top_right")
                }
                if ("left" %in% edge) {
                    if (is_bottom_row) corners <- c(corners, "x_bottom_left")
                    if (is_top_row) corners <- c(corners, "x_top_left")
                }
            }
        }
    }
    
    # Logic for Y-breaks (horizontal gaps between rows)
    if (!is.null(symbol_y)) {
        # We only care if we are in the left col (standard y-axis) or right col (if boxed).
        
        is_left_col <- (col_type == "first")
        is_right_col <- (col_type == "last")
        
        if (is_left_col || is_right_col) {
            # Determine which horizontal edge has the break
            edge <- NULL
            if (row_type == "first") edge <- if (rev_y == "reverse") "bottom" else "top"
            else if (row_type == "last") edge <- if (rev_y == "reverse") "top" else "bottom"
            else if (row_type == "other") edge <- c("top", "bottom")
            
            if (!is.null(edge)) {
                if ("top" %in% edge) {
                    if (is_left_col) corners <- c(corners, "y_top_left")
                    if (is_right_col) corners <- c(corners, "y_top_right")
                }
                if ("bottom" %in% edge) {
                    if (is_left_col) corners <- c(corners, "y_bottom_left")
                    if (is_right_col) corners <- c(corners, "y_bottom_right")
                }
            }
        }
    }
    
    if (length(corners) > 0) {
        # Pass the calculated corners to the element
        symbol <- if (!is.null(symbol_x)) symbol_x else symbol_y
        return(theme(panel.border = element_break_symbol(symbol = symbol, corners = corners, base_border = border, axis = "dual_custom")))
    }
    return(NULL)
}

## Custom theme element: break symbol
element_break_symbol <- function(symbol = "slash", colour = "black", linewidth = 0.5,
                                 linetype = 1, sides = "right", base_border = NULL, axis = "x", corners = NULL) {
    structure(
        list(symbol = symbol, colour = colour, linewidth = linewidth, linetype = linetype,
             sides = sides, base_border = base_border, axis = axis, corners = corners, fill = NA, inherit.blank = FALSE),
        class = c("element_break_symbol", "element_rect", "element")
    )
}

#' @method element_grob element_break_symbol
#' @export
element_grob.element_break_symbol <- function(element, ...) {
    grobs <- list()
    has_border <- !is.null(element$base_border) && !inherits(element$base_border, "element_blank")
    if (has_border) {
        grobs <- c(grobs, list(ggplot2::element_grob(element$base_border, ...)))
    }
    
    gp <- grid::gpar(
        col = element$colour,
        lwd = element$linewidth * 72.27 / 25.4,   # mm -> points
        lty = element$linetype
    )
    
    sides <- element$sides
    symbol <- element$symbol
    axis <- element$axis
    corners <- element$corners
    
    if (symbol == "slash") {
        # Modified slash style: slant is adjusted to 0.5mm to be nearly vertical/horizontal
        slant <- grid::unit(0.6, "mm") 
        height <- grid::unit(3.5, "mm") # y displacement
        shift <- grid::unit(1.0, "mm")  # separation

        # Helper function to add a pair of slashes at a given corner
        add_slash <- function(x_npc, y_npc, orient = "vertical") {
            if (orient == "vertical") {
                # For X-break gap (vertical gap), the slash crosses horizontally
                list(
                    grid::segmentsGrob(
                        x0 = grid::unit(x_npc, "npc") - shift - slant, y0 = grid::unit(y_npc, "npc") - height,
                        x1 = grid::unit(x_npc, "npc") - shift + slant, y1 = grid::unit(y_npc, "npc") + height,
                        gp = gp),
                    grid::segmentsGrob(
                        x0 = grid::unit(x_npc, "npc") + shift - slant, y0 = grid::unit(y_npc, "npc") - height,
                        x1 = grid::unit(x_npc, "npc") + shift + slant, y1 = grid::unit(y_npc, "npc") + height,
                        gp = gp)
                )
            } else {
                # For Y-break gap (horizontal gap), the slash crosses vertically
                list(
                    grid::segmentsGrob(
                        x0 = grid::unit(x_npc, "npc") - height, y0 = grid::unit(y_npc, "npc") - shift - slant,
                        x1 = grid::unit(x_npc, "npc") + height, y1 = grid::unit(y_npc, "npc") - shift + slant,
                        gp = gp),
                    grid::segmentsGrob(
                        x0 = grid::unit(x_npc, "npc") - height, y0 = grid::unit(y_npc, "npc") + shift - slant,
                        x1 = grid::unit(x_npc, "npc") + height, y1 = grid::unit(y_npc, "npc") + shift + slant,
                        gp = gp)
                )
            }
        }

        # If "corners" are provided (Dual Axis 2D mode with precise control)
        if (!is.null(corners) && axis == "dual_custom") {
            # X-axis symbols (Vertical cut)
            if ("x_bottom_right" %in% corners) grobs <- c(grobs, add_slash(1, 0, "vertical"))
            if ("x_top_right" %in% corners) grobs <- c(grobs, add_slash(1, 1, "vertical"))
            if ("x_bottom_left" %in% corners) grobs <- c(grobs, add_slash(0, 0, "vertical"))
            if ("x_top_left" %in% corners) grobs <- c(grobs, add_slash(0, 1, "vertical"))
            
            # Y-axis symbols (Horizontal cut)
            if ("y_top_left" %in% corners) grobs <- c(grobs, add_slash(0, 1, "horizontal"))
            if ("y_top_right" %in% corners) grobs <- c(grobs, add_slash(1, 1, "horizontal"))
            if ("y_bottom_left" %in% corners) grobs <- c(grobs, add_slash(0, 0, "horizontal"))
            if ("y_bottom_right" %in% corners) grobs <- c(grobs, add_slash(1, 0, "horizontal"))
        } else {
            # Standard 1D behavior (fallback)
            if (axis %in% c("x", "dual")) {
                if ("right" %in% sides) {
                    grobs <- c(grobs, add_slash(1, 0, "vertical"))
                    if (has_border) grobs <- c(grobs, add_slash(1, 1, "vertical"))
                }
                if ("left" %in% sides) {
                    grobs <- c(grobs, add_slash(0, 0, "vertical"))
                    if (has_border) grobs <- c(grobs, add_slash(0, 1, "vertical"))
                }
            }
            
            if (axis %in% c("y", "dual")) {
                if ("top" %in% sides) {
                    grobs <- c(grobs, add_slash(0, 1, "horizontal"))
                    if (has_border) grobs <- c(grobs, add_slash(1, 1, "horizontal"))
                }
                if ("bottom" %in% sides) {
                    grobs <- c(grobs, add_slash(0, 0, "horizontal"))
                    if (has_border) grobs <- c(grobs, add_slash(1, 0, "horizontal"))
                }
            }
        }
    } else if (symbol == "zigzag") {
        # Optional
    }

    if (length(grobs) == 0) return(grid::nullGrob())
    do.call(grid::grobTree, grobs)
}
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
