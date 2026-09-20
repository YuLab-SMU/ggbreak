## Every subplot gets a fresh `coord_cartesian()`/`coord_flip()` so that the axis
## being broken can be given its own window. That fresh coord would drop the
## limits the user set on the *other* axis, e.g. `coord_cartesian(xlim = c(5, 6))`
## together with `scale_y_break()`, #59. Return those limits so the subplots can
## pass them along again (`NULL` when the user set none).
other_axis_limits <- function(plot, axis) {
    lim <- tryCatch(plot$coordinates$limits, error = function(e) NULL)
    if (is.null(lim)) return(NULL)
    lim[[setdiff(c("x", "y"), axis)]]
}

## `patchwork` gives the assembled figure a background of its own, taken from the
## default theme.  That rectangle is opaque, so a broken plot hides whatever was
## drawn below it when it is superimposed on another plot (#52), and the
## `plot.background` the user set on the plot never reaches the figure either.
## Blank it here: the background of the figure belongs to the outer ggplot, which
## is the panel that carries the assembled figure, see `set_label()`.
##
## The assembled gtable of the nested path has already had the cell dropped by
## `slice_facet_gtable()`, and there is no patchwork object left to add a theme
## to, so it is returned unchanged.
blank_patch_background <- function(pg) {
    if (inherits(pg, "gtable")) return(pg)
    pg & ggplot2::theme(plot.background = ggplot2::element_blank())
}

## ---------------------------------------------------------------------------
## A broken axis and a facet grid nest in opposite directions.
##
## `ggbreak` draws one subplot per window, and the subplot of a faceted plot
## carries the whole facet grid, so the windows end up stacked *outside* the
## facets and every facet row is torn away from its other windows (#55, #17):
##
##     A w1          A w1
##     A w2    ->    A w2
##     B w1          B w1
##     B w2          B w2
##
## Cut every subplot along the facet grid and put the pieces back together with
## the facet row (column) as the outer loop.  Nothing below runs unless the plot
## is faceted along the direction the windows are stacked on, so a plot without
## facets is assembled exactly as before.

## where the panel cells sit along `dim`; more than one means the plot is
## faceted along that direction
facet_panel_positions <- function(g, dim) {
    lay <- g$layout
    pos <- if (dim == "row") lay$t else lay$l
    sort(unique(pos[grepl("^panel", lay$name)]))
}

## The index `ggplot2` gave this facet row (column), read off the axis that is
## drawn once per panel.  Which of the two indices of `panel-a-b` counts what
## depends on the layout, so it is not safe to guess it from the panel names.
facet_axis_index <- function(g, dim, k) {
    lay <- g$layout
    pos <- if (dim == "row") lay$t else lay$l
    prefix <- if (dim == "row") "^axis-l-" else "^axis-b-"
    m <- grepl(prefix, lay$name) & pos == facet_panel_positions(g, dim)[k]
    if (!any(m)) return(NULL)
    sub(prefix, "", lay$name[m][1])
}

## The cells of a gtable are picked by name, so the names of the cells that are
## kept have to be escaped before they are used as a pattern
escape_regex <- function(x) {
    for (ch in c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")) {
        x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
    }
    x
}

## one facet row (column) of a built gtable, as a gtable of its own
##
## The alternatives below carry no anchors of their own: they are wrapped in
## `^(...)$` by the caller, so a trailing `-` would be read as "the name ends
## here" and match nothing.
slice_facet_gtable <- function(g, dim, k) {
    lay <- g$layout
    n <- length(facet_panel_positions(g, dim))
    pos <- if (dim == "row") lay$t else lay$l
    panels <- lay$name[grepl("^panel", lay$name) & pos == facet_panel_positions(g, dim)[k]]
    pats <- escape_regex(panels)
    idx <- facet_axis_index(g, dim, k)
    if (!is.null(idx)) {
        ## a facet row draws its own y axis, a facet column its own x axis: the
        ## bottom one, not the top one, which is blank unless the user asked for
        ## a second axis there
        pats <- c(pats,
                  if (dim == "row") paste0("axis-[lr]-", idx) else paste0("axis-b-", idx),
                  if (dim == "row") paste0("strip-r-", idx) else paste0("strip-t-", idx))
    }
    ## the cells that run along the facet grid rather than across it belong to
    ## its first or last row (column), and the subplot theme has already blanked
    ## the ones the neighbouring subplot is supposed to draw instead
    ##
    ## `xlab-b` is not among them even though it is the one cell of the last
    ## facet row (column) that is missing otherwise: it spans the whole panel
    ## area, so keeping it also keeps the panel columns and the panel spacing of
    ## every *other* facet column alive, and the piece comes out with a gutter
    ## the width of all of them.  The axis title of the figure is drawn by the
    ## outer ggplot anyway, see `set_label()`.
    if (k == 1) {
        pats <- c(pats, if (dim == "row") c("strip-t-.*", "axis-t-.*") else c("axis-l-.*", "ylab-l"))
    }
    if (k == n) {
        pats <- c(pats, if (dim == "row") "axis-b-.*"
                             else c("axis-r-.*", "strip-r-.*", "ylab-r"))
    }
    keep <- grepl(paste0("^(", paste(pats, collapse = "|"), ")$"), lay$name)

    ## Drop the cells that are not kept instead of blanking them.  A blanked cell
    ## still sits in the layout, and the panel cells of the other facet rows
    ## (columns) would still be there to be found by name: the caller would size
    ## them along with the ones this piece draws, and the legend would be placed
    ## against a panel area that reaches across every facet.
    s <- g
    s$grobs <- s$grobs[keep]
    s$layout <- s$layout[keep, , drop = FALSE]
    ## collapse the rows and columns that kept nothing, so that a piece is only
    ## as wide (tall) as what it actually draws; the ones that are kept keep
    ## their index, which is what makes the pieces comparable
    cols <- unlist(Map(seq, lay$l[keep], lay$r[keep]))
    rows <- unlist(Map(seq, lay$t[keep], lay$b[keep]))
    ## A cell that runs across the whole panel area -- the axis title -- spans
    ## the panel cells of the other facet rows (columns) as well, and a panel
    ## cell is flexible: letting one through would claim a share of the figure
    ## for every facet row (column) there is instead of only for this one.
    rest <- facet_panel_positions(g, dim)[-k]
    if (dim == "row") {
        rows <- setdiff(rows, rest)
    } else {
        cols <- setdiff(cols, rest)
    }
    s$widths[setdiff(seq_along(s$widths), cols)] <- grid::unit(0, "mm")
    s$heights[setdiff(seq_along(s$heights), rows)] <- grid::unit(0, "mm")
    s
}

## the legend a built gtable draws, and where `ggplot2` put it
facet_guide_box <- function(g) {
    lay <- g$layout
    for (j in grep("^guide-box", lay$name)) {
        if (!inherits(g$grobs[[j]], "zeroGrob")) {
            return(list(grob = g$grobs[[j]],
                        position = sub("^guide-box-?", "", lay$name[j])))
        }
    }
    NULL
}

## Give the cell a piece draws its panel in the share of the figure that its
## window is entitled to.  `rbind()` and `cbind()` collect the flexible cells of
## every piece into one vector of `null` units, so the shares are read against
## each other and not against the piece they came from.
size_facet_piece <- function(g, dim, size) {
    panels <- grepl("^panel", g$layout$name)
    if (dim == "row") {
        g$heights[unique(g$layout$t[panels])] <- grid::unit(size, "null")
    } else {
        g$widths[unique(g$layout$l[panels])] <- grid::unit(size, "null")
    }
    g
}

## Returns NULL when there is nothing to nest, otherwise the assembled figure
## and the legend, which the caller has to put back by hand.
##
## The pieces are bound into one gtable rather than handed to `patchwork`.
## `as.ggplot(gtable)` is one opaque panel to `patchwork`: it aligns the boxes of
## the pieces but nothing inside them, so two pieces only line up if they happen
## to have the same inner sizes -- and forcing that with `unit.pmax()` would also
## force their flexible panel cells to the largest of them, which is what makes a
## window twice as wide as its neighbour.  `rbind()` and `cbind()` align the
## direction the pieces are *not* stacked along and leave the flexible cells
## flexible, so every window keeps the share of the figure that `sizes` gives it.
nest_facet_windows <- function(gglist, sizes, dim) {
    if (length(gglist) < 2L) return(NULL)
    ## Only a facet grid is cut apart.  It is the grid that gives a plot facet
    ## rows and facet columns for the windows to nest into; a `facet_wrap()` has
    ## neither, and it puts a strip above every single panel, so cutting it along
    ## the panels that happen to sit in a line would take a panel's strip away
    ## from it and leave it on its neighbour.
    if (!inherits(gglist[[1]]$facet, "FacetGrid")) return(NULL)
    gs <- lapply(gglist, function(p) tryCatch(ggplot2::ggplotGrob(p),
                                              error = function(e) NULL))
    if (any(vapply(gs, is.null, logical(1)))) return(NULL)
    n <- length(facet_panel_positions(gs[[1]], dim))
    if (n < 2L) return(NULL)

    pieces <- list()
    for (k in seq_len(n)) {
        for (j in seq_along(gs)) {
            piece <- slice_facet_gtable(gs[[j]], dim, k)
            pieces <- c(pieces, list(size_facet_piece(piece, dim, sizes[j])))
        }
    }
    ## the facet row (column) is the outer loop, so the pieces of one facet row
    ## (column) are bound next to each other and those of the next one follow
    bound <- Reduce(if (dim == "row") rbind else cbind, pieces)

    list(plot = bound, guide = facet_guide_box(gs[[1]]))
}

## `patchwork` cannot collect the legend of a reassembled figure: the pieces are
## single grobs, so they carry no guides to collect and `guides = "collect"`
## silently draws nothing.  Put the legend back where the plot asked for it, in a
## cell named `guide-box` so that `hoist_bottom_axis_title()` still finds it.
##
## `pg` is either the patchwork object of the ordinary path or the gtable that
## `nest_facet_windows()` builds; the two are reduced to the same thing here.
add_facet_guide_box <- function(pg, guide) {
    pgt <- if (inherits(pg, "gtable")) pg else patchwork::patchworkGrob(pg)
    if (is.null(guide) || !guide$position %in% c("right", "left", "top", "bottom")) {
        return(pgt)
    }
    lay <- pgt$layout
    panels <- grepl("^panel", lay$name)
    if (!any(panels)) return(pgt)
    top <- min(lay$t[panels]); bottom <- max(lay$b[panels])
    left <- min(lay$l[panels]); right <- max(lay$r[panels])

    if (guide$position %in% c("right", "left")) {
        size <- grid::convertWidth(grid::grobWidth(guide$grob), "mm", valueOnly = TRUE)
        pgt <- gtable::gtable_add_cols(
            pgt, grid::unit(size, "mm"),
            pos = if (guide$position == "right") -1 else 0)
        col <- if (guide$position == "right") ncol(pgt) else 1
        pgt <- gtable::gtable_add_grob(pgt, guide$grob, t = top, b = bottom,
                                       l = col, r = col, clip = "off",
                                       name = "guide-box")
    } else {
        size <- grid::convertHeight(grid::grobHeight(guide$grob), "mm", valueOnly = TRUE)
        pgt <- gtable::gtable_add_rows(
            pgt, grid::unit(size, "mm"),
            pos = if (guide$position == "bottom") -1 else 0)
        row <- if (guide$position == "bottom") nrow(pgt) else 1
        pgt <- gtable::gtable_add_grob(pgt, guide$grob, t = row, b = row,
                                       l = left, r = right, clip = "off",
                                       name = "guide-box")
    }
    pgt
}

## `aplot::plot_list(guides = "collect")` hands the collected legend to
## `patchwork`, which draws it at the bottom of the assembled figure.  That
## figure becomes the *panel* of the outer ggplot returned by
## `ggplotify::as.ggplot()`, while the axis title is added to that outer ggplot
## with `xlab()`.  The title therefore ends up below the legend, whereas
## `ggplot2` draws it between the panel and the legend, see #53.
bottom_legend <- function(plot) {
    pos <- tryCatch(plot$theme$legend.position, error = function(e) NULL)
    is.character(pos) && length(pos) == 1L && pos == "bottom"
}

## The title grob the outer ggplot would have drawn in its `xlab-b` row.  It is
## always rendered with `axis.title.x`, whichever aesthetic the label comes
## from: `coord_flip()` only swaps the labels themselves.
bottom_axis_title <- function(label, plot) {
    ggplot2::element_render(ggplot2::theme_get() + plot$theme,
                            "axis.title.x", label)
}

## Give the bottom axis title a row of its own in the assembled figure, just
## above the legend that `patchwork` collects there, and drop it from the outer
## labs, see #53.  Appending it with `patchwork::wrap_elements()` instead would
## not do: a wrapped patch always puts its grob in a `1null` row, which stretches
## the title over the space that belongs to the panels.
##
## Returns the figure together with the label that is left for the outer ggplot,
## so a caller that does not have to move anything can pass both on unchanged.
hoist_bottom_axis_title <- function(pg, plot, label) {
    if (is.null(label) || !bottom_legend(plot)) {
        return(list(plot = pg, label = label))
    }
    pgt <- if (inherits(pg, "gtable")) pg else patchwork::patchworkGrob(pg)
    i <- which(pgt$layout$name == "guide-box")
    if (!length(i)) {
        return(list(plot = pg, label = label))
    }
    title <- bottom_axis_title(label, plot)
    row <- pgt$layout$t[i[1]]
    height <- grid::convertHeight(grid::grobHeight(title), "mm",
                                  valueOnly = TRUE)
    pgt <- gtable::gtable_add_rows(pgt, heights = grid::unit(height, "mm"),
                                   pos = row - 1)
    pgt <- gtable::gtable_add_grob(pgt, title, t = row, l = pgt$layout$l[i[1]],
                                   clip = "off", name = "ggbreak-axis-title")
    list(plot = pgt, label = NULL)
}

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
                             (if (axis == 'y') theme(axis.text.x.top = element_blank(),
                                                      axis.ticks.x.top = element_blank(),
                                                      axis.line.x.top = element_blank(),
                                                      axis.title.x.top = element_blank()) else theme()) +
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
        }else if (has_secondary_axis(plot$scales$scales[[another.scaleind]])){
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

## `geom_text_repel()` and friends lay their labels out in the coordinates of the
## panel and keep them inside it, so a label whose point lies outside the window
## of a subplot is moved to the edge of that subplot instead of being clipped
## away.  ggbreak draws the same plot once per window, so every label ends up in
## every window and the ones of the other windows are piled up along the edge,
## see #35.  Clipping cannot remove them because ggrepel has already moved them
## inside the panel, but ggrepel leaves a label where it belongs if it is told
## that there is no limit; the panel clipping then drops the labels that do not
## belong to the window, exactly as it drops the points outside of it.
release_repel_labels <- function(plot, axis){
    limits <- paste0(axis, "lim")
    for (i in seq_along(plot$layers)){
        layer <- plot$layers[[i]]
        if (!any(grepl("Repel$", class(layer$geom)))){
            next
        }
        ## only touch a parameter that ggrepel really has and that the user has
        ## not set, so that an explicit `xlim`/`ylim` keeps its meaning
        if (!limits %in% names(layer$geom_params) ||
            !all(is.na(layer$geom_params[[limits]]))){
            next
        }
        layer <- copy_ggproto(layer)
        layer$geom_params[[limits]] <- c(-Inf, Inf)
        plot$layers[[i]] <- layer
    }
    return(plot)
}

## A ggproto object is an environment, so assigning a field of one also changes
## every plot that shares it.  Copying it with `ggplot2::ggproto(NULL, x)` does
## not work (the result cannot be drawn any more), so copy the environment and
## its attributes by hand.
copy_ggproto <- function(x){
    if (!is.environment(x)){
        return(x)
    }
    new <- new.env(parent = parent.env(x))
    for (name in ls(x, all.names = TRUE)){
        assign(name, get(name, envir = x, inherits = FALSE), envir = new)
    }
    attributes(new) <- attributes(x)
    return(new)
}

## `find_scale_index()` only reports the scales that the plot carries, so a plot
## which relies on the scale that `ggplot2` creates from the data has none, and
## adding a plain continuous scale then replaces that automatic one.  This
## changes the axis of anything that is not a plain number: a date or datetime
## axis is drawn as numbers (days or seconds since the epoch) afterwards.  Take
## the automatic scale from `ggplot_build()` instead and only override the parts
## that are asked for.  `ggplot_build()` clones the scales of the plot, so the
## object returned here can be modified without touching the input plot.
auto_axis_scale <- function(plot, axis, expand = NULL){
   var <- paste0("panel_scales_", axis)
   scale_obj <- ggplot_build(plot)$layout[[var]][[1]]
   ## building a plot whose coordinate is flipped moves the scale to the other
   ## side of the panel, and a scale that sits perpendicular to its own axis
   ## loses its guide when it is put back into a plot, so restore the position
   ## that the scale would have if it were created by `scale_x_continuous()` or
   ## `scale_y_continuous()`
   scale_obj$position <- if (axis == "x") "bottom" else "left"
   if (!is.null(expand)){
       scale_obj$expand <- expand
   }
   return(scale_obj)
}

add_expand <- function(plot, expand, axis){
   expand <- convert_expand(expand=expand)
   plot <- suppressWarnings(plot + auto_axis_scale(plot, axis, expand))
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

## ---------------------------------------------------------------------------
## A line that crosses a break is drawn in both windows, but the part of it that
## lies in the break interval is not drawn at all: the windows are stacked, the
## break interval is what separates them, and there is no panel to draw in.
## The line therefore stops at the edge of one window and starts again at the
## edge of the next, and the two ends are far apart along the broken axis, so it
## reads as two lines rather than one (#33).
##
## The piece is drawn here, in the blank space `space` opens between the
## windows.  A line grob keeps the geometry of the whole line and is only
## clipped at draw time, so where it leaves one window and enters the next can
## be read off the grob, and the space between the windows is a cell of the
## assembled gtable that spans exactly the two edges.

## the line grobs of a panel, in the order the layers were added; the grid of
## the panel is a polyline too and is left out
bridge_lines <- function(grob) {
    res <- list()
    walk <- function(g) {
        if (inherits(g, "polyline") &&
            (is.null(g$name) || !grepl("panel.grid", g$name))) {
            res[[length(res) + 1]] <<- g
        }
        if (inherits(g, "gtable")) {
            for (ch in g$grobs) walk(ch)
        } else if (inherits(g, "gTree")) {
            for (ch in g$children) walk(ch)
        }
        res
    }
    walk(grob)
    res
}

## the point indices of a polyline grob, one vector per line it carries: a
## single grob holds every group of the layer, and the last point of one group
## must not be joined to the first point of the next
polyline_groups <- function(grob) {
    n <- length(as.numeric(grob$x))
    len <- if (!is.null(grob$id.lengths)) {
        as.integer(grob$id.lengths)
    } else if (!is.null(grob$id)) {
        as.integer(rle(as.integer(grob$id))$lengths)
    } else {
        n
    }
    ## a grob whose groups do not account for every point is read as one line
    ## rather than dropped
    if (sum(len) != n) len <- n
    split(seq_len(n), rep(seq_along(len), len))
}

## where one line crosses `level`, given as a value of the *other* coordinate;
## a line that doubles back crosses it more than once
cross_within <- function(u, v, level) {
    out <- numeric(0)
    for (i in seq_len(length(u) - 1)) {
        u0 <- u[i]
        u1 <- u[i + 1]
        if (is.na(u0) || is.na(u1) || u0 == u1) next
        if ((u0 - level) * (u1 - level) <= 0) {
            out <- c(out, v[i] + (level - u0) / (u1 - u0) * (v[i + 1] - v[i]))
        }
    }
    ## a point that sits exactly on the level is reported by the segment before
    ## it and by the segment after it, and both give the same value
    unique(out)
}

## the crossings of every line of the grob, one vector per line
polyline_cross <- function(grob, axis, level) {
    x <- as.numeric(grob$x)
    y <- as.numeric(grob$y)
    u <- if (axis == "x") x else y
    v <- if (axis == "x") y else x
    lapply(polyline_groups(grob), function(i) cross_within(u[i], v[i], level))
}

## draw the hidden piece of every line that crosses a break, see #33
add_line_bridges <- function(pg, along) {
    gt <- if (inherits(pg, "gtable")) pg else patchwork::patchworkGrob(pg)
    panels <- which(grepl("^panel-[0-9]+$", gt$layout$name))
    if (length(panels) < 2) {
        return(pg)
    }
    ## `along` is the direction the windows are laid out in, which is the
    ## direction perpendicular to the axis that was broken
    pos <- if (along == "row") gt$layout$t[panels] else gt$layout$l[panels]
    panels <- panels[order(pos)]

    for (i in seq_len(length(panels) - 1)) {
        a <- gt$layout[panels[i], , drop = FALSE]
        b <- gt$layout[panels[i + 1], , drop = FALSE]
        if (along == "row") {
            ## the windows are stacked, so the upper one is left across its
            ## bottom edge and the lower one is entered across its top edge
            gap <- seq(a$b + 1, b$t - 1)
            if (!length(gap)) next
            axis <- "y"
            a_level <- 0
            b_level <- 1
            t <- min(gap); bb <- max(gap); l <- a$l; r <- a$r
        } else {
            gap <- seq(a$r + 1, b$l - 1)
            if (!length(gap)) next
            axis <- "x"
            a_level <- 1
            b_level <- 0
            t <- a$t; bb <- a$b; l <- min(gap); r <- max(gap)
        }

        a_lines <- bridge_lines(gt$grobs[[panels[i]]])
        b_lines <- bridge_lines(gt$grobs[[panels[i + 1]]])
        n <- min(length(a_lines), length(b_lines))
        if (n == 0) next

        for (k in seq_len(n)) {
            ca <- polyline_cross(a_lines[[k]], axis, a_level)
            cb <- polyline_cross(b_lines[[k]], axis, b_level)
            ## pair the crossings line by line: one line may cross the edge
            ## while another of the same layer does not
            for (gi in seq_len(min(length(ca), length(cb)))) {
                for (j in seq_len(min(length(ca[[gi]]), length(cb[[gi]])))) {
                    seg <- if (along == "row") {
                        grid::segmentsGrob(cb[[gi]][j], 0, ca[[gi]][j], 1,
                                           default.units = "npc",
                                           gp = a_lines[[k]]$gp)
                    } else {
                        grid::segmentsGrob(0, cb[[gi]][j], 1, ca[[gi]][j],
                                           default.units = "npc",
                                           gp = a_lines[[k]]$gp)
                    }
                    gt <- gtable::gtable_add_grob(
                        gt, seg, t = t, b = bb, l = l, r = r, clip = "off",
                        name = paste0("ggbreak-bridge-", i, "-", k, "-", gi, "-", j)
                    )
                }
            }
        }
    }
    gt
}

## A break is drawn by clipping, not by cutting the data: every window is handed
## the whole dataset and each panel hides the part that belongs to the other
## windows.  The hidden geometry is still handed to the device, and a vector
## device writes it into the file.  The bar plot of #16 is the clearest case --
## a bar from 0 to 990 is a rectangle as tall as the unbroken axis in *both*
## windows, and in the window that shows its top the rectangle reaches about 1.8
## panel heights above the top of the panel it is drawn in.  Illustrator refuses
## to paste such a file ("the requested transformation would make some objects
## too large"), which is what the reporter saw next to the clip paths.
##
## None of that geometry is ever visible, the panel clips it away, so it can be
## cut back to the panel without moving a single pixel.  Cutting it back in the
## *data* is not an option: dropping the row of a bar drops the whole bar, and
## cutting a line at the edge of the panel changes the slope of the segment that
## crosses it.  Only geometry that is a slab along the axis can be cut back
## safely, so this handles `rect` (a bar, a column, a tile, the box of a
## boxplot) and `segments` (a whisker, an error bar, the median of a boxplot).
## A path is deliberately left alone: the piece of a line that the break hides
## has to stay in the grob to be drawn in the gap between the windows, see
## `add_line_bridges()` and #33.
##
## `margin` is how far outside the panel a slab is still allowed to reach.  It
## only has to be wide enough for the border of a bar to fall outside the panel,
## so that the border is clipped away exactly as it was before.
clamp_panel_geometry <- function(g, margin = 0.05) {
    if (inherits(g, "rect")) {
        for (axis in list(list(pos = "x", size = "width", just = 1L),
                          list(pos = "y", size = "height", just = 2L))) {
            cut <- clamp_slab(g[[axis$pos]], g[[axis$size]],
                              just_side(g$just, axis$just), margin)
            if (is.null(cut)) next
            g[[axis$pos]] <- cut$pos
            g[[axis$size]] <- cut$size
        }
        return(g)
    }
    if (inherits(g, "segments")) {
        return(clamp_segment_grob(g, margin))
    }
    if (inherits(g, "polygon")) {
        return(clamp_polygon_grob(g, margin))
    }
    if (!is.null(g$children)) {
        g$children <- map_grobs(g$children, margin)
    }
    if (!is.null(g$grobs)) {
        g$grobs <- map_grobs(g$grobs, margin)
    }
    g
}

## `lapply()` drops the class of a `gList`, which makes the whole grob a
## different object even when nothing inside it changed, so the attributes of
## the list are put back
map_grobs <- function(kids, margin) {
    out <- lapply(kids, clamp_panel_geometry, margin = margin)
    attributes(out) <- attributes(kids)
    out
}

## one side of a `just`, the way `grid` reads it: a single value applies to both
## axes, otherwise the first is horizontal and the second vertical
just_side <- function(just, which) {
    if (is.null(just)) return("centre")
    just <- as.character(just)
    if (length(just) == 1L) just else just[[which]]
}

## the two ends of a slab, from the `pos`/`size` pair a grob keeps
slab_ends <- function(pos, size, side) {
    switch(side,
           centre = list(lo = pos - size / 2, hi = pos + size / 2),
           left = , bottom = list(lo = pos, hi = pos + size),
           list(lo = pos - size, hi = pos))
}

## and back again
slab_restore <- function(lo, hi, side) {
    switch(side,
           centre = list(pos = (lo + hi) / 2, size = hi - lo),
           left = , bottom = list(pos = lo, size = hi - lo),
           list(pos = hi, size = hi - lo))
}

## `pos`/`size` of a slab cut back to `[-margin, 1 + margin]`, or NULL when
## there is nothing to cut.  Only `native` coordinates can be compared against
## the edges of a panel -- a background is `npc` and is left alone -- and only
## finite ones can be compared at all.
clamp_slab <- function(pos, size, side, margin) {
    pos <- native_coords(pos)
    size <- native_coords(size)
    if (is.null(pos) || is.null(size)) return(NULL)
    ends <- slab_ends(pos, size, side)
    ## a slab that lies beyond the panel altogether is pulled back to the margin,
    ## so that nothing is left with a coordinate far outside the panel; it stays
    ## outside it, and the panel clips it away just as it did before
    lo <- pmin(pmax(ends$lo, -margin), 1 + margin)
    hi <- pmax(pmin(ends$hi, 1 + margin), lo)
    if (isTRUE(all.equal(lo, ends$lo)) && isTRUE(all.equal(hi, ends$hi))) return(NULL)
    cut <- slab_restore(lo, hi, side)
    list(pos = grid::unit(cut$pos, "native"), size = grid::unit(cut$size, "native"))
}

## Cut a segment back only when it already runs along an axis.  Shortening a
## vertical segment keeps it vertical and keeps the piece of it that is inside
## the panel, so nothing moves; the same holds for a horizontal one, and moving
## one of them sideways keeps it outside the panel.  A slanted segment is left
## alone, cutting it would move the point where it enters the panel and change
## its slope.
clamp_segment_grob <- function(g, margin) {
    x0 <- native_coords(g$x0)
    x1 <- native_coords(g$x1)
    y0 <- native_coords(g$y0)
    y1 <- native_coords(g$y1)
    if (is.null(x0) || is.null(x1) || is.null(y0) || is.null(y1)) return(g)

    runs <- abs(x0 - x1) < 1e-9 | abs(y0 - y1) < 1e-9
    if (!any(runs)) return(g)

    cut <- clamp_segment_ends(x0, x1, runs, margin)
    if (!is.null(cut)) {
        g$x0 <- grid::unit(cut$first, "native")
        g$x1 <- grid::unit(cut$second, "native")
    }
    cut <- clamp_segment_ends(y0, y1, runs, margin)
    if (!is.null(cut)) {
        g$y0 <- grid::unit(cut$first, "native")
        g$y1 <- grid::unit(cut$second, "native")
    }
    g
}

## the two ends of a segment cut back to `[-margin, 1 + margin]`, or NULL when
## the ends that may be cut are already inside it.  Only the segments marked in
## `runs` are cut.
clamp_segment_ends <- function(first, second, runs, margin) {
    lo <- pmin(pmax(pmin(first, second), -margin), 1 + margin)
    hi <- pmax(pmin(pmax(first, second), 1 + margin), lo)
    cut <- runs & (abs(pmax(first, second) - hi) > 1e-9 |
                   abs(pmin(first, second) - lo) > 1e-9)
    if (!any(cut)) return(NULL)
    low <- pmin(first, second) == first
    list(first = ifelse(cut & low, lo, ifelse(cut, hi, first)),
         second = ifelse(cut & low, hi, ifelse(cut, lo, second)))
}

## the numbers of a grob field that is in `native` units, or NULL when it is not
## a unit, is in another unit, or holds a value that cannot be compared
native_coords <- function(u) {
    if (is.null(u) || !inherits(u, "unit")) return(NULL)
    if (!all(grid::unitType(u) == "native")) return(NULL)
    u <- as.numeric(u)
    if (!length(u) || !all(is.finite(u))) return(NULL)
    u
}

## A polygon can be cut back when it is a slab along the axis: every vertex of it
## lies on one of two levels, so it covers one interval of the axis and nothing
## else.  That is what a rectangle drawn as a polygon is, the box of a boxplot
## among others, and cutting the interval back keeps the part of it that is
## inside the panel.  A polygon whose vertices take other values -- a ribbon, an
## area -- is left alone: there the boundary meets the edge of the panel at a
## point that cutting would move.
clamp_polygon_grob <- function(g, margin) {
    for (axis in c("y", "x")) {
        values <- native_coords(g[[axis]])
        if (is.null(values)) next
        touched <- FALSE
        for (group in polyline_groups(g)) {
            levels <- unique(values[group])
            if (length(levels) > 2L) next
            cut <- clamp_levels(levels, margin)
            if (is.null(cut)) next
            values[group] <- cut[match(values[group], levels)]
            touched <- TRUE
        }
        if (touched) g[[axis]] <- grid::unit(values, "native")
    }
    g
}

## the one or two levels of a slab cut back to `[-margin, 1 + margin]`, in the
## order they came in, or NULL when they are already inside it
clamp_levels <- function(levels, margin) {
    lo <- pmin(pmax(min(levels), -margin), 1 + margin)
    hi <- pmax(pmin(max(levels), 1 + margin), lo)
    if (isTRUE(all.equal(lo, min(levels))) && isTRUE(all.equal(hi, max(levels)))) {
        return(NULL)
    }
    if (length(levels) == 1L) return(lo)
    if (levels[1] < levels[2]) c(lo, hi) else c(hi, lo)
}

## The assembled figure as it goes to a device: the gtable of the outer ggplot
## with the geometry its panels hide cut back, see `clamp_panel_geometry()`.
## Every drawing path goes through here, so `print()`, `grid.draw()` and
## `ggsave()` all write the trimmed geometry and none of them draws anything
## differently.
drawable_grob <- function(g) {
    clamp_panel_geometry(ggplot2::ggplotGrob(g))
}
