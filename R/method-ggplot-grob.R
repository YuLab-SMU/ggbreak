## A broken plot is not drawn by `ggplot2`.  `ggbreak` assembles the windows and
## draws them itself, so the break is applied by `grid.draw()` and by `print()`
## and nowhere else.  `patchwork` and `cowplot` go through neither of them: they
## call `ggplotGrob()`, which is `ggplot_gtable(ggplot_build(x))`, and for a
## `ggbreak` object that builds the plot *without* the break, because the break
## is kept in an attribute that only `grid.draw.ggbreak()` ever reads.  A broken
## plot combined with other plots therefore came out with no break at all and
## without a warning, see #46.
##
## `ggplotGrob()` itself cannot be intercepted, it is not a generic, but the two
## functions it is made of are.  `ggplot_build()` dispatches on the class of the
## *plot* and `ggplot_gtable()` on the class of the *built* object, so the built
## object is tagged below and picked up by the method that follows.

#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build ggbreak
#' @export
ggplot_build.ggbreak <- function(plot, ...) {
    mark_ggbreak_built(NextMethod(), plot)
}

#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build ggwrap
#' @export
ggplot_build.ggwrap <- function(plot, ...) {
    mark_ggbreak_built(NextMethod(), plot)
}

#' @importFrom ggplot2 ggplot_build
#' @method ggplot_build ggcut
#' @export
ggplot_build.ggcut <- function(plot, ...) {
    mark_ggbreak_built(NextMethod(), plot)
}

## the class is what `ggplot_gtable()` dispatches on, the attribute carries the
## plot itself because the built object does not remember the break
mark_ggbreak_built <- function(built, plot) {
    attr(built, "ggbreak_plot") <- plot
    class(built) <- c("ggbreak_built", class(built))
    built
}

#' @importFrom ggplot2 ggplot_gtable
#' @method ggplot_gtable ggbreak_built
#' @export
ggplot_gtable.ggbreak_built <- function(data) {
    ## `grid.draw()` hands back the outer ggplot that carries the assembled
    ## figure rather than the figure itself, so it is converted here
    ##
    ## the drawing is done under `suppressWarnings()` for the same reason
    ## `print.ggbreak()` does it: `aplot::all_ggplot()` still calls the
    ## deprecated `is.ggplot()`, and that warning used to be swallowed by the
    ## `print()` path and would show up here instead
    outer <- ggplot2::ggplotGrob(
        suppressWarnings(grid::grid.draw(attr(data, "ggbreak_plot"), recording = FALSE))
    )
    ## a plot that is handed to `patchwork` or `cowplot` goes to the device the
    ## same way, so the geometry its panels hide is cut back here as well, #16
    clamp_panel_geometry(align_assembled_figure(outer))
}

## The figure that carries the assembled windows, dug out of the grob tree of a
## panel.  `ggplotify::as.ggplot()` does not put the gtable in the panel as it
## is, it is wrapped in a `gTree` a few levels down.
find_assembled_gtable <- function(grob) {
    if (inherits(grob, "gtable")) {
        if (any(grepl("^panel", grob$layout$name))) return(grob)
        for (child in grob$grobs) {
            found <- find_assembled_gtable(child)
            if (!is.null(found)) return(found)
        }
    } else if (inherits(grob, "gTree")) {
        for (child in grob$children) {
            found <- find_assembled_gtable(child)
            if (!is.null(found)) return(found)
        }
    }
    NULL
}

## the rows and columns of `g` that are covered by the cells named in `pattern`,
## restricted to the ones on the `side` of `ref` asked for
covered_cells <- function(g, pattern, axis, ref, side) {
    cells <- g$layout[grepl(pattern, g$layout$name), , drop = FALSE]
    if (!nrow(cells)) return(integer(0))
    pos <- if (axis == "col") cells$l else cells$t
    end <- if (axis == "col") cells$r else cells$b
    keep <- if (side == "before") end < ref else pos > ref
    cells <- cells[keep, , drop = FALSE]
    if (!nrow(cells)) return(integer(0))
    if (axis == "col") {
        sort(unique(unlist(Map(seq, cells$l, cells$r))))
    } else {
        sort(unique(unlist(Map(seq, cells$t, cells$b))))
    }
}

## a sub-gtable of `g` holding the cells that lie entirely inside `rows` x
## `cols`, with the sizes of those rows and columns kept
gtable_piece <- function(g, rows, cols) {
    keep <- g$layout$t %in% rows & g$layout$b %in% rows &
            g$layout$l %in% cols & g$layout$r %in% cols
    if (!any(keep)) return(NULL)
    piece <- gtable::gtable(widths = g$widths[cols], heights = g$heights[rows])
    piece$grobs <- g$grobs[keep]
    layout <- g$layout[keep, , drop = FALSE]
    layout$t <- match(layout$t, rows)
    layout$b <- match(layout$b, rows)
    layout$l <- match(layout$l, cols)
    layout$r <- match(layout$r, cols)
    piece$layout <- layout
    piece
}

## `patchwork` and `cowplot` align the plots of a layout by their `panel` cell.
## The assembled figure is one opaque panel that *contains* the axis of its
## windows, so a plot drawn next to it is aligned against a panel that already
## carries the axis labels of the figure and comes out wider than the plot it
## sits beside, see #46.
##
## The axis of the assembled figure is moved out of the panel here, so that the
## panel is the plot area alone and the labels sit in the cell `patchwork`
## expects an axis to be in.  Nothing is drawn differently -- the same grobs end
## up in the same place -- so this is only applied to the gtable `ggplotGrob()`
## hands out and `print()` keeps drawing the figure the way it did.
align_assembled_figure <- function(outer) {
    index <- which(outer$layout$name == "panel")[1]
    if (is.na(index)) return(outer)
    inner <- find_assembled_gtable(outer$grobs[[index]])
    if (is.null(inner)) return(outer)

    panels <- inner$layout[grepl("^panel", inner$layout$name), , drop = FALSE]
    if (!nrow(panels)) return(outer)
    cols <- sort(unique(unlist(Map(seq, panels$l, panels$r))))
    rows <- sort(unique(unlist(Map(seq, panels$t, panels$b))))

    ## which side of the panel area each axis sits on, and which cell of the
    ## outer gtable it has to move to
    sides <- list(
        list(pattern = "^axis-l", axis = "col", side = "before", cell = "axis-l"),
        list(pattern = "^axis-r", axis = "col", side = "after",  cell = "axis-r"),
        list(pattern = "^axis-t", axis = "row", side = "before", cell = "axis-t"),
        list(pattern = "^axis-b", axis = "row", side = "after",  cell = "axis-b")
    )

    outer$grobs[[index]] <- gtable_piece(inner, rows, cols)

    for (side in sides) {
        ref <- if (side$axis == "col") {
            if (side$side == "before") min(cols) else max(cols)
        } else {
            if (side$side == "before") min(rows) else max(rows)
        }
        taken <- covered_cells(inner, side$pattern, side$axis, ref, side$side)
        if (!length(taken)) next
        target <- which(outer$layout$name == side$cell)[1]
        if (is.na(target)) next
        piece <- if (side$axis == "col") {
            gtable_piece(inner, rows, taken)
        } else {
            gtable_piece(inner, taken, cols)
        }
        if (is.null(piece)) next
        outer$grobs[[target]] <- piece
        ## the room the axis takes is part of the figure, so the cell that now
        ## holds it has to be as wide (or as tall) as it was inside the figure
        if (side$axis == "col") {
            outer$widths[outer$layout$l[target]] <- sum(inner$widths[taken])
        } else {
            outer$heights[outer$layout$t[target]] <- sum(inner$heights[taken])
        }
    }

    ## the title of the figure has to leave the panel as well, for a different
    ## reason than the axes: with `theme(legend.position = "bottom")`
    ## `hoist_bottom_axis_title()` gives it a row of its own *below* the panels,
    ## so that the collected legend cannot be drawn on top of it, and that row is
    ## outside the panel area `rows` spans -- `gtable_piece()` keeps a cell only
    ## when both of its rows lie in them -- so the piece taken above left the
    ## title behind and a broken plot handed to `patchwork` or `cowplot` came out
    ## without it.  The row cannot simply be added to `rows` either: everything
    ## between the outermost panels is the plot area as far as the layout is
    ## concerned, so a plot aligned next to this one would be sized against the
    ## title too.  `xlab-b` is the cell `ggplot2` itself keeps the bottom title
    ## in, so the piece moves there and the cell takes the height the row had
    ## inside the figure, the same bookkeeping the axis cells get above.  The
    ## `guide-box` is deliberately not lifted along with it: this path drops the
    ## collected legend too, which is a separate defect and only must not be made
    ## worse here, #85.
    i <- which(inner$layout$name == "ggbreak-axis-title")[1]
    target <- which(outer$layout$name == "xlab-b")[1]
    if (!is.na(i) && !is.na(target)) {
        r <- seq(inner$layout$t[i], inner$layout$b[i])
        cc <- seq(inner$layout$l[i], inner$layout$r[i])
        piece <- gtable_piece(inner, r, cc)
        if (!is.null(piece)) {
            outer$grobs[[target]] <- piece
            outer$heights[outer$layout$t[target]] <- sum(inner$heights[r])
        }
    }
    outer
}
