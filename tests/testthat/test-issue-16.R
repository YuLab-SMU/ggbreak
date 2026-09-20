## #16: a break is drawn by clipping, not by cutting the data.  Every window is
## handed the whole dataset and each panel hides the part that belongs to the
## other windows, but the hidden geometry is still handed to the device and a
## vector device writes it into the file.  A bar from 0 to 990 is therefore a
## rectangle as tall as the unbroken axis in *both* windows, and the window that
## shows its top carries a rectangle reaching about 1.8 panel heights above the
## top of the panel it is drawn in.  Illustrator refuses to paste such a file
## ("the requested transformation would make some objects too large"), which is
## what the reporter saw next to the clip paths.
##
## `clamp_panel_geometry()` cuts that geometry back to the panel.  Nothing is
## drawn differently -- the panel clips the hidden part away -- so the tests
## below pin down both halves: the geometry handed to the device is bounded, and
## the part of it that *is* visible does not move.

library(grid)
library(ggplot2)
library(ggbreak)

## Every piece of geometry a panel can hide, as the ends of the slab it draws
## along the y axis, in panel heights (a panel is `[0, 1]`).  One grob can hold
## several slabs, so the ends are kept as vectors.
##
##  * a `rect` is a slab when it is read with the anchor it was given -- ggplot2
##    anchors a bar at its top left corner, so `y` is the top edge
##  * a `segments` grob is a slab where it runs along the axis
##  * a `polygon` is a slab where its vertices take only two levels of the axis
##    (a rectangle drawn as a polygon); a ribbon or an area takes more
slabs_of <- function(g, what = c("rect", "segments", "polygon")) {
    out <- list()
    add <- function(lo, hi) out[[length(out) + 1]] <<- data.frame(lo = lo, hi = hi)
    walk <- function(x) {
        if (inherits(x, "rect") && "rect" %in% what) {
            pos <- ggbreak:::native_coords(x$y)
            size <- ggbreak:::native_coords(x$height)
            if (!is.null(pos) && !is.null(size)) {
                side <- ggbreak:::just_side(x$just, 2L)
                ends <- if (side == "top") cbind(pos - size, pos)
                        else if (side == "bottom") cbind(pos, pos + size)
                        else cbind(pos - size / 2, pos + size / 2)
                add(apply(ends, 1, min), apply(ends, 1, max))
            }
        }
        if (inherits(x, "segments") && "segments" %in% what) {
            y0 <- ggbreak:::native_coords(x$y0)
            y1 <- ggbreak:::native_coords(x$y1)
            x0 <- ggbreak:::native_coords(x$x0)
            x1 <- ggbreak:::native_coords(x$x1)
            if (!is.null(y0) && !is.null(y1) && !is.null(x0) && !is.null(x1)) {
                runs <- abs(x0 - x1) < 1e-9 | abs(y0 - y1) < 1e-9
                if (any(runs)) add(pmin(y0, y1)[runs], pmax(y0, y1)[runs])
            }
        }
        if (inherits(x, "polygon") && "polygon" %in% what) {
            y <- ggbreak:::native_coords(x$y)
            if (!is.null(y)) {
                for (group in ggbreak:::polyline_groups(x)) {
                    levels <- unique(y[group])
                    if (length(levels) <= 2L) add(min(levels), max(levels))
                }
            }
        }
        if (!is.null(x$children)) for (ch in x$children) walk(ch)
        if (!is.null(x$grobs)) for (ch in x$grobs) walk(ch)
    }
    walk(g)
    if (!length(out)) return(data.frame(lo = numeric(0), hi = numeric(0)))
    do.call(rbind, out)
}

## the figure as the drawing path hands it to a device
device_gtable <- function(p) ggplotGrob(p)

## the same figure with the geometry left as it is
plain_gtable <- function(p) ggplotGrob(grid.draw(p, recording = FALSE))

## a bar that runs from far below the break to above it
bar_plot <- function() {
    ggplot(data.frame(g = c("Pre", "Post"), v = c(220, 990)), aes(g, v)) +
        geom_col(fill = "steelblue")
}

## a boxplot whose whiskers and box reach across the break
box_plot <- function() {
    set.seed(1)
    dat <- data.frame(g = rep(c("A", "B"), each = 20),
                      v = c(rnorm(20, 5), rnorm(20, 900)))
    ggplot(dat, aes(g, v)) + geom_boxplot() + scale_y_break(c(20, 800))
}

test_that('the geometry a broken plot hands to a device stays inside its panels (#16)', {
    for (p in list(bar_plot() + scale_y_break(c(500, 750)),
                   bar_plot() + scale_y_cut(500),
                   box_plot())) {
        slabs <- slabs_of(device_gtable(p))
        expect_gt(nrow(slabs), 0)
        expect_true(all(slabs$lo >= -0.051))
        expect_true(all(slabs$hi <= 1.051))
    }
})

test_that('a bar that crosses the break is a slab, not a bar of the unbroken axis (#16)', {
    p <- bar_plot() + scale_y_break(c(500, 750))
    before <- slabs_of(plain_gtable(p))
    after <- slabs_of(device_gtable(p))

    ## without the cut the tallest slab reaches far above the window it is in,
    ## which is the rectangle the reporter found in the file
    expect_gt(max(before$hi), 1.5)
    expect_lte(max(after$hi), 1.051)
})

test_that('a whisker and a box that cross the break are cut back too (#16)', {
    before <- slabs_of(plain_gtable(box_plot()))
    after <- slabs_of(device_gtable(box_plot()))

    expect_gt(nrow(before), 0)
    expect_gt(max(before$hi), 1.5)
    expect_true(all(after$hi <= 1.051))
    expect_true(all(after$lo >= -0.051))
})

test_that('cutting the hidden geometry back does not change what is visible (#16)', {
    p <- bar_plot() + scale_y_break(c(500, 750))
    gt <- plain_gtable(p)
    fixed <- ggbreak:::clamp_panel_geometry(gt)

    before <- slabs_of(gt)
    after <- slabs_of(fixed)
    expect_equal(dim(after), dim(before))

    ## what a panel shows is the part of a slab inside it, and that part has to
    ## be the same one before and after.  A slab outside the panel shows nothing
    ## in either figure, so it is normalised to nothing instead of being
    ## compared by coordinates that no one can see.
    visible <- function(lo, hi) {
        empty <- pmin(hi, 1) <= pmax(lo, 0)
        list(lo = ifelse(empty, 0, pmax(lo, 0)),
             hi = ifelse(empty, 0, pmin(hi, 1)))
    }
    expect_equal(visible(after$lo, after$hi), visible(before$lo, before$hi))
})

test_that('a figure with nothing hidden is handed over untouched (#16)', {
    ## the cut has to be a no-op when every slab already lies inside its panel
    p <- ggplot(data.frame(x = 1:10, y = c(1:5, 11:15)), aes(x, y)) +
        geom_point() + scale_y_break(c(5.5, 10.5))
    gt <- plain_gtable(p)

    expect_identical(ggbreak:::clamp_panel_geometry(gt), gt)
})

test_that('a line keeps the geometry the break hides (#16, #33)', {
    ## the piece of a line inside the break interval is not drawn by a panel, it
    ## is drawn in the gap between them and read off the grob, so the
    ## out-of-panel coordinates of a path have to stay where they are
    p <- ggplot(data.frame(x = c(1, 2), y = c(100, 900)), aes(x, y)) +
        geom_line() + scale_y_break(c(500, 750))
    gt <- plain_gtable(p)

    expect_identical(ggbreak:::clamp_panel_geometry(gt), gt)
})

test_that('a ribbon is left alone (#16)', {
    ## the boundary of a ribbon meets the edge of the panel at a point, and
    ## cutting the polygon back would move that point
    p <- ggplot(data.frame(x = 1:20, y = c(1:5, 20:24, 2:6, 21:25)),
                aes(x, y)) +
        geom_ribbon(aes(ymin = 0, ymax = y)) + scale_y_break(c(10, 18))
    gt <- plain_gtable(p)

    ## it is not a slab, so nothing in it is reported as one
    expect_equal(nrow(slabs_of(gt, "polygon")), 0)
})

test_that('a background is left alone (#16)', {
    ## the backgrounds of a plot are `npc`, the edges of a panel say nothing
    ## about them and they must not be read as panel geometry
    gt <- plain_gtable(bar_plot() + scale_y_break(c(500, 750)))
    fixed <- ggbreak:::clamp_panel_geometry(gt)

    npc_heights <- function(g) {
        out <- list()
        walk <- function(x) {
            if (inherits(x, "rect") && inherits(x$height, "unit") &&
                any(grid::unitType(x$height) == "npc")) {
                out[[length(out) + 1]] <<- x$height
            }
            if (!is.null(x$children)) for (ch in x$children) walk(ch)
            if (!is.null(x$grobs)) for (ch in x$grobs) walk(ch)
        }
        walk(g)
        out
    }
    before <- npc_heights(gt)
    skip_if(!length(before), "the plot has no npc background")

    ## the bars are cut back here, the backgrounds are not
    expect_false(identical(fixed, gt))
    expect_identical(npc_heights(fixed), before)
})

test_that('the clamp reads the anchor of a slab correctly (#16)', {
    ## ggplot2 anchors a rect at its top left corner, so `y` is the top edge.
    ## Reading it as the centre moves a bar into the panel instead of leaving it
    ## where it is, which is how the first attempt at this went wrong
    top <- grid::rectGrob(y = grid::unit(1.8, "native"),
                          height = grid::unit(1.0, "native"), just = c("left", "top"))
    ## the slab runs from 0.8 to 1.8, so only its top is above the panel
    cut <- ggbreak:::clamp_panel_geometry(top)
    expect_equal(as.numeric(cut$y), 1.05)
    expect_equal(as.numeric(cut$height), 0.25)

    outside <- grid::rectGrob(y = grid::unit(3.0, "native"),
                              height = grid::unit(1.0, "native"), just = c("left", "top"))
    cut <- ggbreak:::clamp_panel_geometry(outside)
    expect_equal(as.numeric(cut$y), 1.05)
    expect_equal(as.numeric(cut$height), 0)

    centre <- grid::rectGrob(y = grid::unit(0.5, "native"),
                             height = grid::unit(1.0, "native"), just = "centre")
    expect_identical(ggbreak:::clamp_panel_geometry(centre), centre)
})

test_that('a slanted segment is not touched (#16)', {
    ## cutting a slanted segment back moves the point where it enters the panel
    ## and changes its slope, so it is left alone
    slanted <- grid::segmentsGrob(x0 = grid::unit(0, "native"), x1 = grid::unit(1, "native"),
                                  y0 = grid::unit(-3, "native"), y1 = grid::unit(4, "native"))
    expect_identical(ggbreak:::clamp_panel_geometry(slanted), slanted)

    upright <- grid::segmentsGrob(x0 = grid::unit(0.5, "native"), x1 = grid::unit(0.5, "native"),
                                  y0 = grid::unit(-3, "native"), y1 = grid::unit(4, "native"))
    cut <- ggbreak:::clamp_panel_geometry(upright)
    expect_equal(as.numeric(cut$y0), -0.05)
    expect_equal(as.numeric(cut$y1), 1.05)
})
