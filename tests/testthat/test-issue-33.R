## #33: a line that crosses a break is not drawn in the break interval, because
## the break interval is what separates the two windows and there is no panel to
## draw in.  The line therefore stops at the edge of one window and starts again
## at the edge of the next, and the two ends are far apart along the broken axis,
## so a connected dots plot reads as two lines rather than one.
##
## `bridge = TRUE` draws the hidden piece in the blank space `space` opens
## between the windows.  A line grob keeps the geometry of the whole line and is
## only clipped when it is drawn, so where the line leaves one window and enters
## the next can be read off the grob.
##
## This is off by default, so the tests below also pin down that nothing moves
## unless it is asked for.

library(grid)
library(ggplot2)
library(ggbreak)

## the figure that carries the assembled windows, dug out of the grob tree of a
## broken plot; the outer gtable has a cell *called* `panel` as well, so the
## search starts inside it
assembled_gtable <- function(p) {
    outer <- ggplotGrob(grid.draw(p, recording = FALSE))
    panel <- outer$grobs[[which(outer$layout$name == "panel")[1]]]

    find <- function(grob) {
        if (inherits(grob, "gtable")) {
            if (sum(grepl("^panel-[0-9]+$", grob$layout$name)) >= 2) return(grob)
            for (ch in grob$grobs) {
                found <- find(ch)
                if (!is.null(found)) return(found)
            }
        } else if (inherits(grob, "gTree")) {
            for (ch in grob$children) {
                found <- find(ch)
                if (!is.null(found)) return(found)
            }
        }
        NULL
    }
    find(panel)
}

bridges_of <- function(p) {
    gt <- assembled_gtable(p)
    if (is.null(gt)) return(list())
    i <- grep("^ggbreak-bridge-", gt$layout$name)
    gt$grobs[i]
}

## a connected dots plot whose line runs from below the break to above it
crossing <- function() {
    dat <- data.frame(x = c(1, 2), y = c(100, 900))
    ggplot(dat, aes(x, y)) + geom_line() + geom_point()
}

test_that('a line that crosses the break is joined up when bridge = TRUE (#33)', {
    b <- bridges_of(crossing() + scale_y_break(c(500, 750), bridge = TRUE))

    expect_length(b, 1)
    expect_s3_class(b[[1]], "segments")
    ## the two ends sit on the two edges of the space between the windows, so
    ## the piece runs from the bottom of the space to the top of it
    expect_equal(as.numeric(b[[1]]$y0), 0)
    expect_equal(as.numeric(b[[1]]$y1), 1)
    ## and the ends are at the two crossings, which are apart
    expect_true(abs(as.numeric(b[[1]]$x1) - as.numeric(b[[1]]$x0)) > 0.05)
    expect_true(all(as.numeric(b[[1]]$x0) >= 0 & as.numeric(b[[1]]$x0) <= 1))
    expect_true(all(as.numeric(b[[1]]$x1) >= 0 & as.numeric(b[[1]]$x1) <= 1))
})

test_that('the bridge is off unless it is asked for (#33)', {
    expect_length(bridges_of(crossing() + scale_y_break(c(500, 750))), 0)
    ## and a plot that does not use the argument is not affected by it either
    expect_length(bridges_of(crossing() + scale_x_cut(breaks = c(1))), 0)
    expect_length(bridges_of(crossing() + scale_wrap(n = 2)), 0)
})

test_that('a break on the x axis is bridged along the other direction (#33)', {
    dat <- data.frame(x = seq(0, 2 * pi, 0.02))
    dat$y <- sin(dat$x)
    p <- ggplot(dat, aes(x, y)) + geom_line()

    b <- bridges_of(p + scale_x_break(c(1, 3), expand = FALSE, bridge = TRUE))

    expect_length(b, 1)
    expect_s3_class(b[[1]], "segments")
    ## the windows are side by side here, so the piece runs across the space
    ## from its left edge to its right edge
    expect_equal(as.numeric(b[[1]]$x0), 0)
    expect_equal(as.numeric(b[[1]]$x1), 1)
    expect_true(abs(as.numeric(b[[1]]$y1) - as.numeric(b[[1]]$y0)) > 0.05)
})

test_that('a line that does not cross the break gets no bridge (#33)', {
    ## one line below the break and one above it, so nothing is hidden
    dat <- data.frame(x = c(1, 2, 1, 2), y = c(100, 200, 800, 900),
                      g = rep(c("lo", "hi"), each = 2))
    p <- ggplot(dat, aes(x, y, group = g)) + geom_line()

    expect_length(bridges_of(p + scale_y_break(c(500, 750), bridge = TRUE)), 0)
})

test_that('the bridge is drawn like the line it belongs to (#33)', {
    dat <- data.frame(x = c(1, 2), y = c(100, 900))
    p <- ggplot(dat, aes(x, y)) + geom_line(colour = "red", linewidth = 2)

    gt <- assembled_gtable(p + scale_y_break(c(500, 750), bridge = TRUE))
    i <- grep("^ggbreak-bridge-", gt$layout$name)[1]

    ## walk the panels only: the axis of a plot is a polyline too
    line <- NULL
    walk <- function(g) {
        if (is.null(line) && inherits(g, "polyline") &&
            (is.null(g$name) || !grepl("panel.grid", g$name))) {
            line <<- g
        }
        if (inherits(g, "gtable")) {
            for (ch in g$grobs) walk(ch)
        } else if (inherits(g, "gTree")) {
            for (ch in g$children) walk(ch)
        }
    }
    for (j in which(grepl("^panel-[0-9]+$", gt$layout$name))) walk(gt$grobs[[j]])

    expect_false(is.null(line))
    expect_equal(gt$grobs[[i]]$gp$col, line$gp$col)
    expect_equal(gt$grobs[[i]]$gp$lwd, line$gp$lwd)
})

test_that('the argument is carried by the scale and reaches the drawing (#33)', {
    ## off by default
    expect_false(ggbreak:::extract_axis_break(
        attr(crossing() + scale_y_break(c(500, 750)), "axis_break_y"))$bridge)
    expect_true(ggbreak:::extract_axis_break(
        attr(crossing() + scale_y_break(c(500, 750), bridge = TRUE),
             "axis_break_y"))$bridge)
    expect_true(ggbreak:::extract_axis_break(
        attr(crossing() + scale_x_break(c(1, 3), bridge = TRUE),
             "axis_break_x"))$bridge)

    ## a plot with more than one break on the same axis still reports it
    two <- crossing() + scale_y_break(c(500, 750), bridge = TRUE) +
             scale_y_break(c(300, 400))
    expect_true(ggbreak:::extract_axis_break(attr(two, "axis_break_y"))$bridge)
})

test_that('a faceted plot along the broken axis is left alone (#33)', {
    ## there the windows are bound into a facet grid instead of being stacked,
    ## so the two ends of a line are not across from each other
    dat <- data.frame(x = c(1, 2, 1, 2), y = c(100, 900, 150, 850),
                      f = rep(c("a", "b"), each = 2))
    p <- ggplot(dat, aes(x, y)) + geom_line() + facet_grid(f ~ .)

    expect_silent(bridges_of(p + scale_y_break(c(500, 750), bridge = TRUE)))
})

test_that('a break on a discrete axis is left alone (#33)', {
    ## the levels of a window are subset, so its line stops at its own levels
    ## and never reaches the edge of the panel -- there is nothing to read the
    ## hidden piece off
    dat <- data.frame(x = factor(c("low", "medium", "high"),
                                 levels = c("low", "medium", "high")),
                      y = c(10, 20, 30))
    p <- ggplot(dat, aes(x, y, group = 1)) + geom_line() + geom_point()

    ## `suppressWarnings()` here for the same reason `print.ggbreak()` and the
    ## `ggplotGrob()` method use it: a window keeps only its own levels, so the
    ## rows of the other levels fall outside its scale and ggplot2 warns
    ## "Removed N rows containing missing values or values outside the scale
    ## range".  That is what a break *is* in this package.  `assembled_gtable()`
    ## reaches the figure through `grid.draw(..., recording = FALSE)`, the one
    ## entry point that does not wrap the drawing, so the call site has to.
    expect_length(suppressWarnings(
        bridges_of(p + scale_x_break(c("low", "medium"), bridge = TRUE))), 0)
})
