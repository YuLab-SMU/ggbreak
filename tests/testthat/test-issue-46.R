## #46: a broken plot that is combined with other plots came out with no break at
## all.  `patchwork` and `cowplot` do not draw a plot the way `print()` does --
## they call `ggplotGrob()`, which is `ggplot_gtable(ggplot_build(x))` -- and
## neither of those knew about the break, so the plot was built *without* it and
## the break was dropped without a warning.  A broken plot is now built as a
## whole, so `ggplotGrob()` returns the assembled figure.
##
## That figure is one opaque panel to `patchwork`, which aligns the plots of a
## layout by their `panel` cell, so its own axis had to move out of the panel as
## well -- otherwise a plot drawn next to it is aligned against a panel that
## already carries the axis labels and comes out wider than the plot it sits
## beside, which is the misalignment the report is actually about.

library(grid)
library(ggplot2)
library(ggbreak)

## the assembled figure is made of one panel per window, so it holds far more
## grobs than the plain plot it was built from
count_grobs <- function(g) {
    n <- 0L
    walk <- function(x) {
        if (inherits(x, "gtable")) {
            n <<- n + length(x$grobs)
            lapply(x$grobs, walk)
        } else if (inherits(x, "gTree")) {
            n <<- n + length(x$children)
            lapply(x$children, walk)
        }
    }
    lapply(g$grobs, walk)
    n
}

test_that('`ggplotGrob()` of a broken plot draws the break, not the plain plot (#46)', {
    dat <- data.frame(x = 1:20, y = c(1:5, 21:25, 6:10, 26:30))
    p  <- ggplot(dat, aes(x, y)) + geom_point()
    pb <- p + scale_y_break(c(12, 20))

    expect_gt(count_grobs(ggplotGrob(pb)), count_grobs(ggplotGrob(p)))

    ## `ggplotGrob()` is not a generic, so the two functions it is made of are
    ## the ones that had to learn about the break, and the built object is the
    ## only thing `ggplot_gtable()` can dispatch on
    expect_s3_class(ggplotGrob(pb), "gtable")
    expect_silent(ggplotGrob(pb))
})

test_that('the built object of a broken plot still carries the plot (#46)', {
    dat <- data.frame(x = 1:20, y = c(1:5, 21:25, 6:10, 26:30))
    pb <- ggplot(dat, aes(x, y)) + geom_point() + scale_y_break(c(12, 20))

    built <- ggplot_build(pb)
    expect_s3_class(built, "ggbreak_built")
    expect_false(is.null(attr(built, 'ggbreak_plot')))
    expect_s3_class(ggplot_gtable(built), "gtable")
})

test_that('a wrapped and a cut plot are built as a whole as well (#46)', {
    dat <- data.frame(x = 1:20, y = c(1:5, 21:25, 6:10, 26:30))
    p <- ggplot(dat, aes(x, y)) + geom_point()

    for (pb in list(p + scale_wrap(n = 3), p + scale_x_cut(breaks = c(3, 7)))) {
        expect_s3_class(ggplot_build(pb), "ggbreak_built")
        expect_gt(count_grobs(ggplotGrob(pb)), count_grobs(ggplotGrob(p)))
    }
})

test_that('the new methods do not send the drawing of a broken plot into a loop (#46)', {
    dat <- data.frame(x = 1:20, y = c(1:5, 21:25, 6:10, 26:30))
    p <- ggplot(dat, aes(x, y)) + geom_point()

    expect_silent(grid.draw(p + scale_y_break(c(12, 20)), recording = FALSE))
    expect_silent(grid.draw(p + scale_x_break(c(6, 16)) + scale_y_break(c(12, 20)),
                            recording = FALSE))
    expect_silent(grid.draw(p + scale_x_break(c(6, 16)) + coord_flip(),
                            recording = FALSE))
})

## the gtable that carries the assembled figure, dug out of a grob tree
find_gtable <- function(grob) {
    if (inherits(grob, "gtable")) {
        if (any(grepl("^panel", grob$layout$name))) return(grob)
        for (child in grob$grobs) {
            found <- find_gtable(child)
            if (!is.null(found)) return(found)
        }
    } else if (inherits(grob, "gTree")) {
        for (child in grob$children) {
            found <- find_gtable(child)
            if (!is.null(found)) return(found)
        }
    }
    NULL
}

test_that('the panel `patchwork` aligns against is the plot area alone (#46)', {
    ## the assembled figure is one opaque panel to `patchwork`, and that panel
    ## used to hold the axis of its windows as well: a plot drawn next to it was
    ## then aligned against a panel that already carried the axis labels and came
    ## out wider than the plot it sits beside
    dat <- data.frame(x = 1:20, y = c(1:5, 21:25, 6:10, 26:30))
    p <- ggplot(dat, aes(x, y)) + geom_point()

    broken <- list(
        p + scale_y_break(c(12, 20)),
        p + scale_x_break(c(6, 16)),
        p + scale_x_break(c(6, 16)) + coord_flip(),
        p + scale_x_break(c(6, 16)) + scale_y_break(c(12, 20)),
        p + scale_wrap(n = 2),
        p + scale_x_cut(breaks = c(6, 16))
    )

    for (pb in broken) {
        g <- ggplotGrob(pb)

        ## the axis is out of the panel
        inner <- find_gtable(g$grobs[[which(g$layout$name == 'panel')[1]]])
        expect_false(is.null(inner))
        expect_false('axis-l-1' %in% inner$layout$name)

        ## and it is in the cell `patchwork` expects an axis in, with the room it
        ## took inside the figure
        li <- which(g$layout$name == 'axis-l')[1]
        expect_false(inherits(g$grobs[[li]], 'zeroGrob'))
        expect_gt(convertWidth(g$widths[g$layout$l[li]], 'mm', valueOnly = TRUE), 0)

        bi <- which(g$layout$name == 'axis-b')[1]
        expect_false(inherits(g$grobs[[bi]], 'zeroGrob'))
        expect_gt(convertHeight(g$heights[g$layout$t[bi]], 'mm', valueOnly = TRUE), 0)
    }
})
