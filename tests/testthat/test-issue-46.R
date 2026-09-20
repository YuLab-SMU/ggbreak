## #46: a broken plot that is combined with other plots came out with no break at
## all.  `patchwork` and `cowplot` do not draw a plot the way `print()` does --
## they call `ggplotGrob()`, which is `ggplot_gtable(ggplot_build(x))` -- and
## neither of those knew about the break, so the plot was built *without* it and
## the break was dropped without a warning.  A broken plot is now built as a
## whole, so `ggplotGrob()` returns the assembled figure.

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
