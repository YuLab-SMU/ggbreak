## `coord_cartesian()`/`coord_flip()` limits are kept, #59.
##
## `ggbreak` gives every subplot a fresh coord so that the broken axis can be
## given its own window, and that fresh coord used to drop the limits the user
## had set on the *other* axis.  Adding any coord to a wrapped or cut plot also
## used to recurse for ever (see the second block below).

## every text label drawn in the assembled figure
tick_labels <- function(plot) {
    g <- grid::grid.draw(plot, recording = FALSE)
    gt <- grid::grid.force(ggplot2::ggplotGrob(g))
    out <- character(0)
    walk <- function(x) {
        if (inherits(x, "text")) out <<- c(out, paste(x$label, collapse = ""))
        if (inherits(x, "gTree") && length(x$children) > 0)
            for (i in seq_along(x$children)) walk(x$children[[i]])
    }
    walk(gt)
    sort(unique(out))
}

p <- ggplot() + geom_point(data = iris, aes(x = Sepal.Length, y = Petal.Width))

test_that("coord_cartesian(xlim) is honoured next to scale_y_break()", {
    ## the un-broken plot draws 5.00 5.25 5.50 5.75 6.00 on x
    base <- tick_labels(p + coord_cartesian(xlim = c(5, 6)))
    expect_true(any(grepl("5.00", base, fixed = TRUE)))

    broken <- tick_labels(p + scale_y_break(c(0.7, 0.9)) + coord_cartesian(xlim = c(5, 6)))
    expect_true(any(grepl("5.00", broken, fixed = TRUE)))
    expect_true(any(grepl("6.00", broken, fixed = TRUE)))

    ## and the x axis is left alone when the user set no limits
    nolim <- tick_labels(p + scale_y_break(c(0.7, 0.9)))
    expect_false(any(grepl("5.00", nolim, fixed = TRUE)))
})

test_that("coord_cartesian(ylim) is honoured next to scale_x_break()", {
    broken <- tick_labels(p + scale_x_break(c(5.5, 6)) + coord_cartesian(ylim = c(0.5, 1.5)))
    expect_true(any(grepl("0.50", broken, fixed = TRUE)))
    expect_true(any(grepl("1.50", broken, fixed = TRUE)))

    nolim <- tick_labels(p + scale_x_break(c(5.5, 6)))
    expect_false(any(grepl("0.50", nolim, fixed = TRUE)))
})

test_that("coord_flip() limits are honoured too", {
    broken <- tick_labels(p + scale_y_break(c(0.7, 0.9)) + coord_flip(xlim = c(5, 6)))
    expect_true(any(grepl("5.00", broken, fixed = TRUE)))
})

test_that("a coord can be added to a wrapped or cut plot", {
    ## `is.ggbreak()` is TRUE for "ggbreak", "ggwrap" and "ggcut", but
    ## `.drop_class()` only removed "ggbreak", so `ggplot_add()` re-dispatched
    ## to `ggplot_add.gg()` for ever and R died with a C stack overflow.
    drawn <- function(plot) {
        tryCatch(grid::grid.draw(plot, recording = FALSE),
                 error = function(e) e)
    }
    expect_false(inherits(drawn(p + scale_wrap(2) + coord_cartesian()), "error"))
    expect_false(inherits(drawn(p + scale_wrap(2) + coord_flip()), "error"))
    expect_false(inherits(drawn(p + scale_y_cut(1.5) + coord_cartesian()), "error"))
    expect_false(inherits(drawn(p + scale_x_cut(5.5) + coord_flip()), "error"))

    ## the same trap for anything else `ggplot_add()` handles
    expect_false(inherits(drawn(p + scale_wrap(2) + theme_bw()), "error"))
    expect_false(inherits(drawn(p + scale_y_cut(1.5) + labs(title = "t")), "error"))
})
