## A broken axis and a facet grid nest in opposite directions.
##
## `ggbreak` draws one subplot per window, and the subplot of a faceted plot
## carries the whole facet grid, so the windows used to be stacked *outside* the
## facets: the facet rows came out A B A B instead of A A B B, which tore every
## facet row away from its other windows, see #55 and #17.

facet_data <- function() {
    set.seed(2019-01-19)
    data.frame(
        x = 1:20,
        y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
        row = c(rep("A", 10), rep("B", 10)),
        col = c(rep("C", 5), rep("D", 5), rep("C", 5), rep("D", 5))
    )
}

facet_plot <- function(...) {
    ggplot2::ggplot(facet_data(), ggplot2::aes(x, y)) +
        ggplot2::geom_col(orientation = "x") +
        ggplot2::facet_grid(...) +
        ggplot2::theme_bw()
}

## every label the figure draws, in the order it draws them, which for a facet
## grid is top to bottom
drawn_labels <- function(x) {
    gt <- if (inherits(x, "gg")) {
        grid::grid.force(ggplot2::ggplotGrob(grid::grid.draw(x, recording = FALSE)))
    } else {
        grid::grid.force(x)
    }
    out <- character(0)
    walk <- function(z) {
        if (inherits(z, "text") || inherits(z, "titleGrob")) {
            lb <- z$label
            if (!is.null(lb) && length(lb) == 1L && nzchar(lb)) out <<- c(out, lb)
        }
        if (inherits(z, "gtable")) {
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(gt)
    out
}

## the strips of a facet dimension, in the order they are drawn
strips_of <- function(x, levels) {
    lab <- drawn_labels(x)
    lab[lab %in% levels]
}

test_that("slicing a built facet grid gives one facet row at a time", {
    g <- ggplot2::ggplotGrob(facet_plot(row ~ col))

    expect_length(ggbreak:::facet_panel_positions(g, "row"), 2L)
    expect_length(ggbreak:::facet_panel_positions(g, "col"), 2L)
    expect_equal(strips_of(ggbreak:::slice_facet_gtable(g, "row", 1), c("A", "B")), "A")
    expect_equal(strips_of(ggbreak:::slice_facet_gtable(g, "row", 2), c("A", "B")), "B")
    expect_equal(strips_of(ggbreak:::slice_facet_gtable(g, "col", 1), c("C", "D")), "C")
    expect_equal(strips_of(ggbreak:::slice_facet_gtable(g, "col", 2), c("C", "D")), "D")
    ## both facet rows are kept when a column is sliced out; the row strips sit
    ## on the last column, which is where `ggplot2` draws them
    expect_equal(strips_of(ggbreak:::slice_facet_gtable(g, "col", 2), c("A", "B")),
                 c("A", "B"))
})

test_that("nothing is nested when the plot is not faceted along the broken axis", {
    d <- facet_data()
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_col()

    ## no facet at all
    expect_null(ggbreak:::nest_facet_windows(list(p, p), c(1, 1), "row"))
    expect_null(ggbreak:::nest_facet_windows(list(p, p), c(1, 1), "col"))

    ## `facet_wrap()` puts the two panels side by side, so a y break has nothing
    ## to nest, while an x break has
    pw <- p + ggplot2::facet_wrap(~ row)
    expect_null(ggbreak:::nest_facet_windows(list(pw, pw), c(1, 1), "row"))
    expect_false(is.null(ggbreak:::nest_facet_windows(list(pw, pw), c(1, 1), "col")))

    ## a single window is never nested
    expect_null(ggbreak:::nest_facet_windows(list(pw), 1, "col"))
})

test_that("the windows of a facet row stay together (#55)", {
    p <- facet_plot(row ~ col)
    expect_equal(strips_of(p + scale_y_break(c(7, 17), scales = "free"), c("A", "B")),
                 c("A", "A", "B", "B"))
    ## the column strips are still drawn once, at the top
    expect_equal(strips_of(p + scale_y_break(c(7, 17), scales = "free"), c("C", "D")),
                 c("C", "D"))
})

test_that("the windows of a facet column stay together (#55)", {
    p <- ggplot2::ggplot(facet_data(), ggplot2::aes(x, y)) +
        ggplot2::geom_col() + ggplot2::facet_grid(. ~ col) + ggplot2::theme_bw()
    expect_equal(strips_of(p + scale_x_break(c(7, 14)), c("C", "D")),
                 c("C", "C", "D", "D"))
})

test_that("`coord_flip()` nests along the direction the windows are stacked on", {
    ## with `coord_flip()` a y break is drawn sideways, so it is the facet
    ## columns that have to stay together, and that is the reporter's example
    p <- facet_plot(row ~ col) + ggplot2::coord_flip()
    expect_equal(strips_of(p + scale_y_break(c(7, 17), scales = "free"), c("C", "D")),
                 c("C", "C", "D", "D"))
})

test_that("`scale_y_cut()` nests like `scale_y_break()`", {
    p <- facet_plot(row ~ col)
    expect_equal(strips_of(p + scale_y_cut(breaks = c(3, 6), which = c(1, 2)), c("A", "B")),
                 c("A", "A", "A", "B", "B", "B"))
})

test_that("the legend is still drawn once when the figure is reassembled", {
    ## the pieces are single grobs, so `patchwork` cannot collect their guides
    ## and the legend has to be put back by hand
    d <- facet_data()
    for (position in c("right", "bottom", "left", "top")) {
        p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = col)) +
            ggplot2::geom_col() + ggplot2::facet_grid(row ~ col) +
            ggplot2::theme_bw() + ggplot2::theme(legend.position = position)
        expect_equal(sum(drawn_labels(p + scale_y_break(c(7, 17), scales = "free")) == "col"),
                     1L, info = position)
    }
    ## and it is not drawn at all when the plot does not ask for it
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = col)) +
        ggplot2::geom_col() + ggplot2::facet_grid(row ~ col) +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")
    expect_equal(sum(drawn_labels(p + scale_y_break(c(7, 17), scales = "free")) == "col"),
                 0L)
})
