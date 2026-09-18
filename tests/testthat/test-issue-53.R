## #53: with `legend.position = "bottom"` the collected legend used to be drawn
## above the axis title, because `patchwork` puts it at the bottom of the
## assembled figure -- i.e. inside the panel of the outer ggplot -- while the
## title was added to that outer ggplot and therefore below the legend.
## `ggplot2` draws the title between the panel and the legend.

## The assembled figure is nested: the outer ggplot's panel holds the gtable
## that `hoist_bottom_axis_title()` builds.  Return the row of the axis title and
## the row of the legend, so that their order can be compared.  Both are `NULL`
## when the figure does not have them.
axis_title_and_legend_rows <- function(plot) {
    gt <- grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
    out <- NULL
    walk <- function(x) {
        if (inherits(x, "gtable") &&
            "guide-box" %in% x$layout$name) {
            out <<- list(
                title  = x$layout$t[x$layout$name == "ggbreak-axis-title"],
                legend = x$layout$t[x$layout$name == "guide-box"]
            )
            return(invisible(NULL))
        }
        if (inherits(x, "gTree") && length(x$children) > 0) {
            for (i in seq_along(x$children)) walk(x$children[[i]])
        }
        invisible(NULL)
    }
    walk(gt)
    out
}

set.seed(2019)
d53 <- data.frame(
    x     = 1:20,
    y     = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
    group = c(rep("A", 10), rep("B", 10))
)

base53 <- ggplot(d53, aes(x, y)) + geom_col() + aes(fill = group)

test_that("the bottom axis title is drawn above the collected legend, #53", {
    p <- base53 + theme(legend.position = "bottom") +
         scale_y_break(c(7, 17))

    rows <- axis_title_and_legend_rows(p)

    expect_false(is.null(rows))
    expect_length(rows$title, 1L)
    expect_length(rows$legend, 1L)
    expect_true(rows$title < rows$legend)
})

test_that("the title keeps its place under coord_flip(), #53", {
    p <- base53 + theme(legend.position = "bottom") +
         coord_flip() + scale_y_break(c(7, 17))

    rows <- axis_title_and_legend_rows(p)

    expect_false(is.null(rows))
    expect_true(rows$title < rows$legend)
})

test_that("the title is still drawn when the legend is at the bottom, #53", {
    p <- base53 + theme(legend.position = "bottom") +
         scale_y_break(c(7, 17))

    gt <- grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(p, recording = FALSE))
    )
    labels <- character(0)
    walk <- function(x) {
        if (inherits(x, "text")) {
            labels <<- c(labels, paste(x$label, collapse = ""))
        }
        if (inherits(x, "gTree") && length(x$children) > 0) {
            for (i in seq_along(x$children)) walk(x$children[[i]])
        }
    }
    walk(gt)

    ## the outer ggplot's x label is the one that ends up at the bottom, and
    ## `coord_flip()` makes it the y label of the plot
    expect_true("y" %in% labels)
})

test_that("a legend that is not at the bottom is left alone, #53", {
    for (pos in c("right", "top", "left")) {
        p <- base53 + theme(legend.position = pos) +
             scale_y_break(c(7, 17))
        rows <- axis_title_and_legend_rows(p)
        expect_false(is.null(rows))
        expect_length(rows$title, 0L)
    }
})
