library(ggplot2)
library(ggbreak)

## the sides of the figure that carry a break mark, one entry per gap and side,
## read off the assembled figure (#87)
mark_sides <- function(p) {
    pdf(NULL)
    on.exit(dev.off())
    g <- suppressWarnings(grid::grid.draw(p, recording = FALSE))
    found <- character(0)
    walk <- function(x) {
        if (inherits(x, "gtable")) {
            hit <- grep("^ggbreak-mark-", x$layout$name, value = TRUE)
            found <<- c(found, sub("^ggbreak-mark-([a-z]+)-.*$", "\\1", hit))
            for (ch in x$grobs) walk(ch)
        } else if (inherits(x, "gTree")) {
            for (ch in x$children) walk(ch)
        }
    }
    walk(ggplotGrob(g))
    sort(found)
}

p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

test_that("a mark is drawn only along an axis or a border the plot draws (#87)", {
    ## no top axis and no border: the top edge is not interrupted
    expect_equal(mark_sides(p + theme_minimal() + scale_x_break(c(3, 4), symbol = "slash")),
                 "bottom")
    expect_equal(mark_sides(p + theme_classic() + scale_x_break(c(3, 4), symbol = "slash")),
                 "bottom")
    ## a boxed panel interrupts the top line as well
    expect_equal(mark_sides(p + theme_bw() + scale_x_break(c(3, 4), symbol = "slash")),
                 c("bottom", "top"))
    expect_equal(mark_sides(p + theme_classic() + scale_y_break(c(25, 30), symbol = "slash")),
                 "left")
})

test_that("a dual break marks only the outer axes it draws, as in #87", {
    ## the report: theme_minimal() got marks on the top and the right as well
    dual <- p + scale_x_break(c(3, 4), symbol = "slash") +
        scale_y_break(c(25, 30), symbol = "slash")
    expect_equal(mark_sides(dual + theme_minimal()), c("bottom", "left"))
    expect_equal(mark_sides(dual + theme_bw()), c("bottom", "left", "right", "top"))
    ## a symbol on one axis marks the gaps of that axis only
    expect_equal(mark_sides(p + theme_classic() + scale_x_break(c(3, 4), symbol = "slash") +
                                scale_y_break(c(25, 30))),
                 "bottom")
})

test_that("marks follow the axis wherever it is drawn", {
    expect_equal(mark_sides(p + theme_classic() + scale_x_continuous(position = "top") +
                                scale_y_break(c(25, 30), symbol = "slash")),
                 "left")
    expect_equal(mark_sides(p + theme_classic() + scale_y_continuous(position = "right") +
                                scale_y_break(c(25, 30), symbol = "slash")),
                 "right")
    expect_equal(mark_sides(p + theme_classic() + coord_flip() +
                                scale_x_break(c(3, 4), symbol = "slash")),
                 "left")
    expect_equal(mark_sides(ggplot(mpg, aes(class, hwy)) + geom_boxplot() + theme_classic() +
                                scale_x_break(c("compact", "midsize"), symbol = "slash")),
                 "bottom")
})

test_that("every gap is marked once, and a faceted window only between windows", {
    expect_equal(mark_sides(p + theme_classic() + scale_x_break(c(2.5, 3)) +
                                scale_x_break(c(4, 5), symbol = "slash")),
                 c("bottom", "bottom"))
    expect_equal(mark_sides(p + theme_classic() + facet_wrap(~drv) +
                                scale_x_break(c(3, 4), symbol = "slash")),
                 "bottom")
})

test_that("no symbol, no marks; an unknown symbol is refused", {
    expect_length(mark_sides(p + theme_bw() + scale_x_break(c(3, 4))), 0)
    expect_error(scale_x_break(c(3, 4), symbol = "zigzag"), "symbol")
})
