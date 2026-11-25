library(ggplot2)
library(ggbreak)

## the gtable the windows of a faceted plot are bound into, see
## `nest_facet_windows()`
nested_gtable <- function(p) {
    pdf(NULL)
    on.exit(dev.off())
    g <- suppressWarnings(grid::grid.draw(p, recording = FALSE))
    found <- NULL
    walk <- function(x) {
        if (!is.null(found)) return()
        if (inherits(x, "gtable")) {
            if (any(grepl("^strip-", x$layout$name)) &&
                sum(grepl("^panel-[0-9]+-[0-9]+$", x$layout$name)) > 1) {
                found <<- x
                return()
            }
            for (ch in x$grobs) walk(ch)
        } else if (inherits(x, "gTree")) {
            for (ch in x$children) walk(ch)
        }
    }
    walk(ggplotGrob(g))
    found
}

## the height (width) of the rows (columns) strictly between two cells, in mm
gap_mm <- function(g, from, to, dim = "row") {
    idx <- seq(from + 1, to - 1)
    if (from + 1 > to - 1) return(0)
    u <- if (dim == "row") g$heights[idx] else g$widths[idx]
    conv <- if (dim == "row") grid::convertHeight else grid::convertWidth
    sum(conv(u, "mm", valueOnly = TRUE))
}

df <- data.frame(
    x   = rep(1:4, 4),
    y   = c(10, 50, 80, 300, 20, 60, 90, 250, 15, 40, 70, 400, 5, 30, 95, 350),
    row = rep(c("A", "B"), each = 8),
    col = rep(rep(c("C", "D"), each = 4), 2)
)
p <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(row ~ col)

test_that("a facet row keeps one strip across all of its windows (#86)", {
    g <- nested_gtable(p + scale_y_break(c(100, 200), scales = 0.25))
    lay <- g$layout
    strips <- lay[grepl("^strip-r-", lay$name), ]
    expect_equal(nrow(strips), 2L)
    ## every strip spans the two windows of its facet row
    left <- lay[grepl("^panel-1-[0-9]+$", lay$name), ]
    for (i in seq_len(nrow(strips))) {
        inside <- left$t >= strips$t[i] & left$b <= strips$b[i]
        expect_equal(sum(inside), 2L)
    }
})

test_that("the windows of a facet are set apart by `space`, the facets by the panel spacing (#86)", {
    g <- nested_gtable(p + scale_y_break(c(100, 200), scales = 0.25, space = 0.5))
    lay <- g$layout
    left <- lay[grepl("^panel-1-[0-9]+$", lay$name), ]
    left <- left[order(left$t), ]
    ## A top, A bottom, B top, B bottom
    expect_equal(nrow(left), 4L)
    expect_equal(gap_mm(g, left$b[1], left$t[2]), 5, tolerance = 1e-6)
    expect_gt(gap_mm(g, left$b[2], left$t[3]), 0)
    expect_equal(gap_mm(g, left$b[3], left$t[4]), 5, tolerance = 1e-6)
    ## facet columns C and D are not glued together either
    top <- lay[lay$t == left$t[1] & grepl("^panel-", lay$name), ]
    top <- top[order(top$l), ]
    expect_gt(gap_mm(g, top$r[1], top$l[2], "col"), 0)
})

test_that("a facet column keeps one strip across its windows of an x break (#86)", {
    q <- ggplot(df, aes(y, x)) + geom_point() + facet_grid(col ~ row) +
        scale_x_break(c(100, 200), scales = 0.25)
    g <- nested_gtable(q)
    expect_equal(sum(grepl("^strip-t-", g$layout$name)), 2L)
})

test_that("break marks of a faceted plot go between windows, not between facets", {
    g <- nested_gtable(p + theme_bw() + scale_y_break(c(100, 200), symbol = "slash"))
    marks <- grep("^ggbreak-mark-", g$layout$name, value = TRUE)
    ## a gap per facet row, marked on the left and on the right border
    expect_equal(sort(sub("^ggbreak-mark-([a-z]+)-.*$", "\\1", marks)),
                 c("left", "left", "right", "right"))
})
