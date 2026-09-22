## #52: a broken plot could not be superimposed on another plot.
##
## `patchwork` gives the assembled figure a background of its own, taken from the
## default theme, and that rectangle is opaque.  Two plots stacked with
## `annotation_custom()` therefore hid each other: the `plot.background` the user
## set on the plot never reached the figure, because the background of the figure
## was painted by the assembly instead of by the outer ggplot that holds it.
##
## The contract these tests pin: the assembled figure paints no background of its
## own, and the background of the figure is the one the user asked for.

suppressMessages(library(ggplot2))
suppressMessages(library(testthat))

drawn_gtable <- function(plot) {
    grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
}

## The gtable that `patchwork` assembles the subplots into.  It is the one that
## carries the `panel-area` entry, and it is nested inside the panel of the outer
## ggplot.
patch_gtable <- function(plot) {
    found <- NULL
    walk <- function(z) {
        if (!is.null(found)) return(invisible(NULL))
        if (inherits(z, "gtable")) {
            if ("panel-area" %in% z$layout$name) {
                found <<- z
                return(invisible(NULL))
            }
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(drawn_gtable(plot))
    found
}

## The fill of every grob that paints a background and really paints something.
## A transparent fill is what the user asked for in #52, so it does not count.
opaque_background_fills <- function(plot) {
    out <- character(0)
    walk <- function(z) {
        nm <- if (is.null(z$name)) "" else z$name
        if (inherits(z, "rect") && grepl("background", nm)) {
            fill <- z$gp$fill
            if (!is.null(fill) && !anyNA(fill) &&
                any(grDevices::col2rgb(fill, alpha = TRUE)[4, ] > 0)) {
                out <<- c(out, fill)
            }
        }
        if (inherits(z, "gtable")) {
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(drawn_gtable(plot))
    out
}

set.seed(2022 - 8 - 23)
d <- data.frame(x = 1:20,
                y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22))
base <- ggplot(d, aes(x, y)) + geom_point()

transparent <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
)

test_that("the assembled figure paints no background of its own, #52", {
    p <- base + scale_y_break(c(7, 17))
    pg <- patch_gtable(p)
    expect_false(is.null(pg))
    i <- which(pg$layout$name == "background")
    expect_length(i, 1L)
    expect_s3_class(pg$grobs[[i]], "zeroGrob")
})

test_that("a transparent plot.background really makes the figure transparent, #52", {
    p <- base + transparent + scale_y_break(c(7, 17))
    expect_length(opaque_background_fills(p), 0L)
})

test_that("the same holds for every break flavour, #52", {
    broken <- list(
        y_break = base + scale_y_break(c(7, 17)),
        x_break = base + scale_x_break(c(7, 17)),
        y_cut = base + scale_y_cut(c(7, 17)),
        wrap = base + scale_wrap(n = 3)
    )
    for (nm in names(broken)) {
        p <- broken[[nm]] + transparent
        expect_length(opaque_background_fills(p), 0L)
    }
})

test_that("a figure that asks for no background keeps a white one by default, #52", {
    p <- base + scale_y_break(c(7, 17))
    fills <- opaque_background_fills(p)
    ## the outer ggplot paints the figure white, as `ggplot2` does
    expect_true("white" %in% fills)
})
