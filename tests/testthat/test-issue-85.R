## #85: with `theme(legend.position = "bottom")` the axis title of a broken plot
## used to be drawn below the collected legend, i.e. at the very bottom of the
## figure, while `ggplot2` draws it between the panel and the legend.
##
## The title is put into the assembled figure by `hoist_bottom_axis_title()`,
## which gives it a row of its own directly above the `guide-box` row of the
## `patchwork` gtable and drops it from the outer labs, see #53.  #85 reports the
## same defect as #53 -- the reporter was on a version that predates that fix --
## so these tests pin the contract for every break flavour, including
## `scale_wrap()` and the cut scales, which each build their figure in their own
## `grid.draw()` method.

suppressMessages(library(ggplot2))
suppressMessages(library(testthat))

grob_texts <- function(z) {
    out <- character(0)
    if (inherits(z, "text") && length(z$label) == 1 && nzchar(z$label)) {
        out <- c(out, z$label)
    }
    if (inherits(z, "gTree") && length(z$children) > 0) {
        for (i in seq_along(z$children)) out <- c(out, grob_texts(z$children[[i]]))
    }
    out
}

## The assembled figure is nested: the panel of the outer ggplot holds the
## gtable that `hoist_bottom_axis_title()` builds, and `grid.force()` is what
## populates the children of that gTree.  Return the table row of the hoisted
## title and of the collected legend in that nested gtable, together with the
## text the title carries.
nested_axis_title_and_legend <- function(plot) {
    gt <- grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
    out <- NULL
    walk <- function(x) {
        if (inherits(x, "gtable") && "guide-box" %in% x$layout$name) {
            lt <- x$layout
            ti <- which(lt$name == "ggbreak-axis-title")
            out <<- list(
                title  = lt$t[ti],
                legend = lt$t[lt$name == "guide-box"],
                text   = unlist(lapply(ti, function(i) grob_texts(x$grobs[[i]])))
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

## The outer ggplot must not draw the title itself, otherwise it lands in the
## bottom margin of the whole figure, below the panel that carries the legend.
outer_lab <- function(plot, axis) {
    grid::grid.draw(plot, recording = FALSE)$labels[[axis]]
}

## The reporter's data from #85.
set.seed(1)
d85 <- data.frame(x = 1:10, y = c(runif(19), 10), d = gl(2, 10))
base85 <- ggplot(d85) + geom_point(aes(x = x, y = y, colour = d))

## one title row, directly above the collected legend, and no title left to the
## outer ggplot
expect_title_above_legend <- function(plot, text) {
    rows <- nested_axis_title_and_legend(plot)
    expect_false(is.null(rows))
    expect_length(rows$legend, 1L)
    expect_length(rows$title, 1L)
    expect_identical(rows$title, rows$legend - 1L)
    if (!missing(text)) expect_true(text %in% rows$text)
    expect_null(outer_lab(plot, "x"))
}

test_that("the reporter's plot keeps its x title above the bottom legend, #85", {
    p <- base85 + theme(legend.position = "bottom") +
         ggbreak::scale_y_break(breaks = c(2, 9))
    expect_title_above_legend(p, "x")
})

test_that("an explicit xlab() keeps its place, with either axis broken, #85", {
    for (axis in c("x", "y")) {
        p <- base85 + xlab("x-axis") + ylab("y-axis") +
             theme(legend.position = "bottom")
        p <- if (axis == "x") {
                 p + scale_x_break(c(3, 7))
             } else {
                 p + scale_y_break(c(2, 9))
             }
        expect_title_above_legend(p, "x-axis")
    }
})

test_that("coord_flip() hoists the horizontal title, which is the y label, #85", {
    p <- base85 + xlab("x-axis") + ylab("y-axis") +
         theme(legend.position = "bottom") + coord_flip() +
         scale_y_break(c(2, 9))
    expect_title_above_legend(p, "y-axis")
})

test_that("a dual break also keeps the bottom title above the legend, #85", {
    p <- base85 + xlab("x-axis") + ylab("y-axis") +
         theme(legend.position = "bottom") +
         scale_x_break(c(3, 7)) + scale_y_break(c(2, 9))
    expect_title_above_legend(p, "x-axis")
})

test_that("scale_wrap() and the cut scales hoist the title as well, #85", {
    broken <- list(
        wrap = base85 + scale_wrap(n = 3),
        xcut = base85 + scale_x_cut(breaks = c(3, 7)),
        ycut = base85 + scale_y_cut(breaks = c(2, 9))
    )
    for (nm in names(broken)) {
        p <- broken[[nm]] + xlab("x-axis") + theme(legend.position = "bottom")
        expect_title_above_legend(p, "x-axis")
    }
})

test_that("a legend that is not at the bottom is left alone, #85", {
    for (pos in c("right", "top", "left")) {
        p <- base85 + xlab("x-axis") +
             theme(legend.position = pos) + scale_y_break(c(2, 9))
        rows <- nested_axis_title_and_legend(p)
        expect_false(is.null(rows))
        expect_length(rows$title, 0L)
        ## the title stays on the outer ggplot, where `ggplot2` puts it
        expect_identical(outer_lab(p, "x"), "x-axis")
    }
})

test_that("a plot that carries no title gets no title row, #85", {
    p <- base85 + labs(x = NULL) + theme(legend.position = "bottom") +
         scale_y_break(c(2, 9))
    rows <- nested_axis_title_and_legend(p)
    expect_false(is.null(rows))
    expect_length(rows$legend, 1L)
    expect_length(rows$title, 0L)
    expect_null(outer_lab(p, "x"))
})

## The follow-up report in #85: the same defect only shows when the title comes
## from the `name` of a position scale -- `scale_x_continuous("test")` -- rather
## than from `labs()`.  Three things used to go wrong together: `extract_totallabs()`
## read the raw `plot$labels` and saw the aesthetic default "x" instead of "test";
## `check_axis_title()` re-added "test" to the outer ggplot, which draws below the
## collected legend; and the windows did not blank a scale `name`, so `scale_wrap()`
## and the cut scales printed "test" once per panel.  These assert the fix on the
## grob tree -- not on pixels -- the way the tests above do.

## how many times a string is drawn in the whole figure; a correct figure draws
## its title exactly once, no matter how many windows the break is built from
drawn_text_count <- function(plot, text) {
    gt <- grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
    sum(grob_texts(gt) == text)
}

test_that("an x title from a scale name is hoisted, once, above the legend, #85", {
    p <- base85 + theme(legend.position = "bottom") +
         scale_x_continuous("test") + scale_y_break(c(2, 9))
    ## one title row above the legend, carrying the scale name, and no stray
    ## default "x" left on the outer ggplot
    expect_title_above_legend(p, "test")
    ## not the pre-fix "x" from `ggplot_build()$plot$labels`
    rows <- nested_axis_title_and_legend(p)
    expect_false("x" %in% rows$text)
    ## drawn exactly once across the whole figure -- the duplicate below the
    ## legend and the check_axis_title() copy are gone
    expect_identical(drawn_text_count(p, "test"), 1L)
    expect_identical(drawn_text_count(p, "x"), 0L)
})

test_that("a scale name and xlab() agree on a single title, #85", {
    p <- base85 + theme(legend.position = "bottom") +
         scale_x_continuous("test") + xlab("test") + scale_y_break(c(2, 9))
    expect_title_above_legend(p, "test")
    expect_identical(drawn_text_count(p, "test"), 1L)
})

test_that("each scale name lands on its own axis, distinct titles, #85", {
    p <- base85 + theme(legend.position = "bottom") +
         scale_x_continuous("XL") + scale_y_continuous("YL") +
         scale_y_break(c(2, 9))
    rows <- nested_axis_title_and_legend(p)
    expect_false(is.null(rows))
    ## the hoisted (bottom) title is the x one, and it is not swapped with the y
    expect_true("XL" %in% rows$text)
    expect_false("YL" %in% rows$text)
    ## both titles drawn exactly once, none repeated per window
    expect_identical(drawn_text_count(p, "XL"), 1L)
    expect_identical(drawn_text_count(p, "YL"), 1L)
})

test_that("wrap and cut draw a scale-name title once, not per window, #85", {
    broken <- list(
        wrap = base85 + scale_wrap(n = 3),
        xcut = base85 + scale_x_cut(breaks = c(3, 7)),
        ycut = base85 + scale_y_cut(breaks = c(2, 9))
    )
    for (nm in names(broken)) {
        p <- broken[[nm]] + scale_x_continuous("test") +
             theme(legend.position = "bottom")
        ## with three windows a scale `name` used to be printed in each of them
        expect_identical(drawn_text_count(p, "test"), 1L,
                         info = paste("scale:", nm))
        rows <- nested_axis_title_and_legend(p)
        expect_false(is.null(rows), info = paste("scale:", nm))
        expect_length(rows$title, 1L)
        expect_true("test" %in% rows$text, info = paste("scale:", nm))
    }
})

test_that("a scale-name title with a non-bottom legend is drawn once on the axis, #85", {
    p <- base85 + theme(legend.position = "right") +
         scale_x_continuous("test") + scale_y_break(c(2, 9))
    ## nothing is hoisted, the outer ggplot carries the resolved title
    rows <- nested_axis_title_and_legend(p)
    expect_false(is.null(rows))
    expect_length(rows$title, 0L)
    expect_identical(outer_lab(p, "x"), "test")
    expect_identical(drawn_text_count(p, "test"), 1L)
})

## `patchwork` and `cowplot` never reach `grid.draw()`; they call `ggplotGrob()`,
## which assembles the figure through `ggplot_gtable()` and rebuilds it in
## `align_assembled_figure()`.  That rebuild keeps the cells of the panel area, and
## the row `hoist_bottom_axis_title()` gives the title lies *below* the panels, so
## a broken plot with a bottom legend used to be handed over without its title --
## once the title also stopped being re-added to the outer ggplot by
## `check_axis_title()`, nothing was left to draw it.  Assert on the text, not on
## the row name: on this path the title is moved out of the `ggbreak-axis-title`
## row into the cell `ggplot2` keeps for the bottom axis title.
test_that("ggplotGrob() keeps the hoisted title of a broken plot, #85", {
    for (nm in c("break", "wrap")) {
        p <- if (nm == "break") {
                 base85 + theme(legend.position = "bottom") +
                     scale_x_continuous("test") + scale_y_break(c(2, 9))
             } else {
                 base85 + theme(legend.position = "bottom") +
                     scale_wrap(n = 3) + scale_x_continuous("test")
             }
        gt <- grid::grid.force(ggplot2::ggplotGrob(p))
        expect_identical(sum(grob_texts(gt) == "test"), 1L, info = nm)
        ## and not the aesthetic default it used to report
        expect_identical(sum(grob_texts(gt) == "x"), 0L, info = nm)
    }
})
