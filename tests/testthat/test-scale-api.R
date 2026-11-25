library(grid)
library(ggplot2)
library(ggbreak)

## ---------------------------------------------------------------------------
## The break is recorded on the plot
## ---------------------------------------------------------------------------

test_that("Single axis break attributes are stored correctly", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_x_break(c(3, 4))
  expect_false(is.null(attr(p1, "axis_break_x")))
  expect_true(is.null(attr(p1, "axis_break_y")))
  
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_y_break(c(25, 35))
  expect_false(is.null(attr(p2, "axis_break_y")))
  expect_true(is.null(attr(p2, "axis_break_x")))
})

test_that("Multiple breaks on same axis are stored correctly", {
  p3 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4)) + scale_x_break(c(5, 6))
  ab3 <- attr(p3, "axis_break_x")

  # A single break stores one `ggbreak_params`.  Asking for a second one on the
  # same axis stores a plain list of them: the container carries no class, and
  # `extract_axis_break()` reads the list by position (`object[[1]]`,
  # `object[[length(object)]]`), so the class belongs to the elements.
  expect_false(is.null(ab3))
  expect_length(ab3, 2)
  expect_true(all(vapply(ab3, inherits, logical(1), "ggbreak_params")))
  expect_equal(lapply(ab3, `[[`, "breaks"), list(c(3, 4), c(5, 6)))

  # `scale_x_break(c(3, 4, 5, 6))` is the documented way to ask for two breaks on
  # one axis, and it stores a single `ggbreak_params` holding all four numbers
  # rather than a list of two.  Both shapes reach `extract_axis_break()`, so both
  # are pinned down here.
  p3b <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4, 5, 6))
  ab3b <- attr(p3b, "axis_break_x")
  expect_s3_class(ab3b, "ggbreak_params")
  expect_equal(ab3b$breaks, c(3, 4, 5, 6))
})

test_that("Dual axis breaks are stored correctly (x + y)", {
  p4 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
        scale_x_break(c(3, 4)) + scale_y_break(c(25, 35))
  
  expect_false(is.null(attr(p4, "axis_break_x")))
  expect_false(is.null(attr(p4, "axis_break_y")))
  
  # Check if it renders
  expect_error(print(p4), NA)
})

test_that("Dual axis breaks are stored correctly (y + x)", {
  p5 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
        scale_y_break(c(25, 35)) + scale_x_break(c(3, 4))
  
  expect_false(is.null(attr(p5, "axis_break_x")))
  expect_false(is.null(attr(p5, "axis_break_y")))
})

test_that("Layers can be added to dual-axis break plot", {
  p6 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
        scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
        labs(title = "Dual break") + theme_minimal()
  
  expect_false(is.null(attr(p6, "axis_break_x")))
  expect_false(is.null(attr(p6, "axis_break_y")))
  expect_error(print(p6), NA)
})

## ---------------------------------------------------------------------------
## Which scales can be combined
## ---------------------------------------------------------------------------

## #31: `ggbreak` records each scale it adds in the class of the plot, so one
## plot can carry more than one of them.  Only two pairs can be drawn -- a break
## on the x axis with a break on the y axis (`render_dual_break()`), and a single
## cut.  Every other pair used to be handed to `aplot::plot_list()` with the
## second class still on the subplots, and `plot_list()` *draws* a plot that
## still carries one of these classes (`ggfun::is.ggbreak()` is TRUE for all
## three of them, so it calls `ggbreak2ggplot()`) instead of treating it as a
## plain plot.  `scale_wrap(n = 2) + scale_y_break(...)` therefore came out as a
## rendering of the break nested inside the windows of the wrap, announced by
## nothing but a "Coordinate system already present" message, and a cut on both
## axes silently kept the last cut only, because a cut is stored in a single
## attribute while a break keeps `axis_break_x` and `axis_break_y` apart.
##
## The tests below pin down that the pairs that cannot be drawn say so, and that
## the pairs that can still are.

d31 <- data.frame(x = 1:20, y = c(1:10, 30:39))
base31 <- ggplot(d31, aes(x, y)) + geom_point()

draw31 <- function(p) grid::grid.draw(p, recording = FALSE)

## The error has to name both scales, whichever of the two `grid.draw()` happens
## to dispatch on first, so the pair is what is asserted and not the order.
refuses31 <- function(p, ...) {
    msg <- tryCatch({
        draw31(p)
        NULL
    }, error = function(e) conditionMessage(e))
    expect_false(is.null(msg))
    for (s in c(...)) {
        expect_match(msg, s, fixed = TRUE)
    }
    invisible(msg)
}

test_that('a wrapping scale and a break are refused (#31)', {
    refuses31(base31 + scale_wrap(n = 2) + scale_y_break(c(12, 28)),
              "cannot be combined", "scale_wrap()", "scale_y_break()")
    ## the same pair, the other way round
    refuses31(base31 + scale_y_break(c(12, 28)) + scale_wrap(n = 2),
              "cannot be combined", "scale_wrap()", "scale_y_break()")
    refuses31(base31 + scale_wrap(n = 2) + scale_x_break(c(5, 15)),
              "cannot be combined", "scale_wrap()", "scale_x_break()")
})

test_that('a wrapping scale and a cut are refused (#31)', {
    refuses31(base31 + scale_wrap(n = 2) + scale_y_cut(c(12, 28)),
              "cannot be combined", "scale_wrap()", "scale_y_cut()")
    refuses31(base31 + scale_wrap(n = 2) + scale_x_cut(c(5, 15)),
              "cannot be combined", "scale_wrap()", "scale_x_cut()")
})

test_that('a break and a cut are refused (#31)', {
    refuses31(base31 + scale_x_break(c(5, 15)) + scale_y_cut(c(12, 28)),
              "cannot be combined", "scale_x_break()", "scale_y_cut()")
    refuses31(base31 + scale_x_cut(c(5, 15)) + scale_y_break(c(12, 28)),
              "cannot be combined", "scale_x_cut()", "scale_y_break()")
})

test_that('a cut on both axes is refused rather than dropping one (#31)', {
    ## a cut is stored in a single attribute, so the second cut used to replace
    ## the first and the plot came out cut on one axis only, without a word
    ## about the cut that was dropped -- the two are not the same figure
    expect_error(base31 + scale_x_cut(c(5, 15)) + scale_y_cut(c(12, 28)),
                 "`scale_x_cut()` cannot be combined with `scale_y_cut()`",
                 fixed = TRUE)
    expect_error(base31 + scale_y_cut(c(12, 28)) + scale_x_cut(c(5, 15)),
                 "`scale_y_cut()` cannot be combined with `scale_x_cut()`",
                 fixed = TRUE)
})

test_that('the refusal is on every path that draws the plot (#31)', {
    p <- base31 + scale_wrap(n = 2) + scale_y_break(c(12, 28))
    ## `print()` and `ggsave()`, i.e. the path the user takes
    expect_error(print(p), "cannot be combined")
    ## `ggplotGrob()`, the path `patchwork` and `cowplot` take, see #46
    expect_error(ggplotGrob(p), "cannot be combined")
    ## `ggplot_build()` alone does not draw the plot -- it is `ggplot_gtable()`
    ## that asks `grid.draw()` for the assembled figure -- so the pair has to be
    ## refused by the second half of `ggplotGrob()`
    expect_error(ggplot_gtable(ggplot_build(p)), "cannot be combined")
})

test_that('the pairs that can be drawn still are (#31)', {
    ## a single scale on one axis, whatever it is
    ok <- list(
        wrap     = base31 + scale_wrap(n = 2),
        ybreak   = base31 + scale_y_break(c(12, 28)),
        xbreak   = base31 + scale_x_break(c(5, 15)),
        ycut     = base31 + scale_y_cut(c(12, 28)),
        xcut     = base31 + scale_x_cut(c(5, 15)),
        ## both axes broken
        dual     = base31 + scale_x_break(c(5, 15)) + scale_y_break(c(12, 28)),
        ## more than one break on the same axis
        twobreak = base31 + scale_y_break(c(5, 8)) + scale_y_break(c(25, 28))
    )
    for (nm in names(ok)) {
        expect_error(suppressMessages(suppressWarnings(draw31(ok[[nm]]))), NA)
    }
})

test_that('a break and a cut on the same axis are not read as one another (#31)', {
    ## the message has to name the axis each scale is on: a cut knows its axis,
    ## a break knows it through which of the two attributes is set.  Only the
    ## first line is checked, the hint below it names the pairs that do work.
    msg <- refuses31(base31 + scale_x_break(c(5, 15)) + scale_y_cut(c(12, 28)),
                     "scale_x_break()", "scale_y_cut()")
    first <- strsplit(msg, "\n", fixed = TRUE)[[1]][1]
    expect_match(first, "scale_x_break()", fixed = TRUE)
    expect_match(first, "scale_y_cut()", fixed = TRUE)
    expect_false(grepl("scale_y_break", first, fixed = TRUE))
    expect_false(grepl("scale_x_cut", first, fixed = TRUE))
})
