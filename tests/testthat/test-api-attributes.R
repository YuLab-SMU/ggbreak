library(ggplot2)
library(ggbreak)

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
