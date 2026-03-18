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
  expect_s3_class(ab3, "ggbreak_params")
  # Depending on implementation, it might be a list of params or a specific class
  # Based on ggbreak implementation, multiple breaks might be stored in a list structure
  # But here we just verify it exists.
  expect_false(is.null(ab3))
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
