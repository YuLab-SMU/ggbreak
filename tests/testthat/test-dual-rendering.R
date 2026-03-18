library(ggplot2)
library(ggbreak)

test_that("Regression: Single x/y breaks render correctly", {
  p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_x_break(c(3, 4))
  expect_error(print(p1), NA)
  
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_y_break(c(25, 35))
  expect_error(print(p2), NA)
  
  p3 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4)) + scale_x_break(c(5, 6))
  expect_error(print(p3), NA)
})

test_that("Dual axis rendering: Basic x + y", {
  p4 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35))
  expect_error(print(p4), NA)
})

test_that("Dual axis rendering: y + x order", {
  p5 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_y_break(c(25, 35)) + scale_x_break(c(3, 4))
  expect_error(print(p5), NA)
})

test_that("Dual axis rendering: Scatter with outlier clusters", {
  set.seed(42)
  df <- data.frame(
    x = c(rnorm(50, 5, 1), rnorm(10, 50, 2)),
    y = c(rnorm(50, 10, 2), rnorm(10, 100, 5))
  )
  p6 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 45)) + scale_y_break(c(20, 90))
  expect_error(print(p6), NA)
})

test_that("Dual axis rendering: With title and theme", {
  p7 <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
    labs(title = "Dual axis break", subtitle = "with color") +
    theme_minimal()
  expect_error(print(p7), NA)
})

test_that("Dual axis rendering: Parameters (space, scales)", {
  set.seed(42)
  df <- data.frame(
    x = c(rnorm(50, 5, 1), rnorm(10, 50, 2)),
    y = c(rnorm(50, 10, 2), rnorm(10, 100, 5))
  )
  
  # Test space parameter
  p8 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 45), space = 0.5) +
    scale_y_break(c(20, 90), space = 0.5)
  expect_error(print(p8), NA)
  
  # Test scales parameter
  p9 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 45), scales = 0.3) +
    scale_y_break(c(20, 90), scales = 0.3)
  expect_error(print(p9), NA)
})

test_that("Dual axis rendering: Multiple breaks on BOTH axes", {
  set.seed(42)
  df <- data.frame(
    x = c(rnorm(50, 5, 1), rnorm(10, 50, 2)),
    y = c(rnorm(50, 10, 2), rnorm(10, 100, 5))
  )
  p10 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 20)) + scale_x_break(c(30, 45)) +
    scale_y_break(c(20, 40)) + scale_y_break(c(60, 90))
  expect_error(print(p10), NA)
})

test_that("Dual axis rendering: Compatibility with various themes", {
  base_p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
    labs(title = "Dual axis break")
  
  expect_error(print(base_p + theme_bw()), NA)
  expect_error(print(base_p + theme_linedraw()), NA)
  expect_error(print(base_p + theme_classic()), NA)
})
