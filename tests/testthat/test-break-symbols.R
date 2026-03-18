library(ggplot2)
library(ggbreak)

test_that("scale_x_break with symbol renders correctly", {
  p <- ggplot(mpg, aes(displ, hwy)) + 
    geom_point() +
    scale_x_break(c(3, 4), symbol = "slash") +
    theme_bw()
  
  # Just verify it builds without error
  expect_error(print(p), NA)
})

test_that("scale_y_break with symbol renders correctly", {
  p <- ggplot(mpg, aes(displ, hwy)) + 
    geom_point() +
    scale_y_break(c(25, 30), symbol = "slash") +
    theme_linedraw()
  
  expect_error(print(p), NA)
})

test_that("Dual break with symbols renders correctly", {
  p <- ggplot(mpg, aes(displ, hwy)) + 
    geom_point() +
    scale_x_break(c(3, 4), symbol = "slash") +
    scale_y_break(c(25, 30), symbol = "slash") +
    theme_bw()
  
  expect_error(print(p), NA)
})
