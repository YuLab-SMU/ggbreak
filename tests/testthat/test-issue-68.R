library(grid)
library(ggplot2)

## #68: a discrete axis has no transform, so `ggrange2()` returns
## `flagrev = NULL` for it and `grid.draw.ggbreak()` died with
## "argument is of length zero" on `if (rng$flagrev == "reverse")`.  The panel
## limits of a discrete scale cannot be set with `coord_cartesian()` either, so
## a discrete axis is split by subsetting the levels of its scale instead.
##
## The break points are level names and are read exactly like continuous ones:
## levels lying strictly between the two ends of a break interval are dropped,
## so two adjacent levels only insert a gap, which is what #68 asks for.

dat <- data.frame(x = rep(letters[1:5], 4),
                  y = c(1, 2, 3, 4, 5, 60, 70, 80, 90, 100,
                        6, 7, 8, 9, 10, 20, 30, 40, 50, 55))

test_that('a discrete axis accepts break points given as level names (#68)', {
  base <- ggplot(dat, aes(x, y)) + geom_col()

  ## a break between two adjacent levels, the case in the report
  expect_silent(suppressWarnings(grid.draw(base + scale_x_break(c("a", "b")), recording = FALSE)))
  ## a break that drops the levels in between
  expect_silent(suppressWarnings(grid.draw(base + scale_x_break(c("b", "d")), recording = FALSE)))
  ## several breaks
  expect_silent(suppressWarnings(grid.draw(base + scale_x_break(c("b", "c")) +
                            scale_x_break(c("d", "e")), recording = FALSE)))
  ## a break symbol, a non default scale and a flipped coordinate system
  expect_silent(suppressWarnings(grid.draw(base + scale_x_break(c("a", "b"), symbol = "slash"),
                          recording = FALSE)))
  expect_silent(suppressWarnings(grid.draw(base + scale_x_break(c("a", "b"), scales = "free"),
                          recording = FALSE)))
  expect_silent(suppressWarnings(grid.draw(base + scale_x_break(c("a", "b")) + coord_flip(),
                          recording = FALSE)))
  ## facetting and a legend
  expect_silent(suppressWarnings(grid.draw(
    ggplot(transform(dat, g = rep(c("p", "q"), 10)), aes(x, y, fill = g)) +
      geom_col() + facet_wrap(~g) + scale_x_break(c("a", "b")),
    recording = FALSE)))
})

test_that('the y axis of a horizontal bar plot accepts a break (#68)', {
  ## the report asks for a break between the levels of the y axis
  bar <- ggplot(dat, aes(y, x)) + geom_col()

  expect_silent(suppressWarnings(grid.draw(bar + scale_y_break(c("a", "b")), recording = FALSE)))
  expect_silent(suppressWarnings(grid.draw(bar + scale_y_break(c("b", "d")), recording = FALSE)))
  expect_silent(suppressWarnings(grid.draw(bar + scale_y_break(c("a", "b")) + coord_flip(),
                          recording = FALSE)))
})

test_that('the panels of a discrete break hold the levels they should (#68)', {
  idx <- function(breaks, levels = letters[1:5]) {
    vapply(ggbreak:::discrete_break_index(breaks, levels),
           function(i) paste(levels[i], collapse = ""), character(1))
  }

  ## two adjacent levels only insert a gap, no level is dropped
  expect_identical(idx(list(c("a", "b"))), c("a", "bcde"))
  ## the levels strictly inside the interval are dropped
  expect_identical(idx(list(c("b", "d"))), c("ab", "de"))
  expect_identical(idx(list(c("a", "e"))), c("a", "e"))
  ## two separate intervals
  expect_identical(idx(list(c("b", "c"), c("d", "e"))), c("ab", "cd", "e"))
  ## overlapping intervals are merged, the way continuous ones are
  expect_identical(idx(list(c("b", "c"), c("c", "e"))), c("ab", "e"))
})

test_that('the levels keep their own order, not the alphabetical one (#68)', {
  ## `merge_intervals()` sorts alphabetically, which would put "high" before
  ## "low"; the order of the axis is the one of the levels
  lv <- c("low", "medium", "high", "xl")
  idx <- ggbreak:::discrete_break_index(list(c("medium", "xl")), lv)
  expect_identical(vapply(idx, function(i) paste(lv[i], collapse = ""), character(1)),
                   c("lowmedium", "xl"))
})

test_that('breaks that are not levels are reported (#68)', {
  base <- ggplot(dat, aes(x, y)) + geom_col()

  expect_error(grid.draw(base + scale_x_break(c("a", "zz")), recording = FALSE),
               'not levels of the')
  expect_error(grid.draw(base + scale_x_break(c("d", "b")), recording = FALSE),
               'not in ascending order')
})

test_that('scale_x_cut() works on a discrete axis (#68)', {
  base <- ggplot(dat, aes(x, y)) + geom_col()

  ## unlike a break, a cut keeps everything and shares the level it cuts at
  idx <- ggbreak:::discrete_cut_index(c("b", "d"), letters[1:5])
  expect_identical(vapply(idx, function(i) paste(letters[1:5][i], collapse = ""),
                          character(1)),
                   c("ab", "bcd", "de"))

  expect_silent(suppressWarnings(grid.draw(base + scale_x_cut(c("b", "d")), recording = FALSE)))
  expect_silent(suppressWarnings(grid.draw(ggplot(dat, aes(y, x)) + geom_col() +
                            scale_y_cut(c("b", "d")), recording = FALSE)))
})

test_that('a continuous axis is unaffected by the discrete path', {
  set.seed(2019 - 01 - 19)
  d <- data.frame(x = 1:20,
                  y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22))

  ## these keep using `coord_cartesian()`
  expect_silent(suppressWarnings(grid.draw(ggplot(d, aes(x, y)) + geom_col() +
                            scale_y_break(c(7, 17)), recording = FALSE)))
  expect_silent(suppressWarnings(grid.draw(ggplot(d, aes(x, y)) + geom_col() +
                            scale_y_cut(breaks = c(7, 18)), recording = FALSE)))
  ## a break outside the range is still reported
  expect_error(grid.draw(ggplot(d, aes(x, y)) + geom_col() +
                           scale_y_break(c(100, 200)), recording = FALSE),
               'not in the plot range')
})
