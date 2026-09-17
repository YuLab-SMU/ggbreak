library(grid)
library(ggplot2)

## #84: the break points are fed to the scale transform of the axis.  A datetime
## axis uses `transform_time()`, which accepts `POSIXct` objects only, while its
## range is stored as numeric seconds since the epoch.  Passing break points as
## `Date`, as character or as numeric therefore failed with
## "`transform_time()` works with objects of class <POSIXct> only", even though
## all of them are natural ways to specify a datetime break.

## 10 days of data, so that a midnight break is inside the axis range
dt <- as.POSIXct('2026-09-10 12:00:00', tz = 'UTC')
dat <- data.frame(t = dt + (0:239) * 3600, y = rep(c(1:5, 60:64), 24))
b_date  <- as.Date(c('2026-09-13', '2026-09-15'))
b_posix <- as.POSIXct(c('2026-09-13 00:00:00', '2026-09-15 00:00:00'), tz = 'UTC')
b_char  <- c('2026-09-13 00:00:00', '2026-09-15 00:00:00')
b_num   <- as.numeric(b_posix)

test_that('break points of a datetime axis accept any datetime-like class (#84)', {
  base <- ggplot(dat, aes(t, y)) + geom_point()

  ## `POSIXct` always worked, the other classes all used to error
  for (breaks in list(b_posix, b_date, b_char, b_num)) {
    expect_silent(grid.draw(base + scale_x_break(breaks), recording = FALSE))
    expect_silent(grid.draw(base + scale_x_cut(breaks), recording = FALSE))
  }
})

test_that('the y axis of a datetime plot accepts datetime-like break points (#84)', {
  base <- ggplot(dat, aes(y, t)) + geom_point()

  for (breaks in list(b_posix, b_date, b_char, b_num)) {
    expect_silent(grid.draw(base + scale_y_break(breaks), recording = FALSE))
    expect_silent(grid.draw(base + scale_y_cut(breaks), recording = FALSE))
  }
})

test_that('a break point is converted to the instant the scale displays (#84)', {
  to_time <- ggbreak:::as_datetime_break

  ## `POSIXct` is already the expected class and must be passed through untouched
  expect_identical(to_time(b_posix, tz = 'UTC'), b_posix)

  ## a `Date` break means midnight of that day *on the axis*, so it has to be
  ## read in the timezone of the scale rather than always in UTC.  Only the
  ## instant matters, the `tzone` attribute is the one of the scale.
  expect_identical(to_time(as.Date('2026-09-13'), tz = 'UTC'),
                   as.POSIXct('2026-09-13', tz = 'UTC'))
  expect_equal(as.numeric(to_time(as.Date('2026-09-13'), tz = 'Asia/Shanghai')),
               as.numeric(as.POSIXct('2026-09-12 16:00:00', tz = 'UTC')))

  ## character break points are wall clock times, just like `Date` ones
  expect_equal(as.numeric(to_time('2026-09-13 06:00:00', tz = 'Asia/Shanghai')),
               as.numeric(as.POSIXct('2026-09-12 22:00:00', tz = 'UTC')))

  ## numeric break points are seconds since the epoch, mirroring how `as.Date()`
  ## reads numeric break points as days since the epoch; the timezone only
  ## affects the label, not the instant
  expect_identical(to_time(0, tz = 'UTC'), as.POSIXct('1970-01-01', tz = 'UTC'))
  expect_identical(to_time(b_num, tz = 'UTC'), b_posix)
})

test_that('a `Date` axis is unaffected by the datetime conversion (#84)', {
  ## `Date` axes keep using `as.Date()`; this used to work and has to keep doing so
  ddat <- data.frame(d = as.Date('2026-09-10') + 0:9,
                     y = c(1, 2, 3, 4, 5, 60, 70, 80, 90, 100))
  base <- ggplot(ddat, aes(d, y)) + geom_point()

  for (breaks in list(as.Date(c('2026-09-13', '2026-09-15')),
                      c('2026-09-13', '2026-09-15'),
                      as.numeric(as.Date(c('2026-09-13', '2026-09-15'))))) {
    expect_silent(grid.draw(base + scale_x_break(breaks), recording = FALSE))
    expect_silent(grid.draw(base + scale_x_cut(breaks), recording = FALSE))
  }
})

test_that('a datetime break outside the plot range is still reported (#84)', {
  base <- ggplot(dat, aes(t, y)) + geom_point()

  ## the fix must not turn the range check of #43 into a no-op
  expect_error(grid.draw(base + scale_x_break(as.Date(c('2027-01-01', '2027-01-02'))),
                         recording = FALSE),
               'not in the plot range')
})

## A plot that never calls `scale_*_datetime()` has no scale of its own, and the
## subplots used to be given a plain continuous scale instead, so the axis was
## drawn as seconds since the epoch from then on.  The subplots have to get a
## scale of the same kind as the automatic one.
axis_tick_labels <- function(plot, side = 'b') {
  g <- grid.force(ggplotGrob(suppressMessages(grid.draw(plot, recording = FALSE))))
  out <- character()
  walk <- function(grob, path = '') {
    if (inherits(grob, 'text') && grepl(paste0('/axis-', side, '-'), path) &&
        !is.null(grob$label)) {
      out <<- c(out, as.character(grob$label))
    }
    if (inherits(grob, 'gTree') && length(grob$children) > 0) {
      for (i in seq_along(grob$children)) {
        walk(grob$children[[i]], paste0(path, '/', names(grob$children)[i]))
      }
    }
  }
  walk(g)
  out[nzchar(out)]
}

test_that('a datetime axis without an explicit scale is not drawn as numbers (#84)', {
  breaks <- as.Date(c('2026-09-13', '2026-09-15'))

  ## the labels have to be the ones of the very same plot with an explicit
  ## `scale_*_datetime()`, which has always worked
  base <- ggplot(dat, aes(t, y)) + geom_point()
  for (b in list(breaks, as.character(breaks), as.numeric(b_posix))) {
    expect_identical(axis_tick_labels(base + scale_x_break(b)),
                     axis_tick_labels(base + scale_x_datetime() + scale_x_break(b)))
  }
  expect_identical(axis_tick_labels(base + scale_x_cut(breaks)),
                   axis_tick_labels(base + scale_x_datetime() + scale_x_cut(breaks)))
  expect_identical(axis_tick_labels(base + scale_wrap(2)),
                   axis_tick_labels(base + scale_x_datetime() + scale_wrap(2)))
  expect_identical(axis_tick_labels(base + scale_x_break(breaks) + scale_y_break(c(30, 40))),
                   axis_tick_labels(base + scale_x_datetime() + scale_x_break(breaks) +
                                        scale_y_break(c(30, 40))))

  ybase <- ggplot(dat, aes(y, t)) + geom_point()
  expect_identical(axis_tick_labels(ybase + scale_y_break(breaks), side = 'l'),
                   axis_tick_labels(ybase + scale_y_datetime() + scale_y_break(breaks),
                                    side = 'l'))
  expect_identical(axis_tick_labels(ybase + scale_y_cut(breaks), side = 'l'),
                   axis_tick_labels(ybase + scale_y_datetime() + scale_y_cut(breaks),
                                    side = 'l'))

  ## and it is not only about the class of the scale: seconds are long numbers
  labels <- axis_tick_labels(base + scale_x_break(breaks))
  expect_true(length(labels) > 0)
  expect_false(any(grepl('^[0-9]+$', labels)))
})

test_that('a `Date` axis without an explicit scale keeps its dates (#84)', {
  ddat <- data.frame(d = as.Date('2026-09-10') + 0:9,
                     y = c(1, 2, 3, 4, 5, 60, 70, 80, 90, 100))
  base <- ggplot(ddat, aes(d, y)) + geom_point()
  breaks <- as.Date(c('2026-09-13', '2026-09-15'))

  expect_identical(axis_tick_labels(base + scale_x_break(breaks)),
                   axis_tick_labels(base + scale_x_date() + scale_x_break(breaks)))
  expect_identical(axis_tick_labels(base + scale_x_cut(breaks)),
                   axis_tick_labels(base + scale_x_date() + scale_x_cut(breaks)))

  ## a `Date` axis is drawn as days since the epoch when it degrades to numbers
  labels <- axis_tick_labels(base + scale_x_break(breaks))
  expect_true(length(labels) > 0)
  expect_false(any(grepl('^[0-9]+([.][0-9]+)?$', labels)))
})

test_that('the automatic scale keeps its position under `coord_flip()` (#84)', {
  set.seed(1)
  df <- data.frame(x = rnorm(100, 10, 3), y = rnorm(100, 10, 3))
  base <- ggplot(df, aes(x, y)) + geom_point()

  ## building a flipped plot moves the scale to the other side of the panel, and
  ## putting such a scale back drops the guide, so the axis lost all its labels
  ## and only a single one per panel was left
  for (p in list(base + coord_flip() + scale_wrap(2),
                 base + coord_flip() + scale_x_cut(12))) {
    expect_true(length(axis_tick_labels(p, side = 'l')) > 3)
  }
  expect_identical(axis_tick_labels(base + coord_flip() + scale_wrap(2), side = 'l'),
                   axis_tick_labels(base + coord_flip() + scale_x_continuous() +
                                        scale_wrap(2), side = 'l'))
})
