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
