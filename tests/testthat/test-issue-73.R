## Saving a broken plot with pdf() used to produce an extra blank page, #73.
##
## ggbreak builds the subplots while the device is already open; building a
## ggplot on a device that has no current page makes R start one (converting
## `line`/`strwidth` units needs a page) and nothing is drawn on it, so the
## page that `print()` starts afterwards is page two.
##
## `onefile = FALSE` writes one file per page (an opened device already leaves
## one empty file behind), so counting the files tells us how many pages the
## plot really needed.

pages_of <- function(expr) {
    dir <- tempfile("ggbreak-pages-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE))
    grDevices::pdf(file.path(dir, "p%03d.pdf"), onefile = FALSE,
                   width = 5, height = 4)
    force(expr)
    grDevices::dev.off()
    length(list.files(dir, pattern = "^p[0-9]+\\.pdf$"))
}

set.seed(42)
d <- data.frame(x = runif(60), y = c(runif(30), runif(30) + 20),
                lev = factor(rep(c("low", "medium", "high", "xl"), 15),
                             levels = c("low", "medium", "high", "xl")))

test_that("a broken plot takes a single page", {
    ## the file an opened device leaves behind, i.e. the page budget to start with
    expect_equal(pages_of(NULL), 1)

    expect_equal(pages_of(print(ggplot(d, aes(x, y)) + geom_point() +
                                scale_y_break(c(5, 15)))), 1)
    expect_equal(pages_of(print(ggplot(d, aes(y, x)) + geom_point() +
                                scale_x_break(c(5, 15)))), 1)
    expect_equal(pages_of(print(ggplot(d, aes(x, y)) + geom_point() +
                                scale_wrap(2))), 1)
    expect_equal(pages_of(print(ggplot(d, aes(x, y)) + geom_point() +
                                scale_y_cut(10))), 1)
})

test_that("both axes broken still takes a single page", {
    p <- ggplot(d, aes(x, y)) + geom_point() +
        scale_x_break(c(0.3, 0.6)) + scale_y_break(c(5, 15))
    expect_equal(pages_of(print(p)), 1)
})

test_that("a discrete break takes a single page", {
    p <- ggplot(d, aes(lev, x)) + geom_point() + scale_x_break(c("medium", "xl"))
    expect_equal(pages_of(print(p)), 1)
})

test_that("grid.draw(), the path ggsave() takes, needs a single page too", {
    p <- ggplot(d, aes(x, y)) + geom_point() + scale_y_break(c(5, 15))
    expect_equal(pages_of(grid::grid.draw(p)), 1)
})
