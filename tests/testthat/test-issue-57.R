## #57: a theme that is set globally (`theme_set()`) used to be applied to the
## *outer* ggplot as well.  That outer plot is only the container of the
## assembled subplots -- it is the panel that carries the `patchwork` figure --
## so a global `theme_bw()` drew a second panel border around the whole figure
## and a global `theme_classic()` a second pair of axis lines outside the plot.
## The panels, their borders and their axis lines belong to the subplots, and
## they have to look the same however the theme reaches them.
##
## The invariant these tests pin is that a theme applied with `theme_set()`
## renders exactly like the same theme applied to the plot: the figure is drawn
## from the same grobs either way.  Before the fix the global theme added a
## border, a background and axis lines at the outer level, so the two signatures
## differed.

suppressMessages(pkgload::load_all(quiet = TRUE))
suppressMessages(library(ggplot2))
suppressMessages(library(testthat))

## Names and classes of every grob the figure really draws, with the running
## numbers that `ggplot2` puts in a grob name stripped, so that two figures can
## be compared without depending on the order in which the grobs were created.
## Blank elements are skipped: they are the ones this issue is about.  Gtables
## are skipped too -- they are containers, and `ggplot2` numbers them by
## position.
grob_signature <- function(plot) {
    gt <- grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
    out <- character(0)
    walk <- function(z) {
        if (!inherits(z, "zeroGrob") && !inherits(z, "gtable")) {
            nm <- sub("\\.[0-9]+$", "", if (is.null(z$name)) "" else z$name)
            out <<- c(out, paste(class(z)[1], nm))
        }
        if (inherits(z, "gtable")) {
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(gt)
    sort(out)
}

## Draw the plot once with `th` set globally and once with the very same theme
## added to the plot, and return both signatures.  The global theme is put back
## to the default before the inline figure is drawn, otherwise it would reach
## the outer container of that figure as well and the two would agree whatever
## the package does.
signature_set_and_inline <- function(p, th) {
    theme_set(th)
    set_sig <- grob_signature(p)
    theme_set(theme_grey())
    inline_sig <- grob_signature(p + th)
    list(set = set_sig, inline = inline_sig)
}

set.seed(42)
d <- data.frame(x = 1:20, y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22))
base <- ggplot(d, aes(x, y)) + geom_point()

themes <- list(bw = theme_bw(), classic = theme_classic(),
               minimal = theme_minimal(), linedraw = theme_linedraw())

test_that("a globally set theme draws the same figure as the same inline theme, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    for (nm in names(themes)) {
        s <- signature_set_and_inline(base + scale_y_break(c(7, 17)), themes[[nm]])
        expect_identical(s$set, s$inline, info = nm)
    }
})

test_that("the invariant holds for every break flavour, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    broken <- list(
        y_break = base + scale_y_break(c(7, 17)),
        x_break = base + scale_x_break(c(7, 17)),
        y_cut = base + scale_y_cut(c(7, 17)),
        x_cut = base + scale_x_cut(c(7, 17)),
        wrap = base + scale_wrap(n = 3),
        dual = base + scale_x_break(c(7, 17)) + scale_y_break(c(7, 17)),
        flip = base + coord_flip() + scale_y_break(c(7, 17))
    )
    for (nm in names(broken)) {
        s <- signature_set_and_inline(broken[[nm]], themes$bw)
        expect_identical(s$set, s$inline, info = nm)
    }
})

test_that("an unbroken plot is left alone, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    s <- signature_set_and_inline(base, themes$classic)
    expect_identical(s$set, s$inline)
})

test_that("only the subplots carry a panel border, never the container, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    theme_set(theme_bw())
    sig <- grob_signature(base + scale_y_break(c(7, 17)))
    ## two subplots, each with the border of the global theme, and no third one
    expect_identical(sum(sig == "rect panel.border..rect"), 2L)
})

test_that("the default theme is untouched, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    sig <- grob_signature(base + scale_y_break(c(7, 17)))
    ## the default theme has no panel border to begin with
    expect_identical(sum(sig == "rect panel.border..rect"), 0L)
    ## and the figure still has an opaque background of its own
    expect_identical(sum(sig == "rect plot.background..rect"), 1L)
})
