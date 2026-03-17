library(ggplot2)
devtools::load_all(".")

# Test-2: check that the grid drawing method for 2D grids (with both x and y breaks) is working correctly
# - This is a regression test to ensure that the new grid drawing method for 2D grids (with both x and y breaks) 
#   is working correctly and does not break existing functionalities
# - It also checks that the new dual-axis break feature is working as expected when rendered, including with different themes and parameters.
#   Possibilities to see the output at tests/test-outputs/ after running this test.

outdir <- file.path("tests", "test-outputs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Helper function to save test outputs
save_test <- function(p, name) {
    path <- file.path(outdir, paste0(name, ".png"))
    ggsave(path, plot = p, width = 8, height = 6, dpi = 150)
    message("Saved: ", path)
}

# ===== OLD: check consistency with existing functionalities
cat("\n=== Regression 1: x-break only ===\n")
p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_x_break(c(3, 4))
g1 <- grid.draw(p1, recording = FALSE)
save_test(g1, "reg_x_break")
cat("  OK\n")

cat("\n=== Regression 2: y-break only ===\n")
p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_y_break(c(25, 35))
g2 <- grid.draw(p2, recording = FALSE)
save_test(g2, "reg_y_break")
cat("  OK\n")

cat("\n=== Regression 3: multiple x-breaks ===\n")
p3 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4)) + scale_x_break(c(5, 6))
g3 <- grid.draw(p3, recording = FALSE)
save_test(g3, "reg_multi_x_break")
cat("  OK\n")

# ===== NEW: dual-axis breaks 
cat("\n=== Dual 1: basic x + y break ===\n")
p4 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35))
g4 <- tryCatch(grid.draw(p4, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g4)) {
    save_test(g4, "dual_basic")
    cat("  OK\n")
}

cat("\n=== Dual 2: y + x order ===\n")
p5 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_y_break(c(25, 35)) + scale_x_break(c(3, 4))
g5 <- tryCatch(grid.draw(p5, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g5)) {
    save_test(g5, "dual_yx_order")
    cat("  OK\n")
}

cat("\n=== Dual 3: scatter with outlier clusters ===\n")
set.seed(42)
df <- data.frame(
    x = c(rnorm(50, 5, 1), rnorm(10, 50, 2)),
    y = c(rnorm(50, 10, 2), rnorm(10, 100, 5))
)
p6 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 45)) + scale_y_break(c(20, 90))
g6 <- tryCatch(grid.draw(p6, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g6)) {
    save_test(g6, "dual_scatter_outliers")
    cat("  OK\n")
}

cat("\n=== Dual 4: with title and theme ===\n")
p7 <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
    labs(title = "Dual axis break", subtitle = "with color") +
    theme_minimal()
g7 <- tryCatch(grid.draw(p7, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g7)) {
    save_test(g7, "dual_with_theme")
    cat("  OK\n")
}

cat("\n=== Dual 5: space parameter ===\n")
p8 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 45), space = 0.5) +
    scale_y_break(c(20, 90), space = 0.5)
g8 <- tryCatch(grid.draw(p8, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g8)) {
    save_test(g8, "dual_space")
    cat("  OK\n")
}

cat("\n=== Dual 6: scales parameter ===\n")
p9 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 45), scales = 0.3) +
    scale_y_break(c(20, 90), scales = 0.3)
g9 <- tryCatch(grid.draw(p9, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g9)) {
    save_test(g9, "dual_scales")
    cat("  OK\n")
}

cat("\n=== Dual 7: multiple breaks on BOTH axes ===\n")
p10 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 20)) + scale_x_break(c(30, 45)) +
    scale_y_break(c(20, 40)) + scale_y_break(c(60, 90))
g10 <- tryCatch(grid.draw(p10, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g10)) {
    save_test(g10, "dual_multi_breaks")
    cat("  OK\n")
}

cat("\n=== Dual 8: other themes test ===\n")
p11 <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
    labs(title = "Dual axis break", subtitle = "with color") +
    theme_bw()
g11 <- tryCatch(grid.draw(p11, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g11)) {
    save_test(g11, "dual_with_theme_bw")
    cat("  OK\n")
}
p12 <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
    labs(title = "Dual axis break", subtitle = "with color") +
    theme_linedraw()
g12 <- tryCatch(grid.draw(p12, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g12)) {
    save_test(g12, "dual_with_theme_linedraw")
    cat("  OK\n")
}
p13 <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
    labs(title = "Dual axis break", subtitle = "with color") +
    theme_classic()
g13 <- tryCatch(grid.draw(p13, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g13)) {
    save_test(g13, "dual_with_theme_classic")
    cat("  OK\n")
}

cat("\n=== Dual 9: multiple breaks + theme test ===\n")
p14 <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_break(c(10, 20)) + scale_x_break(c(30, 45)) +
    scale_y_break(c(20, 40)) + scale_y_break(c(60, 90)) +
    theme_linedraw()
g14 <- tryCatch(grid.draw(p14, recording = FALSE), error = function(e) {
    cat("  ERROR:", e[["message"]], "\n"); NULL
})
if (!is.null(g14)) {
    save_test(g14, "dual_multi_breaks_theme")
    cat("  OK\n")
}

cat("\n=== All Phase 2 tests completed ===\n")
cat("Check tests/test-outputs/ for PNG files.\n")
