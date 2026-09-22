# AGENTS.md

Guidance for coding agents working on **ggbreak** — `scale_x_break()` / `scale_y_break()`,
`scale_wrap()` and `scale_x_cut()` / `scale_y_cut()` for ggplot2.

## The one invariant

**A break is drawn by clipping, not by cutting data.** Every window receives the whole
dataset, and each panel's clip path hides the part that belongs to the other windows.

Almost every bug in this package is a consequence of that decision, so read it as a rule:

- It is *why* layers keep working after a break — the selling point over "draw two plots
  and paste them together". Do not "fix" anything by subsetting the data: dropping a row
  drops a whole bar, and cutting a line at the panel edge changes its slope.
- Hidden geometry still reaches the device. A bar from 0 to 990 is, in every window, a
  rectangle as tall as the unbroken axis; the window showing its top carries one reaching
  ~1.8 panel heights above the panel. A vector device writes that out, and Illustrator
  then refuses to paste the file. Hence `clamp_panel_geometry()` (#16).
- Because a line's grob keeps the *whole* line and is only clipped at draw time, where a
  line leaves one window and enters the next can be read back off the grob. That is how
  `bridge = TRUE` joins a line across a break (#33) — and why lines are never clamped.

## Rendering pipeline

`print.ggbreak()` → `grid.draw.ggbreak()` (same shape for `ggwrap` / `ggcut`):

1. `newpage_if_recording(recording)` (#73) — start the page *before* building the
   subplots, otherwise building opens a blank page that is left behind.
2. `check_scale_combination(x, class)` (#31) — refuse if the plot still carries another
   ggbreak class. Must run before the method strips its own class.
3. Build the subplots, then `assemble_windows(gglist, sizes, along, legendpos)` (#55) —
   the single assembly point, returns `list(plot =, guide =)`. A faceted plot is bound
   into one gtable with `rbind()` / `cbind()`; anything else goes through
   `aplot::plot_list(output = "patchwork")`. The nested path returns a **gtable**, the
   plain path a patchwork object.
4. `ggplotify::as.ggplot()` → `set_label()`, which forwards the outer theme, margin and
   background (#57, #71, #52).
5. Every actual draw goes through `drawable_grob(g)`
   (= `clamp_panel_geometry(ggplotGrob(g))`, #16) — six call sites.

There is a **second, separate path** for `ggplotGrob()` (#46), used by patchwork and
cowplot: `ggplot_build.ggbreak*` tags the built object, `ggplot_gtable.ggbreak_built`
returns the assembled figure, and `align_assembled_figure()` moves the axis out of the
`panel` cell so the layout aligns the plot area rather than the plot area plus axis.
`print()` and `ggsave()` must stay byte-for-byte unchanged by anything done here.

## Code map

| File | Lines | Responsibility |
|---|---|---|
| `R/method-grid-draw.R` | 721 | the three `grid.draw` methods; `newpage_if_recording()`, `check_scale_combination()`, `assemble_windows()` |
| `R/grid-draw-utilities.R` | 1425 | the workhorse: `subplot_theme()`, `axis_theme()`, `auto_axis_scale()`, `add_expand()`, `other_axis_limits()`; `nest_facet_windows()` (#55); `bottom_legend()` / `hoist_bottom_axis_title()` (#53); `add_line_bridges()` / `polyline_groups()` (#33); `clamp_panel_geometry()` / `drawable_grob()` (#16) |
| `R/utilities.R` | 501 | range computation, `has_secondary_axis()`, `set_label()` (the outer-theme / margin / background forwarding point) |
| `R/method-ggplot-grob.R` | 175 | the `ggplotGrob()` path (#46) |
| `R/method-ggplot-add.R` | 107 | `ggplot_add.*`; each scale pushes **one class onto the plot** |
| `R/scales.R` | 142 | the five exported scales |
| `R/method-print.R` | 17 | `print.*` → `grid.draw(..., recording = TRUE)` |

`hoist_bottom_axis_title()`, `add_facet_guide_box()` and `blank_patch_background()` all
accept a gtable as well as a patchwork object.

## Scale signatures

```r
scale_x_break(breaks, scales = "fixed", ticklabels = NULL, expand = TRUE,
              space = 0.1, symbol = NULL, bridge = FALSE)
scale_y_break(breaks, scales = "fixed", ticklabels = NULL, expand = TRUE,
              space = 0.1, symbol = NULL, bridge = FALSE)
scale_x_cut(breaks, which = NULL, scales = NULL, expand = FALSE, space = 0.1)
scale_y_cut(breaks, which = NULL, scales = NULL, expand = FALSE, space = 0.1)
scale_wrap(n)
```

`scales` is routinely documented wrong, including in old blog posts. Its real meaning
(see `compute_relative_range_()`):

- `"fixed"` (**the default**) — each window gets a share **proportional to the range it
  covers**; the width relationship of the unbroken axis is preserved.
- `"free"` — all windows get an equal share. This is identical to the number `1`.
- a number `n` — each window gets `n ×` the share of the **first** window.

`space` is in centimetres and is the blank space inserted between windows.

**How to read a break back off a plot.** `scale_x_break()`/`scale_y_break()` return the
`ggbreak_params` object itself, and `ggplot_add.ggbreak_params()` keeps it in an
**attribute** of the plot -- `attr(p, "axis_break_x")` / `attr(p, "axis_break_y")` -- not as a
scale. On a `ggbreak` plot `length(p$scales$scales)` is **0**, so filtering the scales list
for `ggbreak_params` finds nothing. `extract_axis_break()` takes that params object, or a
`list` of them when there is more than one break on the same axis, and is the adapter to
use. Handing it a *plot* instead dies in `S7::prop(x, "meta")`, because the plot falls
through to `object[[1]]`.

## Build and test

- `make rd` runs `devtools::document()`. **Never hand-edit `man/*.Rd` or `NAMESPACE`.**
  Edit the roxygen in `R/*.R` and run `make rd`. It re-flows `importFrom` into one block
  per package and bumps `Config/roxygen2/version` to 8.1.0 — that is pure formatting,
  commit it rather than reverting.
- **The suite runs under `R CMD check`** — the `tests/testthat.R` driver was added in
  `61e6ee8`. Until then the 22 files shipped in the tarball and were executed by nothing:
  the check log had **no "checking tests" line at all**, so "Status: OK" said nothing about
  them, and `test_dir()` was the only way to run them. Locally:
  `testthat::test_dir("tests/testthat")` (about 2m40s; check is ~4m31s in total). The suite
  is **0 failed / 0 warning / 0 skipped** as of `61e6ee8`, so a failure is yours — there is
  no known-failing test to step over any more. `test-api-attributes.R:18` used to fail, and
  it was a wrong assertion rather than a known-bad test; do not take a red test for granted
  without checking the contract it claims to pin down.
- **A test file must not call `pkgload::load_all()`.** It works under
  `testthat::test_dir()` from the package root and **aborts under `R CMD check`**, where the
  tests are run from a directory that holds no DESCRIPTION
  (`cli::cli_abort(..., class = "pkgload_no_desc")` at the top of the file, so the whole file
  reports as one failure). Three files did this and the suite read **FAIL 3** under check
  while `test_dir()` said 0. `pkgload` is not in `Suggests` either, so it was also an
  unstated dependency. The tests run against the **installed** package -- that is what CRAN
  checks, and it is what the other 19 files already do -- so `:::` works without it.
  Consequence for the local workflow: `test_dir()` now exercises whatever is installed, so
  `make install` first, or a fix that is committed but not installed will pass against the
  stale build.
- After adding an S3 method, check the "S3 generic/method consistency" result —
  `ggplot_build`'s generic is `function(plot, ...)`, so a method written as
  `function(plot)` raises a WARNING.
- `R CMD check --as-cran` needs network access; without a proxy it times out. Drop
  `--as-cran` when offline.
- `R CMD check <tarball>` needs an **absolute** path, because the tarball lands in the
  package directory rather than the working directory.

## Any change to rendering needs a pixel regression

Install the before and after builds into two separate libraries and diff the images:

```bash
git archive <rev> | tar -x -C /tmp/before
mkdir -p /tmp/Rlib-before && R CMD INSTALL --library=/tmp/Rlib-before /tmp/before
R_LIBS=/tmp/Rlib-before Rscript render-battery.R   # writes /tmp/out/before/*.png
```

then compare the two sets, e.g.

```python
# (abs(a - b).sum(axis=2) > 10).sum()  must be 0 on the default scenarios
```

Two things that will waste your afternoon:

- The render battery only covers the **`ggsave()` path**. A change that only adds
  `ggplotGrob()` behaviour is all-zero there and needs its own targeted check.
- Fix the seed and any timestamps in the battery (`set.seed()`, a literal
  `as.POSIXct("...")`). `Sys.time()` produces differences that are not real.

## Versioning, NEWS and commits

- `DESCRIPTION`'s `Version` is a **four-part development number** (`0.1.7.021`). Bump the
  last part by `+0.001` for every change. CRAN releases are three-part (`0.1.7`).
- `NEWS.md` keeps **one section per development cycle**, titled with the current
  development number. Append one `+` entry and bump the title by 0.001. Only when the
  release is blessed does the title become the three-part release number.
- Entry format: `+ description (YYYY-MM-DD, Ddd, #issue)` — newest entry at the top.
- Commit messages in English. Fix one issue at a time: commit it, reply on the issue
  citing the `master` hash, then move to the next.

## Pitfalls that have cost real time

- **`geom_col()`'s rect uses `just = c("left", "top")`, so `y` is the TOP EDGE, not the
  centre.** Reading it as the centre moves every bar by half its height — measured
  **15903 px** of difference. Restore by `just`: handle `centre`, `left|bottom` and `top`.
- **Lines are deliberately never clamped.** `bridge` reads the hidden piece off the
  `polylineGrob`; clamping it would break #33. Only rects, axis-aligned segments and
  two-level polygons are clamped.
- **Slanted segments are left alone** — only segments already running along an axis are
  safe to clamp, and each axis is tested separately.
- A slab entirely outside the panel must be pulled *to the margin*
  (`lo <- pmin(pmax(ends$lo, -margin), 1 + margin)`), not left where it is.
- `ggplotGrob()` is **not** a generic; its body is `ggplot_gtable(ggplot_build(x))`.
  `ggplot_build()` dispatches on the **plot's** class and `ggplot_gtable()` on the
  **built object's** class, so registering only `ggplot_gtable.ggbreak` never fires.
  `grid.draw.ggbreak()` is not exported — call it as
  `grid::grid.draw(p, recording = FALSE)` to go through dispatch, and wrap it in
  `suppressWarnings()` as `print.ggbreak()` does.
- One `polylineGrob` holds **every group** of a layer, delimited by `id` / `id.lengths`.
  Scanning by point order connects the last point of one group to the first of the next,
  inventing a segment that does not exist. Split by id and handle each group.
- `ggfun::is.ggbreak()` returns TRUE for `ggbreak`, `ggwrap` **and** `ggcut`. So
  `aplot::plot_list()` does not treat such a subplot as plain — it *draws* it. This is the
  root cause of #31, and it is also why `ggplot_add.gg()` must drop all three classes.
- `lapply()` drops the `gList` class, so `identical()` fails on a grob tree where nothing
  changed. Restore it (`attributes(out) <- attributes(kids)`).
- Panel backgrounds use **`npc`**, not `native` — filter them out of `native_coords()`.
- **Never use `scale_x_continuous()` to "supply" a missing scale** (#84). It overrides the
  auto-derived `ScaleContinuousDatetime` / `ScaleContinuousDate` and the axis turns into
  epoch numbers. Use `auto_axis_scale()`, which reads
  `ggplot_build(plot)$layout[["panel_scales_x"]][[1]]`. When putting such a scale back,
  reset `scale_obj$position` to `"bottom"` / `"left"`, or `coord_flip()` makes every axis
  label disappear.
- `as.numeric()` on a compound unit returns `1`, not the value. Use
  `convertX()` / `convertY(u, "native", valueOnly = TRUE)`.
- `expect_silent()` breaks on ggplot2's "Removed N rows containing missing values…"
  message. Do not wrap `ggsave()` or `grid.draw()` in it.
- In a gtable, `gtable_filter(trim = TRUE)` makes the slices' row/column counts differ,
  so position-based `unit.pmax()` pairs the wrong cells. Keeping the original shape and
  setting unused rows to `unit(0, "mm")` works. Also: **emptying a cell is not the same
  as deleting it** — a `nullGrob` still counts in the layout, so trim `$grobs` **and**
  `$layout`.
- Cells in one row/column share positions (`panel-1`, `axis-l-1`, `ylab-l-1`), so
  selecting by `t == r & b == r` picks the wrong ones — select by `$layout$name`.
- `facet_wrap()` **is not nested, so the whole wrap is repeated once per window.** A
  `facet_wrap(~ g, nrow = 1) + scale_x_break(c(6, 16))` draws `a b c | a b c` (6 panels,
  6 strips) where the equivalent `facet_grid(. ~ g)` draws `a a | b b | c c`. The reason is
  one line of code: `nest_facet_windows()` returns `NULL` unless the facet
  `inherits(..., "FacetGrid")`, because a wrap has no facet rows or columns to nest into
  and puts a strip above every panel. Pre-existing, not a regression — 0.1.7.13 and 0.2.0
  render it identically (0 px apart).
- **The wrap's `a b c | a b c` is the layout to keep -- do not "fix" it to nest.** Measured
  by letting `FacetWrap` through that one line in an isolated install: the `nrow = 1` wrap
  then renders **identically** to `facet_grid(. ~ g)` (0 px apart, same bytes), i.e.
  `a a | b b | c c`, where the x axis restarts once per group although the data holds one
  break. At `nrow = 2` it is worse than that -- the figure comes out **mangled**, the first
  row losing its strips while the second row's survive, which is exactly what the comment
  above `nest_facet_windows()` warns about. So the exclusion is right for two independent
  reasons, and nesting a wrap is **not** #55 / #17 left unfinished: those were about
  `facet_grid`, and the grid case is the one that was broken.
- `facet_grid()` is the one #55 / #17 fixed — before it, the strips were crammed into
  comma-joined cells and the rows came out `A B A B`.
- **`vignettes/ggbreak.Rmd` is CRLF, so edit it as bytes.** A plain text write
  (`open(p, "w")`, most editors) rewrites every line ending and turns a
  one-paragraph edit into a **1038-line** diff on a 520-line file. HEAD holds 520
  CRLF / 0 bare LF; read and write `"rb"`/`"wb"` and repair with
  `d.replace(b"\n", b"\r\n")` if you slipped. Check `git diff --stat` after
  every edit to this file — a diff far larger than the edit means this, not a
  real change.
- **The vignette's FAQ is a documentation surface that drifts.** `R CMD check`
  only proves the vignette *builds*, so its prose can keep teaching a workaround
  that `NEWS.md` for the same version calls fixed — 0.2.0 shipped a FAQ that still
  told users to `print()` before `cowplot::plot_grid()`. When a NEWS entry
  retires a workaround, grep the vignette for it in the same commit. Two chunks
  are `eval=FALSE` (lines 487, 514) and therefore run for nobody.

- **Only two entry points suppress warnings; a raw `grid.draw()` does not.** A broken plot
  legitimately drops rows that fall outside a window's scale, so ggplot2 says
  "Removed N rows containing missing values or values outside the scale range". Both
  user-facing paths wrap the drawing -- `print.ggbreak()`/`ggwrap`/`ggcut` in
  `R/method-print.R`, and the `ggplotGrob()` method at `R/method-ggplot-grob.R:56` -- so
  `print()`, `ggsave()`, `ggplotGrob()`, `patchwork` and `cowplot` are all quiet. A direct
  `grid::grid.draw(p, recording = FALSE)` is *not* wrapped, and that is the path the tests
  use, so a test that hits it must wrap the call itself (`suppressWarnings()`), as
  `test-issue-68.R` and the discrete-axis test of `test-issue-33.R` do. Measured: 0
  warnings from every entry point above, 4 from the raw draw of a discrete break.

## Environment

- R 4.6.1; ggplot2 4.0.3, patchwork 1.3.2, cowplot 1.2.0, aplot 0.3.1, ggfun 0.2.1,
  ggplotify 0.1.3.
- `gh issue close --reason "not planned"` — quote the reason, or the shell splits it.
- `gh issue view <n>` needs `--json`, or it fails on the deprecated `projectCards` field.
- An older ggbreak for comparison is on the CRAN Archive
  (`.../src/contrib/Archive/ggbreak/ggbreak_0.1.2.tar.gz`).
