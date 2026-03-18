# Development Plan: Axis Break Symbols in ggbreak

## 1. Background & Objective
**Issue:** [#76](https://github.com/YuLab-SMU/ggbreak/issues/76)
**Objective:** Provide users with the capability to optionally render standard axis break symbols (such as double-slash `//`, zig-zag, or wave) at the exact locations where axes are broken.
**Problem Context:** Users cannot simply draw these using `geom_segment()` because `ggbreak` trims the data falling within the break gap and manipulates the layout via `patchwork`/`grid` using `plot.margin`. The area between subplots is outside the `ggplot2` standard drawing panel.

## 2. Proposed Architecture & Approach
The most robust and native way to solve this in the `ggplot2`/`grid` ecosystem is to define a **custom theme element** (e.g., `element_break_symbol`). This follows the exact same philosophy recently used for `element_partial_rect` in PR 81.

### 2.1 Core Components
1. **Custom Theme Element (`element_break_symbol`)**:
   - Inherits from `element_line` or `element`.
   - Stores parameters like symbol type (`"slash"`, `"zigzag"`), line width, color, and which edges it should be applied to.
2. **Grob Constructor (`element_grob.element_break_symbol`)**:
   - An S3 method that takes the theme element and returns a `grid` object (grob).
   - For a `"slash"` symbol (`//`), it will generate a `grobTree` consisting of two parallel `segmentsGrob` items positioned slightly outside the exact panel boundary.
3. **Integration in `grid-draw-utilities.R`**:
   - Modify the functions that assemble subplot themes (e.g., `subplot_theme()`, `subplot_theme_2d()`).
   - If the user specifies they want break symbols (e.g., via a new parameter in `scale_x_break(..., symbol = "slash")` or via global `theme(axis.break.symbol = ...)`), the subplot themes will be injected with this custom element at the boundaries adjacent to the break.

## 3. Implementation Steps

### Step 1: Define the Theme Element
Create `element_break_symbol()` in a new or existing utility file (e.g., `R/theme-elements.R`).
```R
element_break_symbol <- function(symbol = "slash", colour = "black", linewidth = 0.5, linetype = 1, sides = "right") {
    structure(
        list(symbol = symbol, colour = colour, linewidth = linewidth, linetype = linetype, sides = sides),
        class = c("element_break_symbol", "element")
    )
}
```

### Step 2: Create the S3 `element_grob` Method
Implement `element_grob.element_break_symbol(element, ...)`.
- **Logic:** Based on `element$sides` (e.g., `"right"` for the left subplot of an x-axis break), calculate the coordinates for the symbols.
- **Example for `slash` on the right side:** Draw two short diagonal lines across the right border of the panel. Use `grid::segmentsGrob()`.
- **Note:** `clip = "off"` must be ensured in the subplots so the grob can extend into the margin (the gap between subplots).

### Step 3: Register in NAMESPACE
Add `S3method(element_grob, element_break_symbol)` to the `NAMESPACE` file.

### Step 4: Inject into Subplot Themes
Modify `R/grid-draw-utilities.R` (specifically `subplot_theme()` and `subplot_theme_2d()`).
- Currently, these functions handle `first_margin_theme`, `other_margin_theme`, etc.
- We need to add logic: If `axis_break` contains a `symbol` parameter (or if we detect a specific theme setting), attach the `element_break_symbol` to the appropriate side.
  - For `type = "first"` (leftmost plot in x-break): add symbol to the `"right"` side.
  - For `type = "other"` (middle plots): add symbol to both `"left"` and `"right"` sides.
  - For `type = "last"` (rightmost plot): add symbol to the `"left"` side.

### Step 5: Update `scale_x_break` / `scale_y_break` API
Add a `symbol` parameter to the break functions (defaulting to `NULL` or `FALSE` for backward compatibility).
```R
scale_x_break <- function(breaks, scales = "fixed", space = 0.1, expand = TRUE, ticklabels = NULL, symbol = NULL) { ... }
```

## 4. Testing Strategy

### 4.1 Unit Tests
Create `tests/test-3-break-symbols.R`.
1. **Basic rendering:** Test `scale_x_break(..., symbol = "slash")` and ensure it renders without throwing errors.
2. **Axis variants:** Test on `scale_y_break`.
3. **Dual axis:** Test combining both x and y breaks with symbols.
4. **Theme propagation:** Verify that changing `linewidth` or `color` of the axis lines appropriately cascades to the symbol (or that the symbol can be styled independently).

### 4.2 Visual/Regression Tests
- Generate a test plot with a large gap and a `"slash"` symbol.
- Visually inspect the output using `grid.draw()` to ensure the symbols align correctly across the gap and do not get clipped by the panel boundaries.

## 5. Documentation
- Update `vignettes/ggbreak.Rmd` with a new section: **Feature: Axis Break Symbols**.
- Add a demo showing how to use the `symbol` argument to draw `//` across a broken axis.
- Update `roxygen2` comments for `scale_x_break` and `scale_y_break` explaining the new `symbol` parameter.

## 6. Effort and Timeline Estimate
- **Core Engine (Grob & Element):** 1-2 hours
- **Theme Injection & Layout logic:** 2-3 hours
- **Testing & Edge Cases (Clipping, Coord flip):** 2 hours
- **Documentation:** 1 hour
- **Total:** ~1 working day.
