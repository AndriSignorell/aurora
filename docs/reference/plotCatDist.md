# Categorical Distribution Plot

Visualizes the distribution of a categorical variable using horizontal
bar plots of absolute and relative frequencies. Optionally, cumulative
proportions (ECDF-style) can be displayed.

## Usage

``` r
plotCatDist(
  x,
  type = c("both", "freq", "perc"),
  ecdf = FALSE,
  col = "grey80",
  border = FALSE,
  maxlablen = 25,
  maxcats = NULL,
  main = NULL,
  ...
)
```

## Arguments

- x:

  a factor or character vector, a one-dimensional table of counts, or a
  numeric vector of precomputed frequencies. Unnamed numeric frequency
  vectors are labelled by their positions.

- type:

  character; one of `"both"`, `"freq"`, `"perc"`. Controls whether
  absolute frequencies, relative frequencies, or both are displayed.

- ecdf:

  logical; if `TRUE`, cumulative proportions are shown instead of simple
  relative frequencies.

- col:

  fill color for bars.

- border:

  logical; draw borders around bars.

- maxlablen:

  integer; maximum length of category labels before truncation.

- maxcats:

  optional maximum number of categories to display (truncates if
  exceeded).

- main:

  plot title.

- ...:

  further graphical parameters passed to
  [`par()`](https://rdrr.io/r/graphics/par.html).

## Value

Invisibly returns a list with frequencies and proportions.

## Details

The function produces horizontal bar plots:

- Absolute frequencies (counts)

- Relative frequencies (percentages) or cumulative proportions

If `type = "both"`, both views are shown side by side.

Long labels are truncated, and large category sets can be limited via
`maxcats`.

Raw categorical data and their pre-tabulated form are treated
identically. When categories are truncated via `maxcats`, proportions
remain based on the total frequency before truncation.

## See also

Other plot.univariate:
[`plotArea()`](https://andrisignorell.github.io/aurora/reference/plotArea.md),
[`plotBar()`](https://andrisignorell.github.io/aurora/reference/plotBar.md),
[`plotBox()`](https://andrisignorell.github.io/aurora/reference/plotBox.md),
[`plotDens()`](https://andrisignorell.github.io/aurora/reference/plotDens.md),
[`plotDensBox()`](https://andrisignorell.github.io/aurora/reference/plotDensBox.md),
[`plotDot()`](https://andrisignorell.github.io/aurora/reference/plotDot.md),
[`plotECDF()`](https://andrisignorell.github.io/aurora/reference/plotECDF.md),
[`plotFdist()`](https://andrisignorell.github.io/aurora/reference/plotFdist.md),
[`plotLines()`](https://andrisignorell.github.io/aurora/reference/plotLines.md),
[`plotQQ()`](https://andrisignorell.github.io/aurora/reference/plotQQ.md),
[`plotViolin()`](https://andrisignorell.github.io/aurora/reference/plotViolin.md)

## Examples

``` r
# Basic usage
x <- factor(sample(letters[1:5], 100, TRUE))
plotCatDist(x)

plotCatDist(table(x))


# Only proportions
plotCatDist(x, type = "perc")


# With cumulative distribution
plotCatDist(x, ecdf = TRUE)


# Many categories (truncation)
x2 <- factor(sample(letters, 200, TRUE))
plotCatDist(x2, maxcats = 10)

```
