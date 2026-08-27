# Subscript notation

Infix operator producing an HTML subscript, e.g. for indexed symbols
such as \\x_i\\.

## Usage

``` r
x %_% i
```

## Arguments

- x:

  a character vector (the base symbol)

- i:

  a character vector (the subscript), recycled against `x`

## Value

a character vector: `x` followed by `<sub>i</sub>`

## See also

Other html:
[`as.fileLink()`](https://andrisignorell.github.io/pharos/reference/as.fileLink.md),
[`as.html()`](https://andrisignorell.github.io/pharos/reference/as.html.md),
[`as.img()`](https://andrisignorell.github.io/pharos/reference/as.img.md),
[`embedFile()`](https://andrisignorell.github.io/pharos/reference/embedFile.md),
[`escapeHtml()`](https://andrisignorell.github.io/pharos/reference/escapeHtml.md),
[`htmlNotation`](https://andrisignorell.github.io/pharos/reference/htmlNotation.md),
[`toHtmlTable()`](https://andrisignorell.github.io/pharos/reference/toHtmlTable.md)

## Examples

``` r
"x" %_% "i"  # -> "x<sub>i</sub>"
#> [1] "x<sub>i</sub>"
```
