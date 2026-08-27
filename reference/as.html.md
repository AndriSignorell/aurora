# Mark a character vector as HTML

Tags a character vector with the S3 class `"html"` so that it prints via
[`preview.html`](https://andrisignorell.github.io/pharos/reference/preview.html.md)
as readable text instead of as a raw character vector.

## Usage

``` r
as.html(x)
```

## Arguments

- x:

  a character vector, typically containing HTML markup

## Value

`x`, with class `"html"` added

## See also

Other html:
[`as.fileLink()`](https://andrisignorell.github.io/pharos/reference/as.fileLink.md),
[`as.img()`](https://andrisignorell.github.io/pharos/reference/as.img.md),
[`embedFile()`](https://andrisignorell.github.io/pharos/reference/embedFile.md),
[`escapeHtml()`](https://andrisignorell.github.io/pharos/reference/escapeHtml.md),
[`htmlNotation`](https://andrisignorell.github.io/pharos/reference/htmlNotation.md),
[`htmlSubscript`](https://andrisignorell.github.io/pharos/reference/htmlSubscript.md),
[`toHtmlTable()`](https://andrisignorell.github.io/pharos/reference/toHtmlTable.md)

## Examples

``` r
as.html("<b>bold</b>")
#> <b>bold</b>
```
