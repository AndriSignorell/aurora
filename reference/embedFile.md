# Base64-encode a file

The building block behind
[`as.img`](https://andrisignorell.github.io/pharos/reference/as.img.md)
and
[`as.fileLink`](https://andrisignorell.github.io/pharos/reference/as.fileLink.md):
reads a file and returns its contents base64-encoded, ready to be placed
in a data URI or in any container format that carries binary payloads as
text.

## Usage

``` r
embedFile(path)
```

## Arguments

- path:

  path to an existing file

## Value

a single character string

## See also

Other html:
[`as.fileLink()`](https://andrisignorell.github.io/pharos/reference/as.fileLink.md),
[`as.html()`](https://andrisignorell.github.io/pharos/reference/as.html.md),
[`as.img()`](https://andrisignorell.github.io/pharos/reference/as.img.md),
[`escapeHtml()`](https://andrisignorell.github.io/pharos/reference/escapeHtml.md),
[`htmlNotation`](https://andrisignorell.github.io/pharos/reference/htmlNotation.md),
[`htmlSubscript`](https://andrisignorell.github.io/pharos/reference/htmlSubscript.md),
[`toHtmlTable()`](https://andrisignorell.github.io/pharos/reference/toHtmlTable.md)

## Examples

``` r
fn <- tempfile(fileext = ".txt")
writeLines("hello", fn)
substr(embedFile(fn), 1, 8)
#> [1] "aGVsbG8K"
unlink(fn)
```
