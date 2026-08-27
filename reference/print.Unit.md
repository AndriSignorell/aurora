# Print Object with Unit

S3 method for printing objects with a `"Unit"` class. Displays the value
along with its associated unit.

## Usage

``` r
# S3 method for class 'Unit'
print(x, ...)
```

## Arguments

- x:

  an object with class `"Unit"`.

- ...:

  additional arguments passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

Invisibly returns `x`.

## See also

[base::attr](https://rdrr.io/r/base/attr.html),
[bedrock::label](https://andrisignorell.github.io/bedrock/reference/Label.html)

Other format:
[`convUnit()`](https://andrisignorell.github.io/pharos/reference/convUnit.md),
[`fm()`](https://andrisignorell.github.io/pharos/reference/fm.md),
[`fmCI()`](https://andrisignorell.github.io/pharos/reference/fmCI.md),
[`style()`](https://andrisignorell.github.io/pharos/reference/style.md),
[`unit()`](https://andrisignorell.github.io/pharos/reference/unit.md)

## Examples

``` r
x <- 10
unit(x) <- "m"
class(x) <- "Unit"
print(x)
#> 10 [ m ]
```
