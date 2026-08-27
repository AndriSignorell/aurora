# Ellipse Geometry

Create an elliptic geometry.

## Usage

``` r
ellipse(x = 0, y = 0, radiusX = 1, radiusY = radiusX, numPoints = 100)
```

## Arguments

- x, y:

  centre coordinates.

- radiusX, radiusY:

  horizontal and vertical radius.

- numPoints:

  number of points used to approximate the ellipse.

## Value

An object inheriting from class `"ellipseGeometry"`.

## Details

Use
[`rotate`](https://andrisignorell.github.io/pharos/reference/rotate.md)
to rotate the resulting geometry.

## See also

Other geometry.structures:
[`arc()`](https://andrisignorell.github.io/pharos/reference/arc.md),
[`band()`](https://andrisignorell.github.io/pharos/reference/band.md),
[`bezier()`](https://andrisignorell.github.io/pharos/reference/bezier.md),
[`circle()`](https://andrisignorell.github.io/pharos/reference/circle.md),
[`polygon()`](https://andrisignorell.github.io/pharos/reference/polygon.md),
[`regPolygon()`](https://andrisignorell.github.io/pharos/reference/regPolygon.md),
[`ring()`](https://andrisignorell.github.io/pharos/reference/ring.md)
