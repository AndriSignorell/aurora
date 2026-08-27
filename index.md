# 📦 pharos

**Title:** Descriptive Statistics Graphics and Utilities  
**License:** GPL (≥ 2)

## 🧩 Overview

`pharos` is the graphics layer of the **DescToolsX ecosystem**. It draws
statistical graphics on top of base graphics — distribution and density
displays, bivariate and categorical plots, diagnostic and model
evaluation panels — and supplies the colour, geometry, formatting and
HTML tools they are built from.

A theme system resolves colours, symbols and layout centrally, so the
look of a whole analysis can be set once instead of being repeated at
every call.

The package is self-contained and usable without the rest of the suite.

📖 **Documentation:** <https://andrisignorell.github.io/pharos/>

## ⚙️ Installation

``` r

install.packages("pharos")
```

Or the development version from GitHub:

``` r

remotes::install_github("AndriSignorell/pharos")
```

## 📚 Core Features

### 🔹 Univariate and Distribution Plots

- [`plotFdist()`](https://andrisignorell.github.io/pharos/reference/plotFdist.md),
  [`plotDens()`](https://andrisignorell.github.io/pharos/reference/plotDens.md),
  [`plotECDF()`](https://andrisignorell.github.io/pharos/reference/plotECDF.md),
  [`plotProbDist()`](https://andrisignorell.github.io/pharos/reference/plotProbDist.md)
- [`plotBox()`](https://andrisignorell.github.io/pharos/reference/plotBox.md),
  [`plotViolin()`](https://andrisignorell.github.io/pharos/reference/plotViolin.md),
  [`plotRidge()`](https://andrisignorell.github.io/pharos/reference/plotRidge.md),
  [`plotDensBox()`](https://andrisignorell.github.io/pharos/reference/plotDensBox.md)
- [`plotDot()`](https://andrisignorell.github.io/pharos/reference/plotDot.md),
  [`plotBar()`](https://andrisignorell.github.io/pharos/reference/plotBar.md),
  [`plotBag()`](https://andrisignorell.github.io/pharos/reference/plotBag.md),
  [`plotQQ()`](https://andrisignorell.github.io/pharos/reference/plotQQ.md)

### 🔹 Bivariate and Multivariate Plots

- [`plotXY()`](https://andrisignorell.github.io/pharos/reference/plotXY.md),
  [`plotBubble()`](https://andrisignorell.github.io/pharos/reference/plotBubble.md),
  [`plotDens2D()`](https://andrisignorell.github.io/pharos/reference/plotDens2D.md),
  [`plotHexbin()`](https://andrisignorell.github.io/pharos/reference/plotHexbin.md)
- [`plotCor()`](https://andrisignorell.github.io/pharos/reference/plotCor.md),
  [`plotAssoc()`](https://andrisignorell.github.io/pharos/reference/plotAssoc.md),
  [`plotMosaic()`](https://andrisignorell.github.io/pharos/reference/plotMosaic.md),
  [`plotHeatmap()`](https://andrisignorell.github.io/pharos/reference/plotHeatmap.md)
- [`plotTernary()`](https://andrisignorell.github.io/pharos/reference/plotTernary.md),
  [`plotPolar()`](https://andrisignorell.github.io/pharos/reference/plotPolar.md),
  [`plotCirc()`](https://andrisignorell.github.io/pharos/reference/plotCirc.md),
  [`plotWeb()`](https://andrisignorell.github.io/pharos/reference/plotWeb.md),
  [`plotTreemap()`](https://andrisignorell.github.io/pharos/reference/plotTreemap.md)

### 🔹 Model and Diagnostic Displays

- [`plotLift()`](https://andrisignorell.github.io/pharos/reference/plotLift.md),
  [`plotPropCI()`](https://andrisignorell.github.io/pharos/reference/plotPropCI.md),
  [`plotMiss()`](https://andrisignorell.github.io/pharos/reference/plotMiss.md),
  [`plotCatDist()`](https://andrisignorell.github.io/pharos/reference/plotCatDist.md)
- [`plot.BlandAltman()`](https://andrisignorell.github.io/pharos/reference/plot.BlandAltman.md),
  [`plot.Lc()`](https://andrisignorell.github.io/pharos/reference/plot.lc.md),
  [`plotBinaryTree()`](https://andrisignorell.github.io/pharos/reference/binaryTree.md)
- [`lines.lm()`](https://andrisignorell.github.io/pharos/reference/linesLm.md),
  [`lines.loess()`](https://andrisignorell.github.io/pharos/reference/lines.loess.md),
  [`splineCI()`](https://andrisignorell.github.io/pharos/reference/splineCI.md)

### 🔹 Layout and Faceting

- [`plotFacet()`](https://andrisignorell.github.io/pharos/reference/plotFacet.md)
  — panel layouts with a user-supplied panel function
- [`canvas()`](https://andrisignorell.github.io/pharos/reference/canvas.md),
  [`plotArea()`](https://andrisignorell.github.io/pharos/reference/plotArea.md),
  [`mar()`](https://andrisignorell.github.io/pharos/reference/mar.md),
  [`abcCoords()`](https://andrisignorell.github.io/pharos/reference/abcCoords.md),
  [`axisBreak()`](https://andrisignorell.github.io/pharos/reference/axisBreak.md),
  [`axTicks()`](https://andrisignorell.github.io/pharos/reference/axTicks.md),
  [`axisFmt()`](https://andrisignorell.github.io/pharos/reference/axisFmt.md)
- [`spreadOut()`](https://andrisignorell.github.io/pharos/reference/spreadOut.md),
  [`lineToUser()`](https://andrisignorell.github.io/pharos/reference/lineToUser.md),
  [`isValidPlotRegion()`](https://andrisignorell.github.io/pharos/reference/isValidPlotRegion.md)

### 🔹 Annotation

- [`boxedText()`](https://andrisignorell.github.io/pharos/reference/boxedText.md),
  [`barText()`](https://andrisignorell.github.io/pharos/reference/barText.md),
  [`textLegend()`](https://andrisignorell.github.io/pharos/reference/textLegend.md),
  [`colLegend()`](https://andrisignorell.github.io/pharos/reference/colLegend.md),
  [`errBars()`](https://andrisignorell.github.io/pharos/reference/errBars.md),
  [`band()`](https://andrisignorell.github.io/pharos/reference/band.md),
  [`stamp()`](https://andrisignorell.github.io/pharos/reference/stamp.md),
  [`titleRect()`](https://andrisignorell.github.io/pharos/reference/titleRect.md),
  [`lineSep()`](https://andrisignorell.github.io/pharos/reference/lineSep.md)

### 🔹 Colour

- Conversions:
  [`colToHex()`](https://andrisignorell.github.io/pharos/reference/colToHex.md),
  [`colToRGB()`](https://andrisignorell.github.io/pharos/reference/colToRGB.md),
  [`colToHSV()`](https://andrisignorell.github.io/pharos/reference/colToHSV.md),
  [`hexToRGB()`](https://andrisignorell.github.io/pharos/reference/hexToRGB.md),
  [`rgbToCmy()`](https://andrisignorell.github.io/pharos/reference/RGBToCMY.md),
  [`cmykToRgb()`](https://andrisignorell.github.io/pharos/reference/CMYKToRGB.md),
  [`longToRGB()`](https://andrisignorell.github.io/pharos/reference/longToRGB.md)
- Manipulation:
  [`addOpacity()`](https://andrisignorell.github.io/pharos/reference/addOpacity.md),
  [`fade()`](https://andrisignorell.github.io/pharos/reference/fade.md),
  [`darken()`](https://andrisignorell.github.io/pharos/reference/darken.md),
  [`lighten()`](https://andrisignorell.github.io/pharos/reference/lighten.md),
  [`shade()`](https://andrisignorell.github.io/pharos/reference/shade.md),
  [`mixColors()`](https://andrisignorell.github.io/pharos/reference/mixColors.md),
  [`contrastColor()`](https://andrisignorell.github.io/pharos/reference/contrastColor.md),
  [`grayScale()`](https://andrisignorell.github.io/pharos/reference/grayscale.md)
- Palettes:
  [`pal()`](https://andrisignorell.github.io/pharos/reference/pal.md),
  [`palNames()`](https://andrisignorell.github.io/pharos/reference/palNames.md),
  [`hcol()`](https://andrisignorell.github.io/pharos/reference/hcol.md),
  [`findColor()`](https://andrisignorell.github.io/pharos/reference/findColor.md),
  [`setBackCol()`](https://andrisignorell.github.io/pharos/reference/setBackCol.md)

### 🔹 Geometry

- [`arc()`](https://andrisignorell.github.io/pharos/reference/arc.md),
  [`bezier()`](https://andrisignorell.github.io/pharos/reference/bezier.md),
  [`circle()`](https://andrisignorell.github.io/pharos/reference/circle.md),
  [`ellipse()`](https://andrisignorell.github.io/pharos/reference/ellipse.md),
  [`ring()`](https://andrisignorell.github.io/pharos/reference/ring.md),
  [`polygon()`](https://andrisignorell.github.io/pharos/reference/polygon.md),
  [`regPolygon()`](https://andrisignorell.github.io/pharos/reference/regPolygon.md),
  [`polarGrid()`](https://andrisignorell.github.io/pharos/reference/polarGrid.md)
- [`rotate()`](https://andrisignorell.github.io/pharos/reference/rotate.md),
  [`transformXY()`](https://andrisignorell.github.io/pharos/reference/transformXY.md),
  coordinate conversions, degree/radian conversion,
  [`convUnit()`](https://andrisignorell.github.io/pharos/reference/convUnit.md)

### 🔹 Formatting and Strings

- [`fm()`](https://andrisignorell.github.io/pharos/reference/fm.md),
  [`fmCI()`](https://andrisignorell.github.io/pharos/reference/fmCI.md),
  [`unit()`](https://andrisignorell.github.io/pharos/reference/unit.md),
  [`ftable.list()`](https://andrisignorell.github.io/pharos/reference/ftable.list.md)
- [`strAbbr()`](https://andrisignorell.github.io/pharos/reference/strAbbr.md),
  [`strAlign()`](https://andrisignorell.github.io/pharos/reference/strAlign.md),
  [`strCap()`](https://andrisignorell.github.io/pharos/reference/strCap.md),
  [`strChop()`](https://andrisignorell.github.io/pharos/reference/strChop.md),
  [`strPad()`](https://andrisignorell.github.io/pharos/reference/strPad.md),
  [`strTrunc()`](https://andrisignorell.github.io/pharos/reference/strTrunc.md),
  [`strRev()`](https://andrisignorell.github.io/pharos/reference/strRev.md),
  [`strDist()`](https://andrisignorell.github.io/pharos/reference/strDist.md),
  [`strSpell()`](https://andrisignorell.github.io/pharos/reference/strSpell.md),
  [`strExtract()`](https://andrisignorell.github.io/pharos/reference/strExtract.md)

### 🔹 HTML Output

- [`as.html()`](https://andrisignorell.github.io/pharos/reference/as.html.md),
  [`toHtmlTable()`](https://andrisignorell.github.io/pharos/reference/toHtmlTable.md),
  [`escapeHtml()`](https://andrisignorell.github.io/pharos/reference/escapeHtml.md),
  [`htmlNotation()`](https://andrisignorell.github.io/pharos/reference/htmlNotation.md),
  [`htmlSubscript()`](https://andrisignorell.github.io/pharos/reference/htmlSubscript.md),
  [`as.img()`](https://andrisignorell.github.io/pharos/reference/as.img.md),
  [`as.fileLink()`](https://andrisignorell.github.io/pharos/reference/as.fileLink.md),
  [`embedFile()`](https://andrisignorell.github.io/pharos/reference/embedFile.md),
  [`preview()`](https://andrisignorell.github.io/pharos/reference/preview.md)

## 🚀 Design Principles

- **Consistent** — lowerCamelCase API and uniform argument conventions
  across the whole DescToolsX suite
- **Themed** — colours, symbols and layout resolved centrally through
  [`theme()`](https://andrisignorell.github.io/pharos/reference/theme.md)
  and
  [`style()`](https://andrisignorell.github.io/pharos/reference/style.md)
- **Base graphics** — no grid, no extra graphics stack; plots compose
  with everything already in R
- **Fast** — performance-critical routines implemented in Rcpp

## 🧪 Example

``` r

library(pharos)

# distribution overview: histogram, density, boxplot, ecdf in one panel
plotFdist(rnorm(500))

# named plot positions without arithmetic
plot(rnorm(20), type = "n")
xy <- abcCoords("topleft", inset = 1)
text(xy$xy$x, xy$xy$y, "annotation", adj = xy$adj)

# colour manipulation
fade(pal("dark"), 0.4)

# faceting with a panel function
plotFacet(split(iris$Sepal.Length, iris$Species),
          dim = c(1, 3), panelFun = plotDens)
```

## 🧱 The Suite

`pharos` builds on `bedrock` (base utilities). `DescToolsX` (descriptive
statistics), `lumen` (tests and intervals), `alloy` (modelling), `pons`
(MS-Office) and `swissValet` (RStudio addins) complete the family.

## 🙏 Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.

## 📜 License

GPL (≥ 2)
