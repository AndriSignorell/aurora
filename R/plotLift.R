
#' Lift Chart
#'
#' Draws the cumulative lift curve, the cumulative gain curve, or the
#' per-group lift bars of a binary classifier, together with the baseline of
#' a random model.  The chart answers the operational question directly: how
#' much better is acting on the top-scored share of the cases than acting on
#' a random share of the same size.
#'
#' @param x an object of class \code{"Lift"}, as returned by
#'   \code{alloy::lift()}.
#'
#' @param type the curve to draw. One of \code{"cumulative"} (cumulative
#'   lift over depth, the default), \code{"gain"} (share of all positives
#'   captured, over depth), or \code{"decile"} (per-group lift as bars).
#'
#' @param main main title of the plot. \code{NULL} (default) derives a title
#'   from \code{deparse(substitute(x))}. \code{""}, \code{NA}, or
#'   \code{FALSE} suppress the title entirely (and compact the top margin
#'   accordingly); any other string is used as given.
#' @param xlab label for the x-axis. \code{NULL} (default) derives a label
#'   from \code{type}.
#' @param ylab label for the y-axis. \code{NULL} (default) derives a label
#'   from \code{type}.
#' @param ylim numeric vector of length 2; y-axis limits. \code{NULL}
#'   (default) spans the curve together with the baseline.
#'
#' @param col color of the curve or the bars. \code{.useTheme} (default)
#'   resolves to \code{getTheme()$twin[1]} - a single accent color,
#'   consistent with \code{\link{plotECDF}}.
#' @param lwd line width of the curve. Has no effect for
#'   \code{type = "decile"}.
#'
#' @param grid controls drawing of the background grid.
#'   Can be:
#'   \itemize{
#'     \item \code{.useTheme} (default): follow the active theme
#'       (\code{getTheme()$grid})
#'     \item \code{TRUE}: draw grid with theme settings
#'     \item \code{FALSE}, \code{NULL}, or \code{NA}: suppress grid
#'     \item a named list: arguments passed to \code{\link[graphics]{grid}},
#'       overriding the theme defaults for this call only
#'   }
#'
#' @param box controls drawing of the plot box. \code{.useTheme} (default)
#'   resolves to \code{getTheme()$box}. \code{TRUE}/\code{FALSE}/\code{NA},
#'   or a named list, as for \code{grid}.
#'
#' @param baseline controls the reference line of a random model - a
#'   horizontal line at 1 for \code{type = "cumulative"} and
#'   \code{"decile"}, the diagonal for \code{type = "gain"}.
#'   Can be:
#'   \itemize{
#'     \item \code{TRUE} (default): draw with default settings
#'     \item \code{FALSE}, \code{NULL}, or \code{NA}: suppress
#'     \item a named list: arguments passed to \code{\link[graphics]{lines}}
#'       (or \code{\link[graphics]{abline}} for \code{type = "decile"}),
#'       e.g. \code{list(col = "black", lty = "dotted")}
#'   }
#'
#' @param perfect controls the curve of a perfect ranking - the theoretical
#'   maximum attainable at each depth, given the base rate. \code{FALSE} by
#'   default, since it compresses the interesting part of the y-axis;
#'   \code{TRUE} or a named list to draw it, as for \code{baseline}. Has no
#'   effect for \code{type = "decile"}.
#'
#' @param legend controls drawing of the legend.
#'   Can be:
#'   \itemize{
#'     \item \code{TRUE} (default): draw with default settings
#'     \item \code{FALSE}, \code{NULL}, or \code{NA}: suppress
#'     \item a named list: arguments passed to \code{\link[graphics]{legend}},
#'       e.g. \code{list(x = "bottomleft")}
#'   }
#'
#' @param stamp controls the corner stamp. \code{.useTheme} (default)
#'   resolves to \code{getTheme()$stamp}. \code{TRUE}/\code{FALSE}/
#'   \code{NULL}, a string, or a named list of arguments for
#'   \code{\link{stamp}()}.
#'
#' @param ... further graphical parameters passed to \code{par()} via the
#'   internal framework.
#'
#' @details
#' Reading the cumulative curve at depth 0.2 gives the factor by which
#' contacting the top-scored fifth of the cases beats contacting a random
#' fifth. The gain variant answers the complementary question - what share
#' of all positives that fifth captures.
#'
#' The curve necessarily converges to 1 (cumulative lift) or to the diagonal
#' endpoint (gain) at depth 1, where all cases are selected and the model no
#' longer discriminates. Only the left part of the curve carries decision
#' value: a model that separates well over the first two deciles and poorly
#' afterwards is preferable for a small campaign to one with the reverse
#' profile and identical AUC.
#'
#' Optional plot components (\code{grid}, \code{box}, \code{baseline},
#' \code{perfect}, \code{legend}) follow \code{\link[bedrock]{callIf}}
#' semantics:
#' \itemize{
#'   \item \code{TRUE}: draw with defaults
#'   \item \code{FALSE}, \code{NULL}, or \code{NA}: suppress component
#'   \item named list: customize component arguments
#' }
#'
#' \code{col}, \code{grid}, \code{box}, and \code{stamp} default to
#' \code{.useTheme}, deferring to the package's active theme (see [theme])
#' rather than a hardcoded value.
#'
#' The number of groups is a property of the lift table, not of the plot -
#' set it via the \code{nBins} argument of \code{alloy::lift()}.
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso \code{alloy::lift()}, \code{alloy::roc()}, \code{\link{plotECDF}},
#'   \code{\link[bedrock]{callIf}}, [theme]
#'
#' @examples
#' \dontrun{
#' fitLogit <- alloy::fitMod(admit ~ gre + gpa + rank, Admit, fitfn = "logit")
#' lft <- alloy::lift(fitLogit)
#'
#' plotLift(lft)
#' plotLift(lft, type = "gain")
#'
#' # Per-group bars, coarser grouping set at computation time
#' plotLift(alloy::lift(fitLogit, nBins = 5), type = "decile")
#'
#' # Add the perfect-ranking reference, suppress the legend
#' plotLift(lft, perfect = TRUE, legend = FALSE)
#'
#' # No title, compact top margin
#' plotLift(lft, main = "")
#' }
#'

#' @family plot.special
#' @concept model-evaluation
#' @concept prediction
#' @concept line-chart
#'
#'
#' @export
plotLift <- function(

  x,

  # STRUCTURE
  type = c("cumulative", "gain", "decile"),

  # LABELS
  main = NULL,
  xlab = NULL,
  ylab = NULL,

  # AXES
  ylim = NULL,

  # STYLE
  col  = .useTheme,
  lwd  = 2,
  grid = .useTheme,
  box  = .useTheme,

  # FEATURES
  baseline = TRUE,
  perfect  = FALSE,
  legend   = TRUE,

  stamp = .useTheme,
  ...
) {

  mc <- match.call()

  if (!inherits(x, "Lift"))
    stop("Argument 'x' must be an object of class 'Lift', as returned by alloy::lift().")

  type <- match.arg(type)

  main <- .resolveTitle(main, default = deparse(mc$x))

  twin    <- getTheme()$twin
  col     <- if (identical(col, .useTheme)) twin[1L] else col
  colPerf <- twin[2L]
  colBase <- "grey50"

  baseRate <- attr(x, "baseRate")

  xlab <- xlab %||% if (type == "decile") "group" else "depth"
  ylab <- ylab %||% switch(type,
                           cumulative = "cumulative lift",
                           gain       = "cumulative gain",
                           decile     = "lift")

  .withGraphicsState({

    .applyParFromDots(...,
                      defaults = list(
                        mar = c(left = 5, top = .marTop(main))
                      ))

    if (type == "decile") {

      # --- bars ----------------------------------------------------
      ylim <- ylim %||% c(0, max(c(x$lift, 1), na.rm = TRUE) * 1.05)
      mids <- barplot(x$lift, plot = FALSE)

      plot.new()
      plot.window(xlim = range(mids) + c(-0.6, 0.6), ylim = ylim)

      .drawGrid(grid)

      barplot(x$lift, col = col, border = NA, add = TRUE, axes = FALSE,
              names.arg = x$bin)

      bedrock::callIf(graphics::abline, baseline,
                      defaults = list(
                        h   = 1,
                        col = colBase,
                        lty = "dashed"
                      ))

      axis(2)
      .drawBox(box)

    } else {

      # --- prepare -------------------------------------------------
      xv <- c(0, x$depth)
      yv <- switch(type,
                   cumulative = c(x$cumLift[1L], x$cumLift),
                   gain       = c(0, x$gain))

      perfY <- .liftPerfect(xv, baseRate, type = type)

      ylim <- ylim %||% range(c(yv, 1, if (!isFALSE(perfect)) perfY),
                              na.rm = TRUE)

      # --- base plot -----------------------------------------------
      plot.new()
      plot.window(xlim = c(0, 1), ylim = ylim)

      .drawGrid(grid)

      # --- baseline (random model) ---------------------------------
      bedrock::callIf(lines, baseline,
                      defaults = list(
                        x   = c(0, 1),
                        y   = if (type == "gain") c(0, 1) else c(1, 1),
                        col = colBase,
                        lty = "dashed",
                        lwd = 1
                      ))

      # --- perfect ranking -----------------------------------------
      bedrock::callIf(lines, perfect,
                      defaults = list(
                        x   = xv,
                        y   = perfY,
                        col = colPerf,
                        lty = "dotted",
                        lwd = 1
                      ))

      # --- curve ---------------------------------------------------
      lines(xv, yv, col = col, lwd = lwd)

      axis(1)
      axis(2)
      .drawBox(box)
    }

    if (nzchar(main)) title(main = main)
    if (nzchar(xlab)) title(xlab = xlab)
    if (nzchar(ylab)) title(ylab = ylab)

    # --- legend ----------------------------------------------------
    isOn <- function(a) !isFALSE(a) && !is.null(a) && !bedrock::isNA(a)

    legLabels <- "model"
    legFill   <- col

    if (isOn(baseline)) {
      legLabels <- c(legLabels, "random")
      legFill   <- c(legFill,   colBase)
    }

    if (type != "decile" && isOn(perfect)) {
      legLabels <- c(legLabels, "perfect")
      legFill   <- c(legFill,   colPerf)
    }

    bedrock::callIf(graphics::legend, legend,
                    defaults = .legendDefaults(list(
                      x        = if (type == "gain") "bottomright" else "topright",
                      legend   = legLabels,
                      fill     = legFill,
                      text.col = "black",
                      bg       = addOpacity("white")
                    )),
                    forbidden = c("legend", "fill"))

  }, stamp = stamp)

  invisible(x)
}


# Theoretical maximum curve of a perfect ranking, given the base rate.
# A perfect model puts all positives first, so the cumulative hit rate is
# min(1, baseRate/depth) and the gain is min(1, depth/baseRate).
.liftPerfect <- function(depth, baseRate, type = c("cumulative", "gain")) {

  type <- match.arg(type)

  switch(type,
         cumulative = pmin(1, baseRate / pmax(depth, .Machine$double.eps)) / baseRate,
         gain       = pmin(1, depth / baseRate))
}
