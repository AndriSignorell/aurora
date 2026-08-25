#' Graphics Framework Helpers
#'
#' @description
#' The internal machinery every plot function in the suite is built on:
#' state management, parameter resolution, title handling and the two
#' element dispatchers. They are exported so that plot functions living in
#' other packages of the suite - a `plot.FitMod()` in \pkg{alloy}, a
#' `plot.Desc.*` method anywhere outside \pkg{pharos} - can follow the same
#' contract instead of reimplementing it.
#'
#' They are not part of the user-facing API. Nothing here is meant to be
#' called from an analysis script, and the signatures may change with the
#' framework.
#'
#' | Helper | Purpose |
#' |---|---|
#' | [.withGraphicsState()] | save/restore `par()`, draw the stamp on success |
#' | [.applyParFromDots()] | apply graphical parameters from `...` over defaults |
#' | [.resolveTitle()] | three-state `main` contract (§9.4) |
#' | [.marTop()] | top margin implied by the resolved title |
#' | [.marginLines()] | left margin implied by the drawn tick labels |
#' | [.drawGrid()] | grid dispatcher, understands `.useTheme` |
#' | [.drawBox()] | box dispatcher, understands `.useTheme` |
#' | `.useTheme` | sentinel telling "resolve against the active theme" (§9.3) |
#'
#' @section Why these are exported:
#' Section 9.7 of the design rules gives the unavailability of these
#' helpers outside `pharos` as the reason `plot.Desc.*` methods live in
#' `pharos` rather than next to the classes they plot. That reason no
#' longer holds. The placement of the existing methods is unchanged - the
#' export only removes the constraint, it does not itself relocate
#' anything.
#'
#' @section Contract for callers outside pharos:
#' The rules in sections 9.1-9.5 apply unchanged: no direct `par()` call
#' outside `.applyParFromDots()`, `stamp` passed to
#' `.withGraphicsState()` rather than drawn by hand, no `oma`/`omi` in any
#' save/restore list, and no internal `mfrow`.
#'
#' @keywords internal
#' @name graphics-framework
NULL
