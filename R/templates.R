

# #' @export
# plotSomething <- function(x, y,
#                     
#                     # LABELS
#                     main = NULL,
#                     xlab = NULL,
#                     ylab = NULL,                    
#                     
#                     # AXES
#                     xax = NULL,
#                     yax = NULL,
#                     
#                     # STRUCTURE
#                     beside = FALSE,
#                     horiz  = FALSE,
#                     
#                     # STYLE
#                     col = NULL,
#                     border = NULL,
#                     grid = NULL,
#                     box=FALSE,
#                     
#                     # FEATURES
#                     text = NULL,
#                     connlines = NULL,
#                     
#                     # FRAMEWORK
#                     stamp = TRUE,
#                     ...) {
#   
#   th <- .theme(
#     bar = list(col=col, border=border)
#   )
#   col <- th$col
#   border <- th$border
#   
#   dots  <- list(...)
#   
#   .withGraphicsState({
#     
#     
#     # par() aus ...
#     .applyParFromDots(...)
#     
#     
#     # your plot code 
#     # ...
#     
#     
#   }, stamp = stamp)
#   
#   invisible(b)
#   
# }
# 
# 
# 
# # == internal helper functions =======================================
# 
# 
