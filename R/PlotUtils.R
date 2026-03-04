

# internal getOption wrapper for DescTools options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}






# # internal function to restore settings after a plot has been created
# 
# .withGraphicsState <- function(expr) {
#   
#   op <- par(no.readonly = TRUE)
#   ok <- FALSE
#   
#   on.exit({
#     # layout(matrix(1))
#     par(op)
#     
#     if (ok) {   ## && !is.null(.getOption("stamp"))) {
#       tryCatch(stamp(), error = function(e) NULL)
#     }
#     
#   }, add = TRUE)
#   
#   force(expr)
#   ok <- TRUE
#   
# }
# 


# base version
# # with layout saving option
# .withGraphicsState <- function(expr) {
#   
#   op <- par(no.readonly = TRUE)
#   opt <- options()
#   
#   hasLayout <- {
#     n <- layout.show(n = 0)
#     isTRUE(n > 0)
#   }
#   
#   ok <- FALSE
#   
#   on.exit({
#     par(op)
#     options(opt)
#     
#     if (hasLayout) {
#       ## layout wiederherstellen (falls ihr das implementiert habt)
#     }
#     
#     if (ok) {
#       tryCatch(stamp(), error = function(e) NULL)
#     }
#     
#   }, add = TRUE)
#   
#   # force(expr)
#   eval.parent(substitute(expr))
#   
#   ok <- TRUE
# }
# 

.withGraphicsState <- function(expr) {
  
  op <- par(no.readonly = TRUE)
  withr::defer(par(op))
  
  withr::local_options(list(warn = 1))
  
  ok <- FALSE
  
  on.exit({
    if (ok) tryCatch(stamp(), error = function(e) NULL)
  }, add = TRUE)
  
  eval.parent(substitute(expr))
  
  ok <- TRUE
  
  invisible(NULL)
}



.resolvePar <- function(name, value = NULL, default = NULL) {
  
  if (!is.null(value)) {
    return(value)
  }
  
  opt <- .getOption(paste0("descToolsX.plot.", name))
  if (!is.null(opt)) {
    return(opt)
  }
  
  default
}





.applyParFromDots <- function(...) {

  
  patch_fourpar <- function(new_val, old_val, pname) {
    
    if (!is.null(names(new_val))) {
      
      idx <- match(names(new_val),
                   c("bottom","left","top","right"))
      
      if (any(is.na(idx)))
        stop(sprintf("%s names must be bottom, left, top, right", pname))
      
      old_val[idx] <- new_val
      return(old_val)
    }
    
    new_val <- rep_len(new_val, 4)
    idx_na <- is.na(new_val)
    new_val[idx_na] <- old_val[idx_na]
    
    new_val
  }
  
    
  dots <- list(...)
  if (!length(dots)) return(invisible())
  
  dots <- dots[!is.na(names(dots))]
  dots <- dots[names(dots) %in% names(par(no.readonly = TRUE))]
  
  if (!length(dots)) return(invisible())
  
  p <- par(no.readonly = TRUE)
  
  
  if ("mar" %in% names(dots)) {
    dots$mar <- patch_fourpar(dots$mar, p$mar, "mar")
  }
  
  if ("oma" %in% names(dots)) {
    dots$oma <- patch_fourpar(dots$oma, p$oma, "oma")
  }
  
  do.call(par, dots)
  
  invisible()
}




