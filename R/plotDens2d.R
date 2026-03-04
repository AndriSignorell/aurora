


# cats <- MASS::cats
# 
# plotDens2d(cats$Bwt, cats$Hwt)
# plotDens2d(cats$Bwt, cats$Hwt, type="persp")
# plotDens2d(cats$Bwt, cats$Hwt, type="image")
# grid()
# 


plotDens2D <- function(x, y, type=c("contour", "image", "persp"), ...){
  

  bandwidth.nrd <- function (x) {
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L])/1.34
    4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
  }
  
  
  
  kde2d <- function (x, y, h, n = 25, lims = c(range(x), range(y))) {

    nx <- length(x)
    if (length(y) != nx)
      stop("data vectors must be the same length")
    
    if (any(!is.finite(x)) || any(!is.finite(y)))
      stop("missing or infinite values in the data are not allowed")
    
    if (any(!is.finite(lims)))
      stop("only finite values are allowed in 'lims'")
    
    n <- rep(n, length.out = 2L)
    gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
    gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])

    h <- if (missing(h))
      c(bandwidth.nrd(x), bandwidth.nrd(y))
    else rep(h, length.out = 2L)
    
    if (any(h <= 0))
      stop("bandwidths must be strictly positive")
    
    h <- h/4
    ax <- outer(gx, x, "-")/h[1L]
    ay <- outer(gy, y, "-")/h[2L]
    z <- tcrossprod(matrix(dnorm(ax), , nx), 
                    matrix(dnorm(ay), , nx))/(nx * h[1L] * h[2L])
    
    list(x = gx, y = gy, z = z)
    
  }

  
  bw <- c(bandwidth.nrd(x), bandwidth.nrd(y))

  kde <- kde2d(x = x, y=y, h=bw, n=500, lims=c(1, 4, 0, 20))
  
  
  
  switch(match.arg(type),  
         contour= { contour(kde, xlab="Bodyweight", ylab="Heartweight",
                            nlevels=8, labcex=0.8, xlim=c(1.5, 4), ylim=c(5, 17))},
         
         persp = { 
           persp(kde, ticktype="simple", theta=50, phi=20, r=4, scale=TRUE,
                 xlab="Bodyweight", ylab="Heartweight", zlab="Relative Frequency")},
         
         image = {
           image(kde, xlab="Bodyweight", ylab="Heartweight", col=rev(Pal(1, 100)),
                 xlim=c(1.5, 4), ylim=c(5, 17)) }
         )        
  
  
  
}


