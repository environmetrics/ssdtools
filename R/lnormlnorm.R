#' Log Normal-Log Normal Mixture Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the log-normal log-normal mixture distribution.
#' @param x,q	vector of quantiles.
#' @param p	vector of probabilities.
#' @param n number of observations. 
#' If length(n) > 1, the length is
#' taken to be the number required.
#' @param meanlog1,sdlog1 mean and standard deviation of the distribution on the log scale of the first mixture with default values of 0 and 1 respectively.
#' @param meanlog2,sdlog2 mean and standard deviation of the distribution on the log scale of the second mixture with default values of 1 and 1 respectively.
#' @param r proportion of the first mixture.
#' @param log,log.p logical; if TRUE, probabilitise p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.
#' 
#' @name lnormlnorm
#' @examples
#' hist(log(rlnormlnorm(1e+05)))
NULL

#' @rdname lnormlnorm
#' @export
dlnormlnorm <- function(x, meanlog1 = 0, sdlog1 = 1, meanlog2 = 3, sdlog2 = 1, r = 0.5, log = FALSE){
  density <- r * dlnorm(x,meanlog1,sdlog1) + (1 - r) * dlnorm(x, meanlog2, sdlog2)
  if(log) return(log(density))
  density
}

#' @rdname lnormlnorm
#' @export
plnormlnorm <- function(q, meanlog1 = 0, sdlog1= 1, meanlog2 = 3, sdlog2 = 1, r = 0.5, lower.tail = TRUE, log.p = FALSE){
  p <- r * plnorm(q,meanlog1, sdlog1) + (1 - r) * plnorm(q, meanlog2, sdlog2)
  if(!lower.tail)
    p <- 1 - p
  if(log.p) return(log(p))
  p
}

#' @rdname lnormlnorm
#' @export
qlnormlnorm <- function(p, meanlog1 = 0, sdlog1 = 1, meanlog2 = 3, sdlog2 = 1, r = 0.5, lower.tail = TRUE, log.p = FALSE){
  if(p==1) p <- 1-.Machine$double.eps
  if(p==0) p <- 0+.Machine$double.eps
  interval <- c(0+.Machine$double.eps, 10^10)
  
  f <- function(x) {
    plnormlnorm(x, meanlog1, sdlog1, meanlog2, sdlog2, r) - p
  }
  q <- uniroot(f, interval = interval)$root
  if(!lower.tail) q <- 1 - q
  if(log.p) return(log(q))
  q
}

#' @rdname lnormlnorm
#' @export
rlnormlnorm <- function(n, meanlog1 = 0, sdlog1 = 1, meanlog2 = 3, sdlog2 = 1, r = 0.5) {
  if(length(n) > 1) n <- length(n)
  n1 <- rbinom(1, size = n, prob = r)
  n2 <- n - n1
  sample <- c(rlnorm(n1, meanlog1, sdlog1), 
              rlnorm(n2, meanlog2, sdlog2))
  sample(sample, size = n)
}
