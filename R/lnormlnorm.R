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
#' 
#' @name lnormlnorm
NULL

#' @rdname lnormlnorm
#' @export
dlnormlnorm <- function(x, meanlog1 = 0, sdlog1 = 1, meanlog2 = 1, sdlog2 = 0, r = 0.5){
  r * dlnorm(x,meanlog1,sdlog1) + (1-r)*dlnorm(x, meanlog2, sdlog2)
}

#' @rdname lnormlnorm
#' @export
qlnormlnorm <- function(p, meanlog1 = 0, sdlog1 = 1, meanlog2 = 0, sdlog2 = 1, r = 0.5){
  if(p==1){p=1-.Machine$double.eps}
  if(p==0){p=0+.Machine$double.eps}
  minmax <- c(0+.Machine$double.eps, 10^10)
  uniroot(function(x) plnormlnorm(x, meanlog1, sdlog1, meanlog2, sdlog2, r) - p,
          interval = minmax)$root  
}

#' @rdname lnormlnorm
#' @export
plnormlnorm <- function(q, meanlog1 = 0, sdlog1= 1, meanlog2 = 1, sdlog2 = 1, r = 0.5){
  r * plnorm(q,meanlog1, sdlog1) + (1-r)*plnorm(q, meanlog2, sdlog2)
}

#' @rdname lnormlnorm
#' @export
rlnormlnorm <- function(n, meanlog1 = 0, sdlog1 = 1, meanlog2 = 1, sdlog2 = 1, r = 0.5){
  n.lnorm1 <- rbinom(1, size = n, prob = r)
  n.lnorm2 <- n - n.lnorm1
  sample <- c(rlnorm(n=n.lnorm1, meanlog1, sdlog1), 
              rlnorm(n=n.lnorm2, meanlog2, sdlog2))
  sample(sample, size = n)
}
