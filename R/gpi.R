### gpi.R
###------------------------------------------------------------------------
### What: Genotype probability index
### $Id: gpi.R 1155 2007-03-02 22:52:41Z ggorjan $
### Time-stamp: <2007-03-02 23:28:12 ggorjan>
###------------------------------------------------------------------------

gpi <- function(gp, hwp)
{
  ## --- Tests ---

  if(!is.numeric(gp) | !is.numeric(hwp))
    stop("'gp' and 'hwp' must be numeric vectors or matrices")
  if(any(gp < 0 | gp > 1) | any(hwp < 0 | hwp > 1))
    stop("probabilities should lie on interval 0, 1")

  ## --- Setup ---

  msg <- "'gp' and 'hwp' must be of the same dimension"

  if(is.matrix(gp)) {
    k <- ncol(gp)
    nId <- nrow(gp)
    if(!is.matrix(hwp)) { # recycle hwp
      hwp <- matrix(hwp, nrow=nId, ncol=k, byrow=TRUE)
    } else {              # recyle should work only for vectors
      if(!isTRUE(all.equal(dim(gp), dim(hwp)))) stop(msg)
    }
  } else {
    if(is.matrix(hwp)) stop(msg)
    k <- length(gp)
    nId <- 1
    if(k != length(hwp)) stop(msg)
  }

  ## Number of dimensions (k) and alleles (n)
  ## k = n*(n + 1)/2 - 1
  ## 2(k + 1) = n^2 + n
  ## n^2 + n - 2k - 2 = 0 --> quadratic eq.
  ## n = (-b + sqrt(b^2 - 4ac)) / 2a
  ##   = (-1 + sqrt(1 - 4(-2k - 2))) / 2
  ##   = (-1 + sqrt(1 + 8k + 8)) / 2

  n <- (-1 + sqrt(1 + 8*k + 8)) / 2
  if(n %% 1 != 0) {
    msg <- c("nonconformant dimensions of 'gp':\n",
             sprintf("no. dimensions (k)= %s\n", k),
             sprintf("no. alleles (n) = %s\n", n),
             "k = n*(n + 1))/2 - 1?")
    stop(msg)
  }

  ## --- Call ---

  ret <- vector(mode="numeric", length=nId)
  ret <- .Fortran(name=R_gpi,
                  ## Number of individuals
                  nobs=as.integer(nId),
                  ## Number of alleles
                  n=as.integer(n),
                  ## Individual's genotype probabilities
                  gp=as.double(t(gp)),
                  ## Hardy-Weinberg genotype probabilities
                  hwp=as.double(t(hwp)),
                  ret=ret, PACKAGE="GeneticsPed")[["ret"]]

  ## Transpose i.e. t(gp) and t(hwp) is used due to Fortran column
  ## order. This matters on in case matrices are used.

  ## --- Return ---

  ret
}



###------------------------------------------------------------------------
### gpi.R ends here
