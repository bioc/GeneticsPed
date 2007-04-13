### runit.gpi.R
###------------------------------------------------------------------------
### What: Unit tests for gpi() method
### $Id: runit.gpi.R 1153 2007-03-02 17:12:03Z ggorjan $
### Time-stamp: <2007-03-02 18:08:02 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("GeneticsPed")
}

### }}}
### {{{ --- gpi ---

test.gpi <- function()
{
  ## --- Test examples - check man page for details ---

  ## Example 1
  gp <- c(.1, .5)
  hwp <- c(.5625, .3750)
  ret <- 51.8
  checkEquals(round(gpi(gp=gp, hwp=hwp), digits=1), ret)

  ## Example 1 extended
  (gp <- matrix(c(.1, .5, .2, .5), nrow=2, ncol=2, byrow=TRUE))
  ret <- c(ret, 40.5)
  checkEquals(round(gpi(gp=gp, hwp=hwp), digits=1), ret)

  ## Example 2
  gp <- c(.1, .5, .0, .4, .0)
  hwp <- c(.5625, .3750, .0, .0625, .0)
  ret <- 51.8
  checkEquals(round(gpi(gp=gp, hwp=hwp), digits=1), ret)

  ## Example 3

  (hwp <- rep(.2, times=5) %*% t(rep(.2, times=5)))
  hwp <- c(hwp[upper.tri(hwp, diag=TRUE)])
  (hwp <- hwp[1:(length(hwp) - 1)])
  gp <- hwp / 2
  gp[2] <- gp[2] + .5
  gp
  ret <- 50
  checkEquals(gpi(gp=gp, hwp=hwp), ret)

  ## --- Error checking ---

  ## probabilities should lie on interval 0, 1
  checkException(gpi(gp=-1, hwp=0.5))
  checkException(gpi(gp=0.5, hwp=5))

  ## 'gp' and 'hwp' must be numeric vectors or matrices
  checkException(gpi(gp="0.5", hwp=0.2))
  checkException(gpi(gp=0.5, hwp=list(0.2)))
  checkException(gpi(gp=matrix("0.5"), hwp=0.2))

  ## 'gp' and 'hwp' must be of the same dimension
  gp <- matrix(c(.1, .5, .6, .2), nrow=2, ncol=2, byrow=TRUE)
  checkException(gpi(gp=gp, hwp=gp[, 1, drop=FALSE]))
  ## if gp and hwp are matrices, they should be of the same dimension

  checkException(gpi(gp=c(.1, .5), hwp=gp[, 1, drop=FALSE]))
  ## if gp is vector, then hwp should also be a vector

  checkException(gpi(gp=c(.1, .5), hwp=c(.1)))
  ## gp and hwp must have the same length

  checkException(gpi(gp=c(.1, .5, .2, .0), hwp=c(.1, .6, .1, .1)))
  ## wrong number of genotype probabilities
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.gpi.R ends here
