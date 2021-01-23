### runit.gpi.R
###------------------------------------------------------------------------
### What: Unit tests for gpi() method
### $Id: runit.gpi.R 1153 2007-03-02 17:12:03Z ggorjan $
### Time-stamp: <2007-09-13 03:34:20 ggorjan>
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
  checkException(gpi(gp=-1, hwp=0.5),silent = TRUE)
  checkException(gpi(gp=0.5, hwp=5),silent = TRUE)

  ## 'gp' and 'hwp' must be numeric vectors or matrices
  checkException(gpi(gp="0.5", hwp=0.2),silent = TRUE)
  checkException(gpi(gp=0.5, hwp=list(0.2)),silent = TRUE)
  checkException(gpi(gp=matrix("0.5"), hwp=0.2),silent = TRUE)

  ## 'gp' and 'hwp' must be of the same dimension
  gp <- matrix(c(.1, .5, .6, .2), nrow=2, ncol=2, byrow=TRUE)
  checkException(gpi(gp=gp, hwp=gp[, 1, drop=FALSE]),silent = TRUE)
  ## if gp and hwp are matrices, they should be of the same dimension

  checkException(gpi(gp=c(.1, .5), hwp=gp[, 1, drop=FALSE]),silent = TRUE)
  ## if gp is vector, then hwp should also be a vector

  checkException(gpi(gp=c(.1, .5), hwp=c(.1)),silent = TRUE)
  ## gp and hwp must have the same length

  checkException(gpi(gp=c(.1, .5, .2, .0), hwp=c(.1, .6, .1, .1)),silent = TRUE)
  ## wrong number of genotype probabilities
}

### }}}
### {{{ --- gpLong2Wide ---

test.gpLong2Wide <- function()
{
  tmp <- data.frame(ind=c("A", "A", "A", "B", "B"),
                    gen=c("A/A", "A/B", "B/B", "A/A", "A/B"),
                    pro=c(0.5, 0.25, 0.25, 0.75, 0.25))

  checkException(gpLong2Wide(x=1),silent = TRUE)
  ## 'x' must be a data.frame
  checkException(gpLong2Wide(x=tmp, id=2, genotype="gen", prob="pro"),silent = TRUE)
  ## 'id', 'genotype', and 'prob' must be character
  checkException(gpLong2Wide(x=tmp, id="id", genotype="gen", prob="pro"),silent = TRUE)
  ## 'id', 'genotype', and 'prob' must be column names of 'x'
  checkException(gpLong2Wide(x=tmp, id="ind", genotype="gen", prob="pro"),silent = TRUE)
  ## 'x' must be of a genotype class

  if(require(genetics)) {
    tmp$gen <- as.genotype(tmp$gen)
    tmp2 <- gpLong2Wide(x=tmp, id="ind", genotype="gen", prob="pro")
    checkEquals(as.vector(tmp2), c(0.5, 0.75, 0.25, 0.25))
    tmp2 <- gpLong2Wide(x=tmp, id="ind", genotype="gen", prob="pro", trim=FALSE)
    checkEquals(as.vector(tmp2), c(0.5, 0.75, 0.25, 0.25, 0.25, 0))
  }

}

### }}}
### {{{ --- hwp ---

test.hwp <- function()
{
  checkException(hwp(x="A/A"))
  ## 'x' must be of a genotype class

  if(require(genetics)) {
    gen <- genotype(c("A/A", "A/B"))
    ## Pr(A)  = 3/4
    ## Pr(B)  = 1/4
    ## Pr(AA) = (3/4)^2           = 0.5625
    ## Pr(AA) = 2 * (3/4) * (1/4) = 0.3750
    ## Pr(BB) = (1/4)^2           = 0.0625
    ret <- c(0.5625, 0.3750, 0.0625)
    checkEquals(hwp(x=gen), ret[1:2])
    checkEquals(hwp(x=gen, trim=FALSE), ret)
  }
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.gpi.R ends here
