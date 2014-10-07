### gpi.R
###------------------------------------------------------------------------
### What: Genotype probability index
### $Id: gpi.R 1155 2007-03-02 22:52:41Z ggorjan $
### Time-stamp: <2007-09-20 13:43:28 ggorjan>
###------------------------------------------------------------------------

### {{{ gpi

gpi <- function(gp, hwp)
{
  ## --- Tests ---

  if(!is.numeric(gp) || !is.numeric(hwp))
    stop("'gp' and 'hwp' must be numeric vector or matrix")
  if(any(gp < 0 | gp > 1) || any(hwp < 0 | hwp > 1))
    stop("probabilities should lie on interval 0, 1")
  if(any(is.na(gp)) || any(is.na(hwp)))
    stop("NA values are not allowed in 'gp' and 'hwp'")

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
  ret <- .Fortran(R_gpi,
                  ## Number of individuals
                  nobs=as.integer(nId),
                  ## Number of alleles
                  n=as.integer(n),
                  ## Individual's genotype probabilities
                  gp=as.double(gp),
                  ## Hardy-Weinberg genotype probabilities
                  hwp=as.double(hwp),
                  ret=ret, PACKAGE="GeneticsPed")[["ret"]]

  ## --- Return ---

  ret
}

### }}}
### {{{ gpLong2Wide

gpLong2Wide <- function(x, id, genotype, prob, trim=TRUE)
{
  ## --- Check inputs ---

  if(!is.data.frame(x)) stop("'x' must be a data.frame")
  if(!all(sapply(list(id, genotype, prob), is.character)))
    stop("'id', 'genotype', and 'prob' must be character")
  if(!all(c(id, genotype, prob) %in% names(x)))
    stop("'id', 'genotype', and 'prob' must be column names of 'x'")
  if(!(is.genotype(x[, genotype])))
    stop("'x' must be of a genotype class")

  ## --- Setup ---

  idVal <- unique(x[, id])
  nId <- length(idVal)
  gen <- expectedGenotypes(alleles=allele.names(x[, genotype]))
  nGen <- length(gen)
  gp <- matrix(data=0, nrow=nId, ncol=nGen)
  colnames(gp) <- gen
  rownames(gp) <- idVal

  ## --- Fill the matrix ---

  for(i in seq(along=gen)) {
    test <- x[, genotype] == gen[i]
    idVal <- as.character(x[test, id])
    gp[idVal, gen[i]] <- x[test, prob]
  }

  ## --- Return ---

  if(trim) {
    gp[, 1:(ncol(gp) - 1)]
  } else {
    gp
  }
}

### }}}
### {{{ hwp

hwp <- function(x, trim=TRUE)
{
  ## --- Check inputs ---

  if(!(is.genotype(x)))
    stop("'x' must be of a genotype class")

  ## --- Calculate HWE probabilities ---

  ## Pr(AA) = Pr(A) * Pr(A)
  ## Pr(AB) = 2 * Pr(A) * Pr(B)
  ## ...

  tmp <- summary(x)$allele.freq
  tmp <- tmp[!is.na(tmp[, 2]), 2]
  hwp <- outer(X=tmp, Y=tmp, FUN="*")

  ## Keep only lower triangle with Pr(AA), Pr(AB), and Pr(BB);
  ## i.e. skip Pr(BA)
  lowerTriangle(hwp) <- 2 * lowerTriangle(hwp)
  hwp <- hwp[lower.tri(hwp, diag=TRUE)]

  ## --- Return ---

  if(trim) {
    hwp[1:(length(hwp) - 1)]
  } else {
    hwp
  }
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### gpi.R ends here
