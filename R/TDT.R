### TDT.R
###------------------------------------------------------------------------
### What: Gene and gamete flow and mendelian sampling covariance matrices
### $Id: TDT.R 1193 2007-04-06 11:35:47Z ggorjan $
### Time-stamp: <2007-09-07 00:49:04 ggorjan>
###------------------------------------------------------------------------

### {{{ Gene flow matrix - T

###------------------------------------------------------------------------

geneFlowT <- function(x, sort=TRUE, names=TRUE, ...)
{
  ## --- Setup ---

  ascendant <- getAscendantName(x)
  if(length(ascendant) > 2)
    stop("no method for pedigree with more than two ascendants")
  if(sort) idOrig <- as.character(getId(x))  # for sort-back

  ## Pedigree must be extended and sorted
  x <- checkAttributes(x, extended=TRUE, sorted=TRUE)

  n <- nrow(x)
  ret <- matrix(0, n, n)         # n*n matrix
  diag(ret) <- 1                 # diagonal = 1
  rownames(ret) <- colnames(ret) <- getId(x)

  asc1 <- as.character(getFather(x))
  asc2 <- as.character(getMother(x))
  testAsc1 <- !isUnknown(asc1)
  testAsc2 <- !isUnknown(asc2)
  ## individuals with at least one ascendant known
  set <- which(!(!testAsc1 & !testAsc2))

  ## --- Core ---

  for(i in set) {                # loop over set
    j <- 1:(i - 1)               # ret is a lower triangular matrix
    if(testAsc1[i]) tmp <-       0.5 * ret[asc1[i], j]
    if(testAsc2[i]) tmp <- tmp + 0.5 * ret[asc2[i], j]
    ret[i, j] <- tmp
  }

  ## --- End ---

  if(sort) ret <- ret[idOrig, idOrig]
  if(!names) attr(ret, which="dimnames") <- NULL
  ret
}

### }}}
### {{{ Inverse of gene flow matrix - Tinv
###------------------------------------------------------------------------

geneFlowTinv <- function(x, sort=TRUE, names=TRUE, ...)
{
  ret <- - gameteFlowM(x, sort=sort, names=names, ...)
  diag(ret) <- 1
  ret
}

### }}}
### {{{ Gamete flow matrix - M

###------------------------------------------------------------------------

gameteFlowM <- function(x, sort=TRUE, names=TRUE, ...)
{
  ## --- Setup ---

  ascendant <- getAscendantName(x)
  if(length(ascendant) > 2)
    stop("no method for pedigree with more than two ascendants")
  if(sort) idOrig <- as.character(getId(x))  # for sort-back

  ## Pedigree must be extended and sorted FIXME?
  x <- checkAttributes(x, extended=TRUE, sorted=TRUE)

  n <- nrow(x)
  ret <- matrix(0, n, n)         # n*n matrix
  rownames(ret) <- colnames(ret) <- getId(x)

  asc1 <- as.character(getFather(x))
  asc2 <- as.character(getMother(x))
  testAsc1 <- !isUnknown(asc1)
  testAsc2 <- !isUnknown(asc2)
  ## individuals with at least one ascendant known
  set <- which(!(!testAsc1 & !testAsc2))

  ## --- Core ---

  for(i in set) {                # loop over set
    if(testAsc1[i]) ret[i, asc1[i]] <- 0.5
    if(testAsc2[i]) ret[i, asc2[i]] <- 0.5
  }

  ## --- End ---

  if(sort) ret <- ret[idOrig, idOrig]
  if(!names) attr(ret, which="dimnames") <- NULL
  ret
}

### }}}
### {{{ Mendelian sampling covariance matrix - D

###------------------------------------------------------------------------

mendelianSamplingD <- function(x, matrix=TRUE, names=TRUE, ...)
{
  ## --- Setup ---

  ascendant <- getAscendantName(x)
  if(length(ascendant) > 2)
    stop("no method for pedigree with more than two ascendants")

  n <- nrow(x)
  ret <- vector(mode="numeric", length=n)    # n vector ~ diagonal matrix
  ret[] <- 1                                 # has maximally 1 (unknown parents)

  sub <- as.character(getId(x))
  asc1 <- as.character(getFather(x))
  asc2 <- as.character(getMother(x))
  testAsc1 <- !isUnknown(asc1)
  testAsc2 <- !isUnknown(asc2)

  ## --- Core ---

  inbF <- inbreeding(x=x)
  ## Known ascendant1
  ret[testAsc1] <- ret[testAsc1] - 0.25 * (1 + inbF[asc1[testAsc1]])
  ## Known ascendant2
  ret[testAsc2] <- ret[testAsc2] - 0.25 * (1 + inbF[asc2[testAsc2]])

  ## --- End ---

  if(matrix) {
    tmp <- matrix(0, nrow=n, ncol=n)
    diag(tmp) <- ret
    ret <- tmp
  }
  if(names & matrix) rownames(ret) <- colnames(ret) <- sub
  if(names & !matrix) names(ret) <- sub
  ret
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### TDT.R ends here
