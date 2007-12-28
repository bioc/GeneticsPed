### runit.genRelMatrix.R
###------------------------------------------------------------------------
### What: Unit tests for genetic relationship methods
### $Id: runit.genRelMatrix.R 1174 2007-04-03 14:07:11Z ggorjan $
### Time-stamp: <2007-04-03 02:28:36 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("GeneticsPed")
}

### }}}
### {{{ --- Pedigrees ---

data(Mrode2.1)
Mrode2.1$dtB <- as.Date(Mrode2.1$dtB)
x2.1 <- Pedigree(x=Mrode2.1, subject="sub", ascendant=c("fat", "mot"),
                 ascendantSex=c("M", "F"), family="fam", sex="sex",
                 generation="gen", dtBirth="dtB")
n2.1 <- nrow(x2.1)

data(Mrode3.1)
x3.1 <- Pedigree(x=Mrode3.1, subject="calf", ascendant=c("sire", "dam"),
                 ascendantSex=c("Male", "Female"), sex="sex")
n3.1 <- nrow(x3.1)

### }}}
### {{{ --- Additive relationship matrix (A) ---

test.A <- function()
{
  ## Test with result on page 26 of Mrode (2005)
  test <- matrix(c(1,    0,     0.5,    0.5,    0.5,    0.25,
                   0,    1,     0.5,    0,      0.25,   0.625,
                   0.5,  0.5,   1,      0.25,   0.625,  0.5625,
                   0.5,  0,     0.25,   1,      0.625,  0.3125,
                   0.5,  0.25,  0.625,  0.625,  1.125,  0.6875,
                   0.25, 0.625, 0.5625, 0.3125, 0.6875, 1.125),
                 nrow=n2.1, ncol=n2.1)
  checkIdentical(relationshipAdditive(x2.1, names=FALSE), test)

  ## Output of solve(relationshipAdditive(x)) must be ~equal to
  ## inverseAdditive(x)

  x <- generatePedigree(nId=30, nGeneration=6) # highly inbreed pedigree
  test1 <- solve(relationshipAdditive(x))
  test2 <- inverseAdditive(x)
  checkEquals(test1, test2)
}


### }}}
### {{{ --- Inbreeding (F) ---

test.F <- function()
{
  ## Test with result on page 27 of Mrode (2005)
  test <- c(0, 0, 0, 0, 0.125, 0.125)
  checkIdentical(inbreeding(x2.1, names=FALSE), test)
  ## checkIdentical(inbreeding(x2.1, method="sargolzaei", names=FALSE), test)
  checkIdentical(inbreeding(x2.1, method="tabular", names=FALSE), test)
}

### }}}
### {{{ --- Gene and gamete flow matrix (T, Tinv, M) ---

#test.T <- function()
#{
#  ## Test with result on page 28 of Mrode (2005)
#  test <- matrix(c(1,    0,     0,    0,    0,   0,
#                   0,    1,     0,    0,    0,   0,
#                   0.5,  0.5,   1,    0,    0,   0,
#                   0.5,  0,     0,    1,    0,   0,
#                   0.5,  0.25,  0.5,  0.5,  1,   0,
#                   0.25, 0.625, 0.25, 0.25, 0.5, 1),
#                 nrow=n2.1, ncol=n2.1, byrow=TRUE)
#  checkIdentical(geneFlowT(x2.1, names=FALSE), test)
#}
#
#test.Tinv <- function()
#{
#  ## Test with result on page 28 of Mrode (2005)
#  test <- matrix(c(1,     0,    0,    0,    0,   0,
#                   0,     1,    0,    0,    0,   0,
#                   -0.5, -0.5,  1,    0,    0,   0,
#                   -0.5,  0,    0,    1,    0,   0,
#                   0,     0,   -0.5, -0.5,  1,   0,
#                   0,    -0.5,  0,    0,   -0.5, 1),
#                 nrow=n2.1, ncol=n2.1, byrow=TRUE)
#  checkIdentical(geneFlowTinv(x2.1, names=FALSE), test)
#}
#
#test.M <- function()
#{
#  ## Test with result on page 29 of Mrode (2005)
#  test <- matrix(c(0,   0,   0,   0,   0,   0,
#                   0,   0,   0,   0,   0,   0,
#                   0.5, 0.5, 0,   0,   0,   0,
#                   0.5, 0,   0,   0,   0,   0,
#                   0,   0,   0.5, 0.5, 0,   0,
#                   0,   0.5, 0,   0,   0.5, 0),
#                 nrow=n2.1, ncol=n2.1, byrow=TRUE)
#  checkIdentical(gameteFlowM(x2.1, names=FALSE), test)
#}
#
#### }}}
#### {{{ --- Mendelian sampling covariance matrix (D) ---
#
#test.D <- function()
#{
#  ## Test with result on page 28 of Mrode (2005)
#  test <- c(1, 1, 0.5, 0.75, 0.5, 0.46875)
#  test1 <- matrix(0, nrow=n2.1, ncol=n2.1)
#  diag(test1) <- test
#  checkIdentical(mendelianSamplingD(x2.1, matrix=FALSE, names=FALSE), test)
#  checkIdentical(mendelianSamplingD(x2.1, names=FALSE), test1)
#}

### }}}
### {{{ --- Model matrix (Z) ---

test.Z <- function()
{
  test <- matrix(c(0, 0, 0, 1, 0, 0, 0, 0,
                   0, 0, 0, 0, 1, 0, 0, 0,
                   0, 0, 0, 0, 0, 1, 0, 0,
                   0, 0, 0, 0, 0, 0, 1, 0,
                   0, 0, 0, 0, 0, 0, 0, 1), nrow=n3.1, byrow=TRUE)
  Z <- model.matrix(object=x3.1, y=x3.1$pwg, id=x3.1$calf, names=FALSE)
  checkIdentical(test, Z)
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.genRelMatrix.R ends here
