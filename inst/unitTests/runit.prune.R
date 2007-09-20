### runit.prune.R
###------------------------------------------------------------------------
### What: Prune pedigree - unit tests
### $Id$
### Time-stamp: <2007-08-18 03:49:56 ggorjan>
###------------------------------------------------------------------------

## Interactive test setup
if(FALSE) {
  library("RUnit")
  library("GeneticsPed")
}

test.prune <- function()
{
  x <- data.frame(oseba=c(1,  9, 11, 2, 3, 10, 8, 12, 13,  4, 5, 6, 7, 14, 15, 16, 17),
                    oce=c(2, 10, 12, 5, 5,  0, 7,  0,  0,  0, 7, 0, 0,  0,  0,  0,  0),
                   mama=c(3,  8, 13, 0, 4,  0, 0,  0,  0, 14, 6, 0, 0, 15, 16, 17,  0),
                   spol=c(2,  2,  2, 1, 2,  1, 2,  1,  2,  2, 1, 2, 1,  1,  1,  1,  1),
             generacija=c(1,  1,  1, 2, 2,  2, 2,  2,  2,  3, 3, 4, 4,  5,  6,  7,  8),
                   last=c(2, NA,  8, 4, 1,  6,NA, NA, NA, NA,NA,NA,NA, NA, NA, NA, NA))

  ## Default case
  x2 <- prune(x=x, id="oseba", father="oce", mother="mama",
                      unknown=0)
  checkEquals(x2$oseba, c(1, 9, 2, 3, 8, 5, 7))
  checkEquals(x2$oce,   c(2, 0, 5, 5, 7, 7, 0))
  checkEquals(x2$mama,  c(3, 8, 0, 0, 0, 0, 0))

  ## Use of additional test
  x2 <- prune(x=x, id="oseba", father="oce", mother="mama",
                      unknown=0, testAdd=is.na(x$last))
  checkEquals(x2$oseba, c(1, 9, 11, 2, 3, 10, 8, 5, 7))

  ## Use of other data
  y <- data.frame(oseba=c( 11,  15, 16),
                  last2=c(8.5, 7.5, NA))

  x <- merge(x=x, y=y, all.x=TRUE)
  x2 <- prune(x=x, id="oseba", father="oce", mother="mama",
                      unknown=0, testAdd=is.na(x$last2))
}

###------------------------------------------------------------------------
### runit.prune.R ends here
