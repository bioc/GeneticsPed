### runit.pedigree.R
###------------------------------------------------------------------------
### What: Unit tests for "unknown" methods
### $Id: runit.pedigree.R 1174 2007-04-03 14:07:11Z ggorjan $
### Time-stamp: <2007-04-01 22:57:46 ggorjan>
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

data(Mrode3.1)

### }}}
### {{{ --- Pedigree ---

test.Pedigree <- function() {

  ## --- Errors ---

  ## values of 'ascendantSex' must accord with values of 'sex' column
  checkException(Pedigree(x=Mrode3.1, subject="calf",
                          ascendant=c("sire", "dam"),
                          ascendantSex=c(1, 2), sex="sex"))
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.pedigree.R ends here
