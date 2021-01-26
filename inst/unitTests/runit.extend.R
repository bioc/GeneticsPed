### runit.extend.R
###------------------------------------------------------------------------
### What: Unit tests for "unknown" methods
### $Id: runit.extend.R 1174 2007-04-03 14:07:11Z ggorjan $
### Time-stamp: <2007-04-03 14:46:30 ggorjan>
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
x2.1TestAgainst <- x2.1

data(Mrode3.1)
x3.1 <- Pedigree(x=Mrode3.1, subject="calf", ascendant=c("sire", "dam"),
                 ascendantSex=c("Male", "Female"), sex="sex")
x3.1TestAgainst <- x3.1

### }}}
### {{{ --- extend ---

test.extend <- function() {
  ## Removing founders and extending
  x2.1 <- x2.1[!isFounder(x2.1), ]
  x2.1Test <- extend(x2.1)

  ## Prepairing testAgainst object
  ## FIXME: family() does not work at the moment
  x2.1TestAgainst$fam <- NULL
  x2.1Test$fam <- NULL
  ## FIXME: there is no method for "getting" approx. date of birth
  x2.1TestAgainst[isFounder(x2.1TestAgainst), "dtB"] <- NA
  ## Extending does not care the order or individuals and factor levels
  for(i in seq(along=names(x2.1Test))) {
    if(is.factor(x2.1Test[[i]])) {
      x2.1Test[[i]] <- as.character(x2.1Test[[i]])
      x2.1TestAgainst[[i]] <- as.character(x2.1TestAgainst[[i]])
    }
  }
  x2.1TestAgainst <- x2.1TestAgainst[order(x2.1TestAgainst$sub), ]
  x2.1Test <- x2.1Test[order(x2.1Test$sub), ]
  attr(x2.1TestAgainst, ".extended") <- TRUE
  row.names(x2.1TestAgainst) <- NULL
  row.names(x2.1Test) <- NULL
  checkEquals(x2.1Test, x2.1TestAgainst)

  ## x3.1 test about extension of additional data
  x3.1Test <- extend(x3.1)$pwg
  x3.1TestAgainst <- c(NA, NA, NA, x3.1$pwg)
  checkEquals(x3.1Test, x3.1TestAgainst)

  ## x3.1 test about top
  x3.1Test <- extend(x3.1, top=FALSE)$pwg
  x3.1TestAgainst <- c(x3.1$pwg, NA, NA, NA)
  checkEquals(x3.1Test, x3.1TestAgainst)
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.extend.R ends here
