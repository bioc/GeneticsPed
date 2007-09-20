### prune.R
###------------------------------------------------------------------------
### What: Prune pedigree - code
### $Id$
### Time-stamp: <2007-08-19 17:53:16 ggorjan>
###------------------------------------------------------------------------

### {{{ prune

prune <- function(x, id, father, mother, unknown=NA, testAdd=NULL,
                          verbose=FALSE)
{
  ### {{{ --- Checks & setup ---

  if(!is.data.frame(x)) stop("'x' must be a data.frame")

  if(missing(id) | missing(father) | missing(mother))
    stop("'id', 'father', and 'mother' column names must be given")

  if(!(id %in% names(x)))
    stop("column name specified in 'id' does not exist in 'x'")
  if(!(father %in% names(x)))
    stop("column name specified in 'father' does not exist in 'x'")
  if(!(mother %in% names(x)))
    stop("column name specified in 'mother' does not exist in 'x'")

  ## Duplicates
  if(length(unique(x[, id])) != nrow(x)) {
    tmp <- table(x[, id])
    tmp <- names(tmp[tmp > 1])
    stop(paste("there are duplicated entries for 'id';\ncheck individual(s):",
               paste(tmp, collapse=", ")))
  }

  ## Any ID unknown
  test <- is.na(x[, id]) | x[, id] %in% unknown
  if(any(test)) {
    tmp <- which(test)
    stop(paste("'id' must be known; check row(s):",
               paste(tmp, collapse=", ")))
  }

  ## Set unknown to NA
  setToUnknown <- FALSE
  if(any(!(unknown %in% NA))) {
    x[, c(father, mother)] <- unknownToNA(x=x[, c(father, mother)],
                                          unknown=unknown)
    setToUnknown <- TRUE
  }

  ## Is pedigree extended
  test <- x[, father] %in% x[, id] | is.na(x[, father])
  if(any(!test)) {
    tmp <- x[which(test), father]
    stop(paste("pedigree must be extended; father(s) do not appear as individuals:",
               paste(tmp, collapse=", ")))
  }
  test <- x[, mother] %in% x[, id] | is.na(x[, mother])
  if(any(!test)) {
    tmp <- x[which(test), mother]
    stop(paste("pedigree must be extended; mother(s) do not appear as individuals:",
               paste(tmp, collapse=", ")))
  }

  ## TODO: GeneticsPed use of extend

  ## testAdd
  if(!is.null(testAdd)) {
    if(!is.logical(testAdd))
      stop("'testAdd' must be logical i.e. TRUE/FALSE")
    if(length(testAdd) != nrow(x))
      stop("length of 'testAdd' must equal number of rows in 'x'")
  }

  if(verbose) cat("... all checks and setup passed\n")

  ### }}}
  ### {{{ --- Real work ---

  ### {{{ pruneAny

  pruneAny <- function()
  {
    ## Founders i.e. unknown father and mother
    testF <- is.na(x[, father])
    testM <- is.na(x[, mother])
    testFM <- testF & testM
    ## x[testFM, ] # these are potential candidates for removal, but
    ##             # only if they have less than 2 or no descendants

    ## Number of first descendants
    tabF <- table(x[, father])
    tabM <- table(x[, mother])
    testNF <- tabF > 1
    testNM <- tabM > 1
    testNFM <- !(x[, id] %in% c(names(tabF[testNF]), names(tabM[testNM])))

    ## x[testFM, ]           # founders
    ## x[testFM & testNFM, ] # founders with less than 2 or no descendants

    ## Additional test
    ## x[testFM & testNFM & testAdd, ] # founders, < 2, and additional test

    ## Return
    if(is.null(testAdd)) {
      testFM & testNFM
      ## test <- testFM & testNFM
    } else {
      testFM & testNFM & testAdd
      ## test <- testFM & testNFM & testAdd
    }
  }

  ### }}}
  ### {{{ loop

  iter <- 1
  cont <- TRUE
  while(cont) {
    test <- pruneAny()
    if(any(test)) {
      ## Remove individual(s)
      ind <- x[test, id]
      x <- x[!test, ]
      if(!is.null(testAdd)) testAdd <- testAdd[!test]
      ## Set to unknown where these individuals appeared as father/mother
      x[x[, father] %in% ind, father] <- NA
      x[x[, mother] %in% ind, mother] <- NA
      ## Report
      if(verbose) {
        cat(paste("... ", iter, ": removed ", length(ind), " individual(s)\n", sep=""))
        cat(ind, "\n")
      }
      iter <- iter + 1
      cont <- TRUE
    } else {
      cont <- FALSE
    }
  }

  ### }}}

  ### }}}
  ### {{{ --- Return ---

  ## Set NA back to unknown
  if(setToUnknown) {
    x[, c(father, mother)] <- NAToUnknown(x=x[, c(father, mother)],
                                          unknown=unknown[1])
    if(length(unknown) > 1)
      cat(paste("Beware: unknown identification set to", unknown[1], "\n"))
  }

  x
  ### }}}
}

### }}}
### {{{ Dear Emacs
### Local variables:
### folded-file: t
### end:
### }}}

###------------------------------------------------------------------------
### prune.R ends here
