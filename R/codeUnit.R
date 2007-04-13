### codeUnit.R
###------------------------------------------------------------------------
### What: Get internal codes for units
### $Id: codeUnit.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-09-30 01:11:07 ggorjan>
###------------------------------------------------------------------------

codeUnit <- function(x, unknown=as.integer(0), factorInfo=FALSE,
                     warnNA=TRUE)
{
  ret <- list()

  ## --- Checks ---
  x <- GeneticsPed:::checkAttributes(x)
  col <- c(attr(x, ".subject"), attr(x, ".ascendant"))
  if(attr(x, ".colClass") %in% c("integer", "numeric")) {
    x <- NAToUnknown(x, unknown=unknown)
    if(warnNA)
      warning("id already have numeric representation, handling only NAs")
    if(factorInfo) ret$factorInfo
    ret$pedigree <- x
    return(ret)
  }

  ## --- Core ---
  map <- unlist(lapply(x[, col], mapFactor, codes=FALSE))
  map <- unique(map)
  names(map) <- map
  map <- as.list(map)
  doIt <- function(x, map, unknown)
  {
    levels(x) <- map
    x <- as.integer(x)
    x[is.na(x)] <- unknown
    return(x)
  }
  x[, col] <- lapply(x[, col], doIt, map, 0)
  attr(x, ".coded") <- TRUE

  ## --- Return ---
  if(factorInfo) {
    map[1:length(map)] <- 1:length(map) # I need codes for mapping back
    ret$factorInfo <- map
  }
  ret$pedigree <- x
  ret
}

if(FALSE) {
  ped <- generatePedigree(5)
  attr(ped, ".colClass") <- "factor"
  ped$id <- factor(letters[ped$id])
  ped$father <- factor(letters[ped$father])
  ped$mother <- factor(letters[ped$mother])
  x <- ped
  codeUnit(x)
}

###------------------------------------------------------------------------
### codeUnit.R ends here
