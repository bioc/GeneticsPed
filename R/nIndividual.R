### nIndividual.R
###------------------------------------------------------------------------
### What: Number of individuals in a Pedigree
### $Id: nIndividual.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-10-15 13:52:12 ggorjan>
###------------------------------------------------------------------------

nIndividual <- function(x, col=NULL, extend=TRUE, drop=TRUE)
{
  x <- GeneticsPed:::checkAttributes(x)
  if(is.null(col)) col <- attr(x, ".subject")
  subject <- attr(x, ".subject")
  if(col == subject & extend) x <- extend(x)
  if(is.factor(x[[col]])) {
    if(is.list(GeneticsPed:::unusedLevels(x))) {
      if(drop) {
        warning("unused levels in 'col' were dropped")
        x <- GeneticsPed:::dropLevels(x)
      } else {
        warning("there are unused levels in 'col'")
      }
    }
    return(nlevels(x[[col]]))
  } else {
    if(col == subject) { ## subjects are all known
      return(length(x[[col]]))
    } else { ## ascendants might be unknown
      tmp <- unique(x[[col]])
      return(length(tmp[!is.na(tmp)]))
    }
  }
}

dropLevels <- function(x)
{
  x <- GeneticsPed:::checkAttributes(x)
  col <- c(attr(x, ".subject"), attr(x, ".ascendant"))
  x[, col] <- lapply(x[, col], factor)
  x
}

###------------------------------------------------------------------------
### nIndividual.R ends here
