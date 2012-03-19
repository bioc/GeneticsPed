### founder.R
###------------------------------------------------------------------------
### What: Founder and non-founder individuals in the pedigree
### $Id: founder.R 1171 2007-04-03 14:05:17Z ggorjan $
### Time-stamp: <2007-04-03 16:04:50 ggorjan>
###------------------------------------------------------------------------

## FIXME: add option to specify what kind of founder i.e. one, two, ...,
## unknown ascendants.
## FIXME: do we need checkAttributes here --> not with S4

isFounder <- function(x, col=attr(x, ".ascendant"))
{
  x <- GeneticsPed:::checkAttributes(x)
  if((l <- length(col)) > 1) {
    return(rowSums(isUnknown(x[, col])) == l)
  } else {
    return(as.vector(isUnknown(x[[col]])) == 1)
  }
}

###------------------------------------------------------------------------
### founder.R ends here
