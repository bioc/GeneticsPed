### summary.pedigree.R
###------------------------------------------------------------------------
### What: Summary of Pedigree
### $Id: summary.pedigree.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-10-15 13:51:46 ggorjan>
###------------------------------------------------------------------------

## Summary Pedigree
## base
## nonbase
## dates, ...
## attributes
## checks

summary.Pedigree <- function(object, ...)
{
  col <- c(attr(object, ".subject"), attr(object, ".ascendant"))
  num <- vector(length=length(col))
  for(i in seq(along=col)) num[i] <- nIndividual(object, col=col[i])
  data.frame(subject=col, number=num)
}

###------------------------------------------------------------------------
### summary.pedigree.R ends here
