### geneContribution.R
###------------------------------------------------------------------------
### What: Gene contribution or proportion of genes in pedigree by individual
### $Id: geneContribution 895 2006-06-16 14:28:51Z dnadave $
### Time-stamp: <2006-10-01 02:44:37 ggorjan>
###------------------------------------------------------------------------

geneContribution <- function(x, relative=TRUE)
{
  ## --- Setup ---

  subject <- attr(x, ".subject")
  ascendant <- attr(x, ".ascendant")
  idInput <- as.character(x[[subject]]) ## for sort-back

  ## Pedigree must be sorted, extended and unknowns must be NA
  x <- GeneticsPed:::checkAttributes(x, sorted=TRUE, extended=TRUE,
                                     unknownNA=TRUE)
  z <- rep(1, times=nrow(x))
  names(z) <- x[[subject]]

  ascSex <- attr(x, ".ascendantSex")
  ascProb <- table(ascSex)
  multi <- ascProb[ascProb > 1]
  cont <- rep(0.5, times=length(ascSex))
  names(cont) <- ascSex

  ## --- The machine ---

  for(i in rev(seq(along=x[[subject]]))) {
    k <- which(x[[subject]] %in% x[i, ascendant])
    if(length(k) > 0) {
      cont1 <- cont[!is.na(x[i, ], col=ascendant)]
      if(length(cont1) > 2) {
        test <- names(cont1) %in% names(multi)
        cont1[test] <- cont1[test] / multi
      }
      z[k] <- z[k] + cont1 * z[i]
    }
  }

  z <- z - 1
  if(relative)
    z <- z / nrow(x)

  ## --- End ---

  z[idInput]
}

###------------------------------------------------------------------------
### geneContribution.R ends here
